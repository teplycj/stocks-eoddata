source('eod_helper_functions.R')

# set starting balance ----------------------------------------------------

BANK <- 50
sma_window <- 7L
num_simultaneous_stocks <- 10

# load in nyse stocks -----------------------------------------------------

nyse <- data.table::fread('~/local_stocks/NYSE/one_year_stocks.csv') |>
  dplyr::arrange(symbolCode, date)

# helper functions --------------------------------------------------------

get_valid_date <- function(date_val, available_dates) {
  if (date_val > max(available_dates)) {
    stop(paste('Date out of range:', date_val))
  }
  while (!date_val %in% available_dates) {
    date_val <- date_val + 1
  }
  return(date_val)
}

get_one_dollar_floor <- function(prop_to_buy, money_to_spend) {
  purchase_vec <- prop_to_buy * money_to_spend
  below_one_vec <- which(purchase_vec < 1)
  for (x in below_one_vec) {
    diff_to_makeup <- 1 - purchase_vec[x]
    max_purchase_index <- which.max(purchase_vec)
    purchase_vec[x] <- 1
    purchase_vec[max_purchase_index] <- purchase_vec[max_purchase_index] - diff_to_makeup
  }
  return(purchase_vec)
}

calculate_sma <- function(stock_dat, start_date, end_date, avg_window, col_to_use, get_pct_change = FALSE) {
  stock_dat <- stock_dat |>
    dplyr::filter(
      date >= as.Date(start_date) & date <= as.Date(end_date)
    )
  stock_names <- unique(stock_dat$symbolCode)
  lapply(stock_names, \(x) {
    dat <- stock_dat |>
      dplyr::filter(symbolCode == x) |>
      dplyr::mutate(
        sma_price = data.table::frollmean(
          x = get(col_to_use),
          n = avg_window,
          fill = NA_real_,
          algo = 'exact'
        ),
        sma_volume = data.table::frollmean(
          x = volume,
          n = avg_window,
          fill = NA_real_,
          algo = 'exact'
        )
      )
    if (get_pct_change) {
      dat <- calculate_pct_change(
        dat = dat,
        col_to_use = col_to_use,
        new_col = 'pct_change_price'
      ) |>
        calculate_pct_change(
          col_to_use = 'volume',
          new_col = 'pct_change_volume'
        ) 
    }
    return(dat)
  }) |>
    data.table::rbindlist()
}

get_linear_trends <- function(stock_dat, col_to_use, start_date = NULL, end_date = NULL, use_pct_change) {
  if (!is.null(start_date) && !is.null(end_date)) {
    stock_dat <- stock_dat |>
      dplyr::filter(
        date >= as.Date(start_date) & date <= as.Date(end_date)
      )
  }
  stock_names <- unique(stock_dat$symbolCode)
  lapply(stock_names, \(x) {
    x_dat <- stock_dat |> dplyr::filter(symbolCode == x)
    na_val_count <- length(which(is.na(x_dat[[col_to_use]])))
    prop_na <- na_val_count / nrow(x_dat)
    if (prop_na > 0.5) {
      return(data.frame(
        symbolCode = x,
        category = unique(x_dat$Category)
      ))
    }
    linear_fit <- lm(
      formula = as.formula(paste0(col_to_use, ' ~ date')),
      data = x_dat,
      na.action = na.exclude
    )
    slope <- coef(linear_fit)[2]
    dat <- data.frame(
      symbolCode = x,
      category = unique(x_dat$Category),
      trend_line = slope,
      mean_vol = mean(x_dat$volume, na.rm = TRUE),
      median_price = median(x_dat[[col_to_use]], na.rm = TRUE),
      median_close = median(x_dat$close, na.rm = TRUE)
    )
    if (use_pct_change) {
      dat$last_pct_change <- tail(x_dat$pct_change, n = 1)
      dat$momentum <- sign(dat$trend_line) + sign(dat$last_pct_change)
    }
    return(dat)
  }) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE) |>
    dplyr::arrange(-momentum, -trend_line)
}

calculate_pct_change <- function(dat, col_to_use, new_col) {
  i_vec <- which(!is.na(dat[[col_to_use]]))
  dat <- dat |> dplyr::slice(i_vec)
  dat[[new_col]] <- 0
  if (nrow(dat) < 2) return(dat)
  for (r in 2:nrow(dat)) {
    prev_row <- r - 1
    dat[[new_col]][r] <- (dat[[col_to_use]][r] - dat[[col_to_use]][prev_row]) / 
      dat[[col_to_use]][prev_row]
  }
  return(dat)
}

get_ratios_to_buy <- function(stock_dat, buy_date, stocks_to_buy, money_to_spend) {
  stock_dat |>
    dplyr::filter(date == buy_date & symbolCode %in% stocks_to_buy) |>
    dplyr::mutate(
      stock_power = close * volume, # TODO --> figure out a new way to weight stocks being bought
      sum_power = sum(stock_power),
      prop_to_buy = stock_power / sum_power,
      amount_purchased = get_one_dollar_floor(prop_to_buy, money_to_spend),
      pct_purchased = amount_purchased / close
    ) |>
    dplyr::select(
      symbolCode, date, close, prop_to_buy, amount_purchased, pct_purchased
    )
}

assess_purchased_stocks <- function(smoothed_stocks, stock_ledger) {
  purchased_stock_dat <- smoothed_stocks |> 
    dplyr::filter(symbolCode %in% stock_ledger$symbolCode)
  
}

# buy stocks based on sma -------------------------------------------------

nyse_smoothed <- calculate_sma(
  stock_dat = nyse,
  start_date = min(nyse$date),
  end_date = max(nyse$date),
  avg_window = sma_window,
  col_to_use = 'open',
  get_pct_change = TRUE
) 

# 1) get initial trends

start_date <- min(nyse$date)
end_search_date <- start_date + 14

initial_trends <- calculate_sma(
  stock_dat = nyse,
  start_date = start_date,
  end_date = end_search_date,
  avg_window = sma_window,
  col_to_use = 'open',
  get_pct_change = TRUE
) 
  get_linear_trends(col_to_use = 'sma', use_pct_change = TRUE)

# 2) buy initial stocks based on initial trends 
stock_ledger <- get_ratios_to_buy(
  stock_dat = nyse,
  buy_date = end_search_date,
  stocks_to_buy = initial_trends |> 
    dplyr::slice(seq_len(num_simultaneous_stocks)) |>
    purrr::chuck('symbolCode'),
  money_to_spend = BANK
)

# 3) iterate through days and trade
end_iter_date <- max(nyse$date)
x_date <- end_search_date + 1
while(x_date <= end_iter_date) {
  x_date <- get_valid_date(x_date, unique(nyse$date))
  smoothed_stocks <- calculate_sma(
    stock_dat = nyse,
    start_date = start_date,
    end_date = x_date,
    avg_window = sma_window,
    col_to_use = 'close',
    get_pct_change = TRUE
  )
  
  x_date <- x_date + 1
}