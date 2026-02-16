
source('~/stocks-eoddata/eod_helper_functions.R')

# set constants -----------------------------------------------------------

BUFFER_TIME <- 10L # one call every ~10 seconds (given they only allow ~10 calls/minute for free version)
WEEKEND_DAYS <- c('Saturday', 'Sunday')
OUTPUT_DIR <- '~/local_stocks/'
OUTPUT_FILE_NAME <- 'stock_data.csv'

# helper functions --------------------------------------------------------

download_stock_data <- function(exchange_vec, date_vec, buffer_time_constraint = FALSE) {
  ex_grid <- expand.grid(
    exchange = exchange_vec,
    date = date_vec,
    stringsAsFactors = FALSE
  )
  lapply(seq_len(nrow(ex_grid)), \(r) {
    message(paste('getting:', ex_grid$exchange[r], ex_grid$date[r]))
    if (weekdays(as.Date(ex_grid$date[r])) %in% WEEKEND_DAYS) {
      message('skipping weekend day...')
      return(data.frame())
    }
    url_ext <- paste0('Quote/List/', ex_grid$exchange[r])
    dat <- get_eod_data(url_ext = url_ext, time_stamp = ex_grid$date[r])
    if (buffer_time_constraint) Sys.sleep(BUFFER_TIME)
    return(dat)
  }) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE)
}

save_out_stock_data <- function(stock_dat) {
  exchange_vals <- unique(stock_dat$exchangeCode)
  for(ex_val in exchange_vals) {
    ex_out_dir <- file.path(OUTPUT_DIR, ex_val)
    ex_dat <- stock_dat |> dplyr::filter(exchangeCode == ex_val)
    if (dir.exists(ex_out_dir)) {
      saved_dat <- data.table::fread(
        file = file.path(ex_out_dir, OUTPUT_FILE_NAME)
      )
      new_data <- rbind(saved_dat, ex_dat)
      data.table::fwrite(
        x = new_data,
        file = file.path(ex_out_dir, OUTPUT_FILE_NAME)
      )
    } else {
      dir.create(path = ex_out_dir, recursive = TRUE)
      data.table::fwrite(
        x = ex_dat,
        file = file.path(ex_out_dir, OUTPUT_FILE_NAME)
      )
    }
  }
}

# get exchanges -----------------------------------------------------------

#exchange_names <- get_eod_data(url_ext = 'Exchange/List')

free_exchange_vals <- c(
  'AMEX', 'NASDAQ', 'NYSE', 'OTCBB', 'INDEX', 'FOREX', 'CC'
)

# download stock data -----------------------------------------------------

start_date <- Sys.Date() - 27
end_date <- Sys.Date()
dates_to_get <- as.character(as.Date(start_date:end_date))

stock_dat <- download_stock_data(
  exchange_vec = free_exchange_vals[1:3],
  date_vec = dates_to_get,
  buffer_time_constraint = TRUE
)

# save out stock data -----------------------------------------------------

save_out_stock_data(stock_dat = stock_dat)
