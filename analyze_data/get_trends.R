library(ggplot2)
library(gridExtra)

source('~/stocks-eoddata/eod_helper_functions.R')

# load in data ------------------------------------------------------------

exchange_name <- 'NYSE'
stock_dat <- data.table::fread(
  file = file.path('~/local_stocks/', exchange_name, '/one_year_stocks.csv')
)

# get unique stocks -------------------------------------------------------

stock_names <- unique(stock_dat$symbolCode)

# get trends by stock -----------------------------------------------------

get_initial_trends <- function(stock_dat, start_date = NULL, end_date = NULL) {
  if (!is.null(start_date) && !is.null(end_date)) {
    stock_dat <- stock_dat |>
      dplyr::filter(
        date >= as.Date(start_date) & date <= as.Date(end_date)
      )
  }
  stock_names <- unique(stock_dat$symbolCode)
  lapply(stock_names, \(x) {
    x_dat <- stock_dat |> dplyr::filter(symbolCode == x)
    linear_fit <- lm(
      formula = close ~ date,
      data = x_dat
    )
    slope <- coef(linear_fit)[2]
    data.frame(
      symbolCode = x,
      category = unique(x_dat$Category),
      trend_line = slope,
      mean_vol = mean(x_dat$volume),
      median_price = median(x_dat$open)
    )
  }) |>
    data.table::rbindlist() |>
    dplyr::arrange(-trend_line)
}

trend_dat <- get_initial_trends(stock_dat)

# get stock info ----------------------------------------------------------

# don't have the right membership to retrieve this information...
# stock_fundamental_info <- get_eod_data(
#   url_ext = paste0('Fundamental/List/', exchange_name)
# )

# plot positive trends ----------------------------------------------------

create_plot <- function(stock_name, keep_legend, sma_window = 3L) {
  plot_dat <- stock_dat |> 
    dplyr::filter(symbolCode == stock_name) |>
    dplyr::select(date, close, high, low) |>
    data.table::setDT() |>
    dplyr::mutate(
      sma_open = data.table::frollmean(
        x = close,
        n = sma_window,
        fill = NA_real_,
        algo = 'exact'
      )
    ) |>
    data.table::melt(id.vars = 'date') |>
    data.table::setnames(old = 'variable', new = 'Time Stamp')
  current_trend_dat <- trend_dat |> 
    dplyr::filter(symbolCode == stock_name)
  plot_subtitle <- paste0(
    'Trend = ', round(current_trend_dat$trend_line, digits = 3), '\n',
    'High Price = $', round(max(plot_dat$value, na.rm = TRUE), digits = 2L), '\n',
    'Low Price = $', round(min(plot_dat$value, na.rm = TRUE), digits = 2L)
  )
  plot_title <-  paste(
    exchange_name, 
    stock_name, 
    current_trend_dat$category, 
    sep = ' - '
  )
  ggplot() +
    geom_point(
      data = plot_dat,
      mapping = aes(x = date, y = value, colour = `Time Stamp`),
      alpha = 0.6, na.rm = TRUE
    ) +
    geom_line(
      data = plot_dat |> dplyr::filter(`Time Stamp` != 'sma_open'),
      mapping = aes(x = date, y = value, colour = `Time Stamp`),
      alpha = 0.6, linetype = 'dashed', na.rm = TRUE
    ) +
    geom_line(
      data = plot_dat |> dplyr::filter(`Time Stamp` == 'sma_open'),
      mapping = aes(x = date, y = value, colour = `Time Stamp`),
      alpha = 1, linetype = 'solid', linewidth = 3L, na.rm = TRUE
    ) +
    geom_smooth(
      data = plot_dat |> dplyr::filter(`Time Stamp` != 'sma_open'),
      mapping = aes(x = date, y = value, colour = `Time Stamp`),
      method = 'loess', se = FALSE, na.rm = TRUE
    ) +
    scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
    scale_color_manual(
      values = c(
        'close' = '#3bceac',
        'high' = '#ffd23f',
        'low' = '#ee4266',
        'sma_open' = '#540d6e'
      )
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = 'Date',
      y = 'Stock Price ($)'
    ) +
    theme_classic() +
    theme(legend.position = if (keep_legend) 'right' else 'none')
}

pdf(
  file = file.path('~/local_stocks/', exchange_name, 'trend_outputs.pdf'),
  height = 8,
  width = 16
)

plot_stocks <- unique(trend_dat$symbolCode)
plot_counter <- 0L
plots_per_page <- 8L
plot_list <- list()
for (x in plot_stocks) {
  plot_counter <- plot_counter + 1L
  stock_plot <- create_plot(
    stock_name = x,
    keep_legend = if (plot_counter %% (plots_per_page / 2) == 0) TRUE else FALSE,
    sma_window = 10L
  )
  plot_list <- append(plot_list, list(stock_plot))
  if (plot_counter %% plots_per_page == 0) {
    do.call("grid.arrange", c(plot_list, ncol = plots_per_page / 2))
    plot_list <- list()
    plot_counter <- 0
  }
}

dev.off()

data.table::fwrite(
  x = positive_trends,
  file = file.path('~/local_stocks/', exchange_name, 'positive_trends.csv')
)

