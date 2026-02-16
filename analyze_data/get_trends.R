library(ggplot2)
library(gridExtra)

source('~/stocks-eoddata/eod_helper_functions.R')

# load in data ------------------------------------------------------------

exchange_name <- 'NYSE'
stock_dat <- data.table::fread(
  file = file.path('~/local_stocks/', exchange_name, '/stock_data.csv')
)
category_dat <- data.table::fread(
  file = file.path('~/local_stocks/NYSE/nyse_categorized_stocks.csv')
)

stock_dat <-  data.table::merge.data.table(
  x = stock_dat,
  y = category_dat,
  by.x = 'symbolCode',
  by.y = 'ACT Symbol',
  all.x = TRUE
)

# get unique stocks -------------------------------------------------------

stock_names <- unique(stock_dat$symbolCode)

# get trends by stock -----------------------------------------------------

trend_dat <- lapply(stock_names, \(x) {
  x_dat <- stock_dat |> dplyr::filter(symbolCode == x)
  linear_fit <- lm(
    formula = open ~ dateStamp,
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

positive_trends <- trend_dat |> 
  dplyr::filter(trend_line > 0.25 & median_price < 1000)

# get stock info ----------------------------------------------------------

# don't have the right membership to retrieve this information...
# stock_fundamental_info <- get_eod_data(
#   url_ext = paste0('Fundamental/List/', exchange_name)
# )

# plot positive trends ----------------------------------------------------

create_plot <- function(stock_name, keep_legend, sma_window = 3L) {
  plot_dat <- stock_dat |> 
    dplyr::filter(symbolCode == stock_name) |>
    dplyr::select(dateStamp, open, high, low) |>
    data.table::setDT() |>
    dplyr::mutate(
      sma_open = data.table::frollmean(
        x = open,
        n = sma_window,
        fill = NA_real_,
        algo = 'exact'
      )
    ) |>
    data.table::melt(id.vars = 'dateStamp') |>
    data.table::setnames(old = 'variable', new = 'Time Stamp')
  current_trend_dat <- positive_trends |> 
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
      mapping = aes(x = dateStamp, y = value, colour = `Time Stamp`),
      alpha = 0.6, na.rm = TRUE
    ) +
    geom_line(
      data = plot_dat |> dplyr::filter(`Time Stamp` != 'sma_open'),
      mapping = aes(x = dateStamp, y = value, colour = `Time Stamp`),
      alpha = 0.6, linetype = 'dashed', na.rm = TRUE
    ) +
    geom_line(
      data = plot_dat |> dplyr::filter(`Time Stamp` == 'sma_open'),
      mapping = aes(x = dateStamp, y = value, colour = `Time Stamp`),
      alpha = 1, linetype = 'solid', linewidth = 3L, na.rm = TRUE
    ) +
    geom_smooth(
      data = plot_dat |> dplyr::filter(`Time Stamp` != 'sma_open'),
      mapping = aes(x = dateStamp, y = value, colour = `Time Stamp`),
      method = 'loess', se = FALSE, na.rm = TRUE
    ) +
    scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
    scale_color_manual(
      values = c(
        'open' = '#3bceac',
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

plot_stocks <- unique(positive_trends$symbolCode)
plot_counter <- 0L
plots_per_page <- 8L
plot_list <- list()
for (x in plot_stocks) {
  plot_counter <- plot_counter + 1L
  stock_plot <- create_plot(
    stock_name = x,
    keep_legend = if (plot_counter %% (plots_per_page / 2) == 0) TRUE else FALSE
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

