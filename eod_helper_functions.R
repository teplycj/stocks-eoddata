build_httr_data <- function(httr_content) {
  lapply(httr_content, as.data.frame) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE)
}

get_eod_data <- function(url_ext, time_stamp = NULL) {
  query_url <- paste0(
    Sys.getenv('EOD_URL'), url_ext, '?apiKey=', Sys.getenv('EOD_API_KEY')
  )
  if (!is.null(time_stamp)) {
    query_url <- paste0(query_url, '&dateStamp=', as.character(time_stamp))
  }
  response_val <- httr::GET(
    url = query_url, 
    httr::content_type("application/octet-stream")
  )
  tryCatch(
    {
      httr::stop_for_status(response_val)
      httr::content(response_val) |>
        build_httr_data()
    },
    error = function(cond) {
      message('No data for given URL. skipping...')
      message(cond)
      data.frame()
    }
  )
}