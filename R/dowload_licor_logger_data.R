#' get_licor_logger_data
#' Downloads logger data from the licor.cloud platform
#'
#' @param loggers String of logger id's to fetch data for
#' @param start_date Start date of data (YYYY-MM-DD format)
#' @param end_date End date of data (YYYY-MM-DD format)
#' @param token Autorization token (long string, created at https://licor.cloud)
#'
#' @return A tibble of logger data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' logger_data <- download_licor_logger_data(loggers = 20878881,
#'                                           start_date = "2024-06-01",
#'                                           end_date = "2024-06-30",
#'                                           token = "w4cUSIqspTyvIqwYS5rqFHJTfCj25YBMl8BvyjoEQ9bGlAGF")
#'
#'
#' }
#'

get_licor_logger_data <- function (loggers = NULL,
                                        start_date = NULL,
                                        end_date = NULL,
                                        token = NULL) {

  url="https://api.hobolink.licor.cloud"
  indicator_path <- "/v1/data?loggers="
  start_date_time <- paste0("&start_date_time=", start_date, "%2000%3A00%3A00")
  end_date_time <- paste0("&end_date_time=", end_date, "%2000%3A00%3A00")
  loggers_to_paste <- paste0(loggers, collapse = ",")

  combinedURL <- paste0(url, indicator_path, loggers_to_paste, start_date_time, end_date_time)
  auth_string <- paste("Bearer", token, sep = " ")

  myOrg <- httr::GET(url = combinedURL,
                     encode = "json",
                     httr::add_headers(Authorization = auth_string))

  rawContent <- httr::content(myOrg, as = "text")
  formatted_cont <- jsonlite::fromJSON(rawContent)

  out <- tidyr::as_tibble(formatted_cont$data) |>
    dplyr::mutate(data_type = dplyr::recode(sensor_measurement_type,
                                            "Temperature" = "temperature",
                                            "Light Intensity" = "light",
                                            "RH" = "rh",
                                            "Dew Point" = "dew_point")) |>
    dplyr::select(logger_time = timestamp,
                  logger_id = logger_sn,
                  data_type,
                  value)

  return(out)
}


