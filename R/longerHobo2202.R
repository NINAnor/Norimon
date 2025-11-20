#' longerHobo2202
#' Convenience function to read in a raw file of hobo logger MX2202 data and rearrange the columns to make is usable
#'
#' @param inputFile export file of raw logger data from HoboLink.com.
#' @param guess_max optional number of rows to guess the data format from. May have to increase a lot.
#' @param delim Character string of delimiter in source data. Defaults to ";".
#' @param date_format Character vector of date format. Be aware of the day, month, year ordering, and any trailing %z for timezone info.
#' @param single_logger Are you processing data from a single logger or a bulk download (with different column structure)
#' @param ... additional parameters passed to read_csv
#' @return A tibble of well formatted logger data from the MX2301A type logger
#' @export
#'
#' @importFrom rlang .data is_character
#' @examples
#' \dontrun{
#'
#' mx2202_data <- longerHobo2202(inputFile = "../rawData/temp_logger/
#' NasIns_2021_all_MX2202_2021_11_15_10_58_26_CET_1.csv")
#' }
#'
#'

longerHobo2202 <- function(inputFile,
                           guess_max = 10000,
                           delim = ";",
                           date_format = "%y/%m/%d %H:%M:%S %z",
                           single_logger = TRUE,
                           ...) {

  rawDat <- readr::read_delim(inputFile,
                              guess_max = guess_max,
                              col_types = readr::cols(),
                              delim = delim,
                              ...
  )

  suppressWarnings({
    dat <- rawDat %>%
      dplyr::select(-dplyr::matches(c("Line#", "#", "Button Down", "Started", "Host Connected", "End of File"))) %>%
      dplyr::mutate(dplyr::across(
        .cols = dplyr::matches("Date"),
        .fns = ~ as.POSIXct(.x, format = date_format),
        .names = "date"
      )) %>%
      dplyr::mutate_if(rlang::is_character, as.double) %>%
      dplyr::select(-dplyr::matches("Date", ignore.case = FALSE))
  })


  if(single_logger){
    dat <- dat |>
      mutate(logger_id = sub("^(\\d+)\\s.*$", "\\1", basename(inputFile))) |>
      mutate(logger_type = "MX2202")

    out <- dat |>
      select(logger_time = date,
             logger_type,
             logger_id,
             temperature = matches("Temperature"),
             light = matches("Light"))

  } else {

    temp <- dat %>%
      tidyr::pivot_longer(
        cols = tidyr::starts_with("Temperature"),
        names_to = "logger_id",
        values_to = "temperature"
      ) %>%
      select(
        date,
        logger_id,
        temperature
      ) %>%
      filter(!is.na(temperature))

    light <- dat %>%
      tidyr::pivot_longer(
        cols = tidyr::starts_with("Light"),
        names_to = "logger_id",
        values_to = "light"
      ) %>%
      select(
        date,
        logger_id,
        light
      ) %>%
      filter(!is.na(.data$light))

    temp <- temp %>%
      mutate(logger_id = stringr::str_extract(
        .data$logger_id,
        "[^, ]+$"
      ))

    light <- light %>%
      mutate(logger_id = stringr::str_extract(
        .data$logger_id,
        "[^, ]+$"
      ))

    # if(!all(temp$date == light$date)) stop("Tables datetimes doesn't match")

    out <- temp %>%
      inner_join(light,
                 by = c(
                   "date" = "date",
                   "logger_id" = "logger_id"
                 )
      ) %>%
      arrange(
        .data$logger_id,
        .data$date
      ) %>%
      mutate(logger_type = "MX2202") %>%
      select(logger_time = date,
             logger_type,
             logger_id,
             temperature,
             light
      )
  }

  return(out)
}
