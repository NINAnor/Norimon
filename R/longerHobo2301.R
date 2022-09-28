#' longerHobo2301
#'
#' Convenience function to read in a raw file of hobo logger MX2301A data and rearrange the columns to make is usable
#'
#' @param inputFile export file of raw logger data from HoboLink.com
#' @param guess_max optional number of rows to guess the data format from. May have to increase a lot
#' @param ... additional arguments passed to read_csv
#'
#' @return A tibble of well formatted logger data from the MX2301A type logger
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mx2301A_data <- longerHobo2301(inputFile = "../rawData/temp_logger/NasIns_2021_all_MX2301A_2021_11_15_11_00_05_CET_1.csv")
#'
#' }
#'
longerHobo2301 <- function(inputFile,
                           guess_max = 10000,
                           ...){

  rawDat <- read_csv(inputFile,
                     col_types = cols(.default = "c"),
                     guess_max = guess_max,
                     ...)

  dat <- rawDat %>%
    select(-"Line#") %>%
    mutate(date = as.POSIXct(Date, format = "%m/%d/%y %H:%M:%S")) %>%
    mutate_if(is_character, as.double) %>%
    select(-Date)


  temp <- dat %>%
    pivot_longer(cols = starts_with("Temperature"),
                 names_to = "logger_id",
                 values_to = "temperature") %>%
    select(date,
           logger_id,
           temperature) %>%
    filter(!is.na(temperature))

  rh <- dat %>%
    pivot_longer(cols = starts_with("RH"),
                 names_to = "logger_id",
                 values_to = "rh") %>%
    select(date,
           logger_id,
           rh)%>%
    filter(!is.na(rh))

  dew_point  <- dat %>%
    pivot_longer(cols = starts_with("Dew"),
                 names_to = "logger_id",
                 values_to = "dew_point") %>%
    select(date,
           logger_id,
           dew_point) %>%
    filter(!is.na(dew_point))


  #Fix to allow for two deployments of same logger. gets duplicate column names from hobo-export
  temp <- temp %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$")) %>%
    mutate(logger_id = str_extract(logger_id,
                                   "(^[0-9]*)"))

  rh <- rh %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$")) %>%
    mutate(logger_id = str_extract(logger_id,
                                   "(^[0-9]*)"))
  dew_point <- dew_point %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$")) %>%
    mutate(logger_id = str_extract(logger_id,
                                   "(^[0-9]*)"))

  if(!all(all(temp$date == rh$date),
          all(rh$date == dew_point$date))) stop("Tables datetimes doesn't match")

  combDat <- temp %>%
    full_join(rh,
              by = c("date" = "date",
                     "logger_id" = "logger_id")) %>%
    full_join(dew_point,
              by = c("date" = "date",
                     "logger_id" = "logger_id")) %>%
    arrange(logger_id,
            date) %>%
    mutate(logger_type = "MX2301A") %>%
    select(date,
           logger_type,
           logger_id,
           temperature,
           rh,
           dew_point)

  return(combDat)
}
