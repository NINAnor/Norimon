#' longerHobo2202
#' Convenience function to read in a raw file of hobo logger MX2202 data and rearrange the columns to make is usable
#'
#' @param inputFile export file of raw logger data from HoboLink.com
#' @param guess_max optional number of rows to guess the data format from. May have to increase a lot
#' @param ...
#'
#' @return A tibble of well formatted logger data from the MX2301A type logger
#' @export
#'
#' @examples
#'
#' dontrun{
#'
#' mx2202_data <- longerHobo2202(inputFile = "../rawData/temp_logger/NasIns_2021_all_MX2202_2021_11_15_10_58_26_CET/NasIns_2021_all_MX2202_2021_11_15_10_58_26_CET_1.csv"
#'
#' }
#'
#'


longerHobo2202 <- function(inputFile,
                           guess_max = 10000){
  rawDat <- read_csv(inputFile,
                     guess_max = guess_max,
                     col_types = cols())

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

  light <- dat %>%
    pivot_longer(cols = starts_with("Light"),
                 names_to = "logger_id",
                 values_to = "light") %>%
    select(date,
           logger_id,
           light)%>%
    filter(!is.na(light))



  temp <- temp %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$"))
  light <- light %>%
    mutate(logger_id = str_extract(logger_id,
                                   "[^, ]+$"))

  if(!all(temp$date == light$date)) stop("Tables datetimes doesn't match")

  combDat <- temp %>%
    full_join(light,
              by = c("date" = "date",
                     "logger_id" = "logger_id")) %>%
    arrange(logger_id,
            date) %>%
    mutate(logger_type = "MX2202") %>%
    select(date,
           logger_type,
           logger_id,
           temperature,
           light)

  return(combDat)
}