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
#' logger_data <- download_licor_logger_data(loggers = c("20878881", "21386849"),
#'                                           start_date = "2024-06-01",
#'                                           end_date = "2024-06-30",
#'                                           token = "your_token")
#'
#'
#' }
#'
get_licor_logger_data <- function (loggers = NULL,
                                   start_date = NULL,
                                   end_date = NULL,
                                   token = NULL) {

  url="https://api.licor.cloud"
  indicator_path <- "/v2/data?deviceSerialNumber="
  #start_date_time <- paste0("&start_date_time=", start_date, "%2000%3A00%3A00")
  start_date_unix <- as.numeric(as.POSIXct(start_date, tz = "UTC")) * 1000
  start_date_time <- paste0("&startTime=", start_date_unix)
  end_date_unix <- as.numeric(as.POSIXct(end_date, tz = "UTC")) * 1000
  end_date_time <- paste0("&endTime=", end_date_unix)

  formatted_cont_to_tibble <- function(formatted_cont){
    # Initialize an empty list to store intermediate data frames
    list_of_dfs <- list()

    # Loop through each sensor entry in the main data frame
    for (i in seq_along(formatted_cont$sensors$sensorSerialNumber)) {

      # Extract common sensor info once per sensorSerialNumber
      serial_number <- formatted_cont$sensors$sensorSerialNumber[i]
      serial_number_stripped <- gsub("-\\d+$", "", serial_number)


      # Extract measurement-specific info
      measurement_type <- formatted_cont$sensors$data[[i]]$measurementType
      units <- formatted_cont$sensors$data[[i]]$unit

      # Extract the records matrix (the timestamp/value pair)
      # The records are nested inside another list of length 1, so use [[1]]
      records_matrix <- formatted_cont$sensors$data[[i]]$records[[1]]

      # Create a temporary data frame from the matrix
      temp_df <- data.frame(
        timestamp_ms = records_matrix[, 1],
        value = records_matrix[, 2]
      )

      # Convert milliseconds to POSIXct datetime (seconds)
      # Unix time is in seconds; the input is in milliseconds.
      temp_df$timestamp <- as.POSIXct(temp_df$timestamp_ms / 1000,
                                      origin = "1970-01-01",
                                      tz = "UTC")

      # Add the descriptive columns we need to this temporary data frame
      temp_df$logger_id <- serial_number_stripped
      temp_df$measurement_type <- measurement_type
      temp_df$units <- units

      # Select and reorder the required columns
      final_cols_df <- temp_df[c("logger_id", "measurement_type", "units", "timestamp", "value")]

      # Add this completed data frame to our list
      list_of_dfs[[length(list_of_dfs) + 1]] <- final_cols_df
    }

    # Combine all the data frames in the list into one master data frame
    final_combined_data <- do.call(rbind, list_of_dfs)

    return(final_combined_data)

  }

  #Loop over all loggers
  logger_data_list <- list()

  for(i in seq_along(loggers)){

    logger <- loggers[[i]]

    combinedURL <- paste0(url, indicator_path, logger, start_date_time, end_date_time)
    auth_string <- paste("Bearer", token, sep = " ")

    tryCatch({
      myOrg <- httr::GET(url = combinedURL,
                         encode = "json",
                         httr::add_headers(Authorization = auth_string))
    })

    rawContent <- httr::content(myOrg, as = "text")
    formatted_cont <- jsonlite::fromJSON(rawContent)

    if(myOrg$status_code != 200 | length(formatted_cont$sensors$data) == 0){
      stop({
        cat(formatted_cont$error_description)
        cat("\n")
        cat(formatted_cont$message)
        cat("\n")
      })
    }

    logger_data_list[[length(logger_data_list) + 1]] <- formatted_cont_to_tibble(formatted_cont)
  }

  length(logger_data_list)

  out <- do.call(rbind, logger_data_list) |>
    dplyr::as_tibble()

  return(out)
}


