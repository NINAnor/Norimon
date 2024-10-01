#' get_logger_data Get temperature and light logger data from the database
#'
#' @param limit Optional row number limit (for testing)
#' @param dataset Which dataset to retreive data from? Default to "NasIns".,
#' @param agg_level Aggregation level of data. Character, either "year_locality" (default) or "locality_sampling"
#' @param as_tibble Return results as tibble? Boolean.
#'
#' @return Tibble or dataframe of logger data.
#' @export
#'
#' @examples
#' \dontrun{
#' locality_sampling_loggerdata <- get_logger_data(
#'   dataset = "NasIns",
#'   agg_level = "locality_sampling"
#' )
#' }
#'
## Currently the temperature data is in the long format, should be faster to rearrange it in the database and skip the pivot_wider here.
## Note that this splits up the temperature data from the different logger types (since they are not placed similarly in the field)
get_logger_data <- function(limit = NULL,
                            dataset = c("NasIns"),
                            agg_level = c(
                              "year_locality",
                              "locality_sampling"
                            ),
                            as_tibble = F) {
  Norimon::checkCon()


  dataset <- match.arg(dataset, choices = c(
    "NasIns",
    "OkoTrond",
    "TidVar",
    "Nerlands\u00f8ya"
  ))
  agg_level <- match.arg(agg_level, choices = c(
    "year_locality",
    "locality_sampling"
  ))


  ## Set up table sources
  ## Probably needs updating after new batch of data. Also need to test filtering of different identification types
  # locality_sampling <- dplyr::tbl(con, dbplyr::in_schema("events", "locality_sampling"))
  # year_locality <- dplyr::tbl(con, dbplyr::in_schema("events", "year_locality"))
  localities <- dplyr::tbl(con, dbplyr::in_schema("locations", "localities"))

  # logger_deployments <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_deployments"))
  # logger_data <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logger_data"))

  ## Do the pivot_wider on this side, temporarily, before I set this up in the database
  ## Not finished. Better to wait on a wider format database logger format and write a custom sql for the time interval join
  ## We also have the fuzzyjoin package that could do the trick, but it is logical to do the join and the aggregations
  ## on the database side anyway.


  ## Aggregate data to choosen level
  ## Add more choices!

  if (agg_level == "year_locality") {
    yl_query <- paste0(
      "
    SELECT yl.year, l.id as locality_id,ld.logger_type, ld.data_type,
    avg(ld.value) as avg_values,
    max(ld.value) as max_values,
    min(ld.value) as min_values,
    stddev_pop(ld.value) as stddev_values
    FROM loggers.logger_data ld,
    loggers.logger_deployments ldep,
    events.year_locality yl,
    locations.localities l
    WHERE ld.logger_id = ldep.logger_id
    AND ldep.year_locality_id = yl.id
    AND yl.start_date::timestamp <= ld.logger_time
    AND yl.end_date::timestamp >= ld.logger_time
    AND yl.locality_id = l.id
    AND yl.project_short_name = '",
      dataset,
      "'
    GROUP BY yl.id, l.id, ld.logger_type, ld.data_type
     "
    )

    yl_data_raw <- DBI::dbGetQuery(
      con,
      yl_query
    )

    yl_data <- yl_data_raw %>%
      pivot_wider(
        id_cols = c("locality_id", "year"),
        values_from = c("avg_values", "max_values", "min_values", "stddev_values"),
        names_from = c(logger_type, data_type),
        names_prefix = ""
      )

    loc_info <- localities %>%
      select(
        id,
        locality,
        habitat_type,
        region_name
      )

    res <- yl_data %>%
      left_join(loc_info,
        by = c("locality_id" = "id"),
        copy = T
      ) %>%
      select(-"locality_id") %>%
      select(
        locality,
        habitat_type,
        region_name,
        everything()
      ) %>%
      arrange(
        year,
        region_name,
        habitat_type,
        locality
      )
  }


  ## Works but could be faster? check the sql join and indices
  if (agg_level == "locality_sampling") {
    ls_query <- paste0(
      "
    SELECT yl.year, l.id as locality_id, ls.sampling_name, ld.logger_type, ld.data_type,
    avg(ld.value) as avg_values,
    max(ld.value) as max_values,
    min(ld.value) as min_values,
    stddev_pop(ld.value) as stddev_values
    FROM loggers.logger_data ld,
    loggers.logger_deployments ldep,
    events.year_locality yl,
    locations.localities l,
	events.locality_sampling ls
    WHERE ld.logger_id = ldep.logger_id
    AND ldep.year_locality_id = yl.id
	AND ls.year_locality_id = yl.id
	AND yl.locality_id = l.id
    AND ls.start_date::timestamp <= ld.logger_time
    AND ls.end_date::timestamp >= ld.logger_time
    AND yl.project_short_name = '",
      dataset,
      "'
    GROUP BY ls.id, yl.year, l.id, ld.logger_type, ld.data_type
	ORDER BY sampling_name
    "
    )

    ls_data_raw <- DBI::dbGetQuery(
      con,
      ls_query
    ) %>%
      as_tibble()

    ls_data <- ls_data_raw %>%
      pivot_wider(
        id_cols = c("year", "locality_id", "sampling_name"),
        values_from = c("avg_values", "max_values", "min_values", "stddev_values"),
        names_from = c(logger_type, data_type),
        names_prefix = ""
      )

    loc_info <- localities %>%
      select(
        id,
        locality,
        habitat_type,
        region_name
      )

    res <- ls_data %>%
      left_join(loc_info,
        by = c("locality_id" = "id"),
        copy = T
      ) %>%
      select(-"locality_id") %>%
      select(
        locality,
        habitat_type,
        region_name,
        everything()
      ) %>%
      arrange(
        year,
        region_name,
        habitat_type,
        locality
      )
  }


  if (!is.null(limit)) {
    res <- joined %>%
      head(limit)
  }

  if (as_tibble) {
    res <- res %>%
      as_tibble()
  }



  return(res)
}
