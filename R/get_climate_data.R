#' get_climate_data
#'
#' Fetch climatic records for a particular locality. Data includes daily mean temperature, total precipitation, and total snow depth. Data stretches from 1957 up until the latest import to the database.
#'
#' @param locality A character of the locality name, e.g. "Semi-nat_01"
#'
#' @return A tibble of daily climate data from 1957.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' climate_data <- get_climate_data("Semi-nat_01")
#'
#' }
#'
#'
#'

get_climate_data <- function(locality = NULL){

  if(is.null(locality)) stop("Need a locality name as character")

  clim_tbl <- dplyr::tbl(con,
                       DBI::Id(schema = "climate_data",
                          table = "se_norge"
                          ))

  out <- clim_tbl %>%
    dplyr::filter(locality == !!dplyr::enquo(locality)) %>%
    dplyr::select(locality,
           date,
           daily_sum_precip = locality_rr,
           daily_mean_temp = locality_tg,
           daily_mean_snow_depth = locality_sd) %>%
    dplyr::collect()

  return(out)

 }


