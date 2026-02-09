#' get_year_region_stats
#'
#' Utility function, primarily for the plot_year_locality_sum plot function in the external shiny app
#'
#' @param project_short_name The project to fetch data for (Default: "NorIns")
#' @param last_year The last year of the data to fetch info for
#'
#'
#' @return A tibble of project year data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' to_project_year_plot <- get_year_region_stats(last_year = 2025)
#'
#' }
#'

get_year_region_stats <- function(project_short_name = "NorIns",
                                  last_year = 2024) {

  checkCon()
  project_to_filter <- match.arg(project_short_name,
                              choices = c("NorIns", "TidVar", "HulEik")
                              )


   project_year_localities <- tbl(
    con,
    DBI::Id(
      schema = "views",
      table = "project_year_localities"
      )
  ) %>%
    dplyr::mutate(habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type))

  proj_sum <- project_year_localities %>%
    dplyr::filter(project_short_name == project_to_filter) %>%
    dplyr::collect() %>%
    dplyr::mutate(region_name = factor(region_name,
                                levels = c(
                                  "Sørlandet",
                                  "Østlandet",
                                  "Vestlandet",
                                  "Trøndelag",
                                  "Nord-Norge"
                                )
    )) %>%
    dplyr::mutate(habitat_type = factor(habitat_type)) %>%
    dplyr::filter(year <= last_year) |>
    dplyr::mutate(year = factor(year, levels = last_year:min(year))) %>%
    dplyr::group_by(region_name,
             habitat_type,
             year,
             .drop = FALSE
    ) %>%
    dplyr::summarise(
      visits = as.integer(n()),
      .groups = "drop"
    ) %>%
    dplyr::mutate(habitat_type = as.character(habitat_type)) %>%
    dplyr::arrange(
      region_name,
      year,
      habitat_type
    )


  return(proj_sum)
}
