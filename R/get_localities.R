#' get_localities
#'
#' Get the localities from the database, either as an sf object or as a tibble
#'
#' @param dataset Which dataset to fetch ("NorIns", "OkoTrond", "TidVar", "Nerlandsøya", "HulEik", "All") . Default to "NorIns" for the national insect monitoring data
#' @param as_sf Return an sf object? Boolean.
#' @param habitat_type Optional habitat type to subset
#'
#' @return An sf or tibble with the location info
#' @export
#'
#' @examples
#' \dontrun{
#' connect_to_insect_db()
#' loc <- get_localities()
#' ggplot(loc) +
#'   geom_sf(aes(fill = habitat_type))
#' }
#'
get_localities <- function(dataset = c(
                             "NorIns",
                             "OkoTrond",
                             "TidVar",
                             "Nerlands\u00f8ya",
                             "All"
                           ),
                           as_sf = TRUE,
                           habitat_type = c(
                             "All",
                             "Semi-nat",
                             "Forest",
                             "Tid_man",
                             "Tid_aut",
                             "Wetland"
                           )) {
  Norimon::checkCon()

  dataset <- match.arg(dataset, choices = c(
    "NorIns",
    "OkoTrond",
    "TidVar",
    "HulEik",
    "Nerlands\u00f8ya",
    "All"
  ))

  habitat_type_subset <- match.arg(habitat_type,
    choices = c(
      "All",
      "Semi-nat",
      "Forest",
      "Tid_man",
      "Tid_aut",
      "Wetland"
    )
  )


  localities <- sf::read_sf(
    con,
    DBI::Id(
      schema = "locations",
      table = "localities"
    )
  )

  year_locality <- dplyr::tbl(
    con,
    DBI::Id(
      schema = "events",
      table = "year_locality"
    )
  )


  temp <- localities %>%
    right_join(year_locality,
      by = c(
        "id" = "locality_id",
        "ano_flate_id" = "ano_flate_id",
        "ssbid" = "ssbid"
      ),
      copy = TRUE
    )

  if (dataset != "All") {
    temp <- temp %>%
      filter(project_short_name == dataset)
  }

  if (habitat_type_subset != "All") {
    temp <- temp %>%
      filter(habitat_type == habitat_type_subset)
  }

  out <- temp %>%
    select(
      locality,
      ssbid,
      ano_flate_id,
      year,
      project_short_name,
      region_name,
      habitat_type
    ) %>%
    arrange(locality)

  if (!as_sf) {
    out <- st_drop_geometry(out)
  }

  return(out)
}
