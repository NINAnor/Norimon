#' get_forest_type Get the forest types from a buffer around the sampling localities
#'
#' @param limit Optional row limit of returned data (for testing )
#' @param dataset Dataset to get data from. Defaults to "NasIns"
#' @param stat_grouping Group data into either "treslag" or "bonitet"
#' @param as_tibble Return as tibble? Boolean
#'
#' @return Returns a tibble of forest data from the database.
#' @export
#'
#' @examples
#'
#' get_forest_type()
#'
#'

get_forest_type <- function(limit = NULL,
                             dataset = c("NasIns"),
                             stat_grouping = c("treslag", "bonitet"),
                             as_tibble = F) {


  dataset <- match.arg(dataset, choices = c("NasIns",
                                            "OkoTrond",
                                            "TidVar",
                                            "Nerlandsøya"))

  proj_filter <- dplyr::enquo(dataset)


  yl <- dplyr::tbl(con,
                   DBI::Id(schema = "events",
                      table = "year_locality")
  )
  l <- dplyr::tbl(con,
                  DBI::Id(schema = "locations",
                     table = "localities")
  )

  year_locality <- l %>%
    left_join(yl,
              by = c("id" = "locality_id")) %>%
    filter(project_short_name == !!proj_filter) %>%
    select(locality) %>%
    pull()


  skog <- read_sf(con,
                  DBI::Id(schema = "backgrounds",
                     table = "locality_1000m_buffer_skogtype_agg")
  ) %>%
    sf::st_drop_geometry() %>%
    filter(locality %in% year_locality)


  group_enq <- dplyr::enquo(stat_grouping)

  if(length(stat_grouping) == 1){
    skog <- skog %>%
      group_by(across(all_of(c("locality", stat_grouping)))) %>%
      summarise(area = sum(area))
  }


  out <-  skog %>%
    pivot_wider(id_cols = locality,
                names_from = !!group_enq,
                values_from = area,
                names_prefix = "area_")


  out <- out %>%
    mutate_if(is.numeric,
              replace_na,
              replace = 0)

  if(as_tibble){
    out <- out %>%
      as_tibble()
  }


  return(out)
}

