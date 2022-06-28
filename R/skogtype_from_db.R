#' skogtype_from_db Get the forest types from a buffer around the sampling localities
#'
#' @param limit
#' @param dataset
#' @param stat_grouping
#' @param as_tibble
#'
#' @return
#' @export
#'
#' @examples
skogtype_from_db <- function(limit = NULL,
                             dataset = c("NasIns"),
                             stat_grouping = c("treslag", "bonitet"),
                             as_tibble = F) {


  dataset <- match.arg(dataset, choices = c("NasIns",
                                            "OkoTrond",
                                            "TidVar",
                                            "Nerlandsøya"))

  proj_filter <- dplyr::enquo(dataset)


  yl <- dplyr::tbl(con,
                   Id(schema = "events",
                      table = "year_locality")
  )
  l <- dplyr::tbl(con,
                  Id(schema = "locations",
                     table = "localities")
  )

  year_locality <- l %>%
    left_join(yl,
              by = c("id" = "locality_id")) %>%
    filter(project_short_name == !!proj_filter) %>%
    select(locality) %>%
    pull()


  skog <- read_sf(con,
                  Id(schema = "backgrounds",
                     table = "locality_1000m_buffer_skogtype_agg")
  ) %>%
    st_drop_geometry() %>%
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

