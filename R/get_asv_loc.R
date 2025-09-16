



#' get_asv_loc
#'
#' @param species What species to fetch records for. Character string.
#' @param dataset Optionally subset to a (character vector of) project(s).
#' @param subset_years Optionally subset vector of years
#'
#' @return A tibble with asv data.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' anc_bad_asv <- get_asv_loc("Ancylis badiana")
#'
#' }
#'
#'



get_asv_loc <- function(species = NULL,
                        dataset = NULL,
                        subset_years = NULL) {

  asv_perc_reads <- dplyr::tbl(con,
                        DBI::Id(schema = "views",
                                table = "asv_perc_reads"
                        )
  )


  if(!is.null(dataset)) {
    asv_perc_reads <- asv_perc_reads |>
      dplyr::filter(project_short_name %in% dataset)
  }

  if(!is.null(subset_years)){
    asv_perc_reads <- asv_perc_reads |>
      dplyr::filter(year %in% subset_years)
  }


  asv_wider <- asv_perc_reads %>%
    dplyr::filter(species_latin_fixed %in% species) %>%
    dplyr::collect() %>%
    dplyr::as_tibble()

  return(asv_wider)
}
