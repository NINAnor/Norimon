#' Fetch table of identification types in the database.
#'
#' Used to lookup what "id_types" are available to filter on in e.g. get_observations(). The names in the "identification_types" column can be us
#'
#' @param include_project_years Include list of which projects and years each method was used? Boolean, default = TRUE.
#'
#' @return A tibble of the identification names (optionally with the projects they where used in)
#' @export
#'
#' @examples
#'
#'\dontrun{
#'
#' get_id_types()
#'
#' tidvar_2019_legs <- get_observations(id_type = "legs_col_hem_lep", dataset = "TidVar")
#'
#'}
#'
#'

get_id_types <- function(include_project_years = TRUE){

  checkCon()

  id_techn <- dplyr::tbl(con,
                         DBI::Id(schema = "lookup",
                            table = "identification_techniques"))

  if(include_project_years){

    id_tech_proj_q <- "
    SELECT foo.*,
    it.identification_type,
    it.identification_details
    FROM lookup.identification_techniques it,
    (
      SELECT DISTINCT project_short_name,
      year,
      identification_name,
      identification_status
      FROM events.identifications i,
      events.sampling_trap st,
      events.locality_sampling ls,
      events.year_locality yl
      WHERE i.sampling_trap_id = st.id
      AND st.locality_sampling_id = ls.id
      AND ls.year_locality_id = yl.id
    ) foo
    WHERE it.identification_name = foo.identification_name
    ORDER BY project_short_name, year, it.identification_name
    "

    res <- DBI::dbGetQuery(con,
                           id_tech_proj_q) |>
      dplyr::as_tibble()

  } else {

    res <- id_techn |>
      dplyr::select(-id) |>
      dplyr::collect()

  }

  return(res)

}
