#' Get list of all projects in the insect_monitoring DB
#'
#' @returns a tibble with the projects, where the names 'projects_short_name' is used for subsetting data in other functions
#' @export
#'
#' @examples
#' \dontrun{
#' get_projects()
#'
#' }
#'
get_projects <- function(){

  Norimon::checkCon()

  projects <- tbl(con,
                  DBI::Id(schema = "lookup",
                          table = "projects")) |>
    select(-id) |>
    dplyr::as_tibble()

  return(projects)

}
