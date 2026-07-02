#' Get basic sample info (locality, date and trap type)
#'
#' @param dataset Which dataset to fetch data for.
#'
#' @returns A tibble with sample info
#' @export
#'
#' @examples
#'
#' \dontrun{
#' get_sample_info(dataset = "NorIns")
#'
#' }
#'

get_sample_info <- function(dataset = c("NorIns"))
  {

  checkCon()

  dataset <- match.arg(dataset,
                       get_projects()$project_short_name
  )

  raw_sql <- "select l.locality,
  yl.year,
  ls.sampling_name,
  st.sample_name,
  ls.start_date::date,
  ls.end_date::date,
  extract(day FROM (ls.end_date - ls.start_date)) as no_trap_days,
  tt.trap_type,
  traps.trap_model,
  traps.liquid_name
  from events.year_locality yl,
  events.locality_sampling ls,
  events.sampling_trap st,
  locations.localities l,
  locations.traps,
  lookup.trap_types tt
  where st.locality_sampling_id = ls.id
  and ls.year_locality_id = yl.id
  and yl.locality_id = l.id
  and st.trap_id  = traps.id
  and tt.trap_model = traps.trap_model
  and yl.project_short_name = ?id1
  "

  san_sql <- DBI::sqlInterpolate(con,
                                 raw_sql,
                                 id1 = dataset)

  res <- DBI::dbGetQuery(con,
                    san_sql) |>
    dplyr::as_tibble()

  return(res)
}

