#' get_biomass Get biomass data from the database
#'
#' @encoding UTF-8
#' @param limit Optional row limit on output (for testing)
#' @param trap_type Which trap types to fetch data for. "All" (defult), "MF", or "VF".
#' @param subset_orders Optional subset of order
#' @param subset_families Optional subset of families
#' @param subset_genus Optional subset of genus
#' @param subset_species Optional subset of species
#' @param subset_year Optional subset of year
#' @param subset_habitat Optional subset of habitat type ("Forest" or "Semi-nat")
#' @param subset_region Optional subset of region. Currently allowing "Trøndelag", "Østlandet", "Sørlandet"
#' @param dataset Which dataset to fetch data for. Default "NorIns".
#' @param agg_level Aggregate level of data. "year_locality" (default), "locality_sampling", "total".
#' @param as_tibble Return as tibble? Boolean
#'
#' @return A tibble of sample weights
#'
#' @examples
#' \dontrun{
#'
#' connect_to_insect_db()
#' biomass <- get_biomass(
#'   agg_level = "year_locality",
#'   trap_type = "MF"
#' )
#' }
#'
#' @export
#'

get_biomass <- function(limit = NULL,
                        subset_orders = NULL,
                        subset_families = NULL,
                        subset_genus = NULL,
                        subset_species = NULL,
                        subset_year = NULL,
                        subset_habitat = NULL,
                        trap_type = "MF",
                        subset_region = NULL,
                        dataset = "NorIns",
                        agg_level = "year_locality",
                        as_tibble = F) {
  if (!exists("con")) {
    con <- NULL
  }

  checkCon()

  if (!is.null(subset_region)) {
    subset_region <- match.arg(subset_region, choices = c(NULL, "\u00d8stlandet", "Tr\u00f8ndelag", "S\u00f8rlandet", "Nord-Norge"))
  }

  if (!is.null(subset_habitat)) {
    subset_habitat <- match.arg(subset_habitat, choices = c("Forest", "Semi-nat"))
  }

  dataset <- match.arg(dataset, choices = c(
    "NorIns",
    "OkoTrond",
    "TidVar",
    "HulEik",
    "Nerlands\u00f8ya"
  ))

  agg_level <- match.arg(agg_level, choices = c(
    "year_locality",
    "locality_sampling",
    "region_habitat",
    "region_habitat_year",
    "total",
    "none"
  ))

  trap_type <- match.arg(trap_type,
    choices = c("All", "MF", "VF", NULL)
  )

  ## Set up table sources
  ## Probably needs updating after new batch of data. Also need to test filtering of different identification types
  sampling_trap <- dplyr::tbl(con, dbplyr::in_schema("events", "sampling_trap"))
  locality_sampling <- dplyr::tbl(con, dbplyr::in_schema("events", "locality_sampling"))
  year_locality <- dplyr::tbl(con, dbplyr::in_schema("events", "year_locality"))
  localities <- dplyr::tbl(con, dbplyr::in_schema("locations", "localities"))
  traps <- dplyr::tbl(con, dbplyr::in_schema("locations", "traps"))


  ## Join the tables

  joined <- sampling_trap %>%
    dplyr::left_join(locality_sampling,
      by = c("locality_sampling_id" = "id"),
      suffix = c("_obs", "_ls")
    ) %>%
    dplyr::left_join(year_locality,
      by = c("year_locality_id" = "id"),
      suffix = c("_obs", "_yl")
    ) %>%
    dplyr::left_join(localities,
      by = c("locality_id" = "id"),
      suffix = c("_obs", "_loc")
    ) %>%
    dplyr::left_join(traps,
      by = c(
        "trap_id" = "id",
        "year" = "year",
        "locality_id" = "locality_id"
      )
    ) %>%
    dplyr::mutate(year = as.character(year))


  ## Exclude 2020 4 week samplings

  joined <- joined %>%
    dplyr::mutate(weeks_sampled = ifelse(grepl("2020", year) & (grepl("1", trap_short_name) | grepl("3", trap_short_name)), 2, 4)) %>%
    dplyr::mutate(weeks_sampled = ifelse(grepl("2020", year), weeks_sampled, 2))

  joined <- joined %>%
    dplyr::filter(weeks_sampled == 2)

  # filter on dataset

  if (!is.null(dataset)) {
    joined <- joined %>%
      dplyr::filter(project_short_name == dataset)
  }

  # filter on trap type (recommended to only take MF)
  if (!is.null(trap_type) & trap_type != "All") {
    joined <- joined %>%
      dplyr::filter(grepl((trap_type), sample_name))
  }

  # Filter on region name
  if (!is.null(subset_region)) {
    subset_region <- c("", subset_region)
    joined <- joined %>%
      dplyr::filter(region_name %IN% subset_region)
  }

  # Filter on habitat
  if (!is.null(subset_habitat)) {
    subset_habitat <- c("", subset_habitat)
    joined <- joined %>%
      dplyr::filter(.data$habitat_type %in% subset_habitat)
  }

  # Filter on order
  if (!is.null(subset_orders)) {
    subset_orders <- c("", subset_orders) # To allow one-length subsets
    joined <- joined %>%
      dplyr::filter(.data$id_order %IN% subset_orders)
  }

  # Filter on families
  if (!is.null(subset_families)) {
    subset_families <- c("", subset_families)
    joined <- joined %>%
      dplyr::filter(.data$id_family %in% subset_families)
  }

  # Filter on species
  if (!is.null(subset_species)) {
    subset_species <- c("", subset_species)
    joined <- joined %>%
      dplyr::filter(.data$species_latin_fixed %in% subset_species)
  }

  # Filter on year
  if (!is.null(subset_year)) {
    subset_year <- c("", subset_year)
    joined <- joined %>%
      dplyr::filter(.data$year %in% subset_year)
  }

  # Filter on genus
  if (!is.null(subset_genus)) {
    subset_genus <- c("", subset_genus)
    joined <- joined %>%
      dplyr::filter(.data$id_genus %in% subset_genus)
  }


  res <- joined



  ## This is slow because we have to collect the data before we calculate Shannon index.
  ## Best would be to do the Shannon calc on the database side. Seems harder than I first thought.
  if (agg_level == "year_locality") {
    res <- res %>%
      dplyr::collect() %>%
      dplyr::group_by(year_locality_id, locality_id) %>%
      dplyr::summarise(
        sum_wet_weight = sum(wet_weight_bottle - weight_empty_bottle, na.rm = T),
        avg_wet_weight = mean(wet_weight_bottle - weight_empty_bottle, na.rm = T),
        .groups = "keep"
      ) %>%
      dplyr::left_join(localities,
        by = c("locality_id" = "id"),
        copy = T
      ) %>%
      dplyr::left_join(year_locality,
        by = c(
          "year_locality_id" = "id",
          "locality_id" = "locality_id"
        ),
        copy = T
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        year,
        locality,
        habitat_type,
        region_name,
        sum_wet_weight,
        avg_wet_weight
      ) %>%
      dplyr::arrange(
        year,
        region_name,
        habitat_type,
        locality
      )
  }


  if (agg_level == "locality_sampling") {
    res <- res %>%
      dplyr::collect() %>%
      dplyr::group_by(sampling_name, year_locality_id, locality_id) %>%
      dplyr::summarise(
        no_trap_days = mean(as.numeric(end_date_obs - start_date_obs)),
        sum_wet_weight = sum(wet_weight_bottle - weight_empty_bottle, na.rm = T),
        avg_wet_weight = mean(wet_weight_bottle - weight_empty_bottle, na.rm = T),
        .groups = "keep"
      ) %>%
      dplyr::left_join(localities,
        by = c("locality_id" = "id"),
        copy = T
      ) %>%
      dplyr::left_join(year_locality,
        by = c("year_locality_id" = "id"),
        copy = T
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        year,
        locality,
        sampling_name,
        habitat_type,
        region_name,
        no_trap_days,
        sum_wet_weight,
        avg_wet_weight
      ) %>%
      dplyr::arrange(
        year,
        region_name,
        habitat_type,
        locality,
        sampling_name
      )
  }



  if (agg_level == "total") {
    res <- res %>%
      dplyr::collect() %>%
      dplyr::summarise(
        sum_wet_weight = sum(wet_weight_bottle - weight_empty_bottle, na.rm = T),
        avg_wet_weight = mean(wet_weight_bottle - weight_empty_bottle, na.rm = T),
        .groups = "keep"
      ) %>%
      dplyr::select(
        sum_wet_weight,
        avg_wet_weight
      )
  }

  if (!is.null(limit)) {
    res <- joined %>%
      head(limit)
  }


  res <- res %>%
    dplyr::mutate(
      sum_wet_weight = round(sum_wet_weight, 2),
      avg_wet_weight = round(avg_wet_weight, 2)
    )

  if (as_tibble) {
    res <- res %>%
      dplyr::as_tibble()
  }



  return(res)
}
