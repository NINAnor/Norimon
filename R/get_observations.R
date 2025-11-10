#' get_observations Get insect observation data from the database
#'
#' @param id_type Optional filtering on identification/sampling technique, check get_id_types() for available options. Defaults to NULL with no filtering.
#' @param id_status Optional filtering on identification status, check get_id_types() for available options. Defaults to "Primary" to only get one identification per sample. This exists because there might be multiple identifications with the same identification method on the same sample.
#' @param subset_orders Optional subset of order
#' @param subset_families Optional subset of families
#' @param subset_genus Optional subset of genus
#' @param subset_species Optional subset of species
#' @param subset_year Optional subset of year
#' @param subset_region Optional subset of region
#' @param subset_habitat Optional subset of habitat type ("Forest" or "Semi-nat")
#' @param trap_type Optional subset of trap type
#' @param limit Optional limit the output to number of rows (for testing)
#' @param dataset Choose the dataset to fetch data from. Default "NorIns" for national insect monitoring data
#' @param agg_level Aggregation level of data. "year_locality", "region_habitat", "region_habitat_year", "locality_sampling", "total" and "none" for raw data. Default to year_locality
#' @param Hill Calculate shannon diversity as Hill number (exp(Shann_div)). Boolean
#' @param richn_corr Correct GDE calculation by number of species (doi: https://doi.org/10.1101/2022.02.09.479762). Boolean.
#' @param digits Number of digits to round shannon diversity and mean ASV counts to. (defaults to 2)
#' @param as_tibble Coerce output to class tibble
#'
#' @return A tibble of insect observations from the database
#' @export
#'
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#'
#'   connect_to_insect_db()
#'
#'   beetles <- get_observations(subset_orders = "Coleoptera",
#'                               agg_level = "year_locality")
#'
#' }
#'



get_observations <- function(dataset = "NorIns",
                             id_type = NULL,
                             id_status = "Primary",
                             trap_type = "All",
                             subset_orders = NULL,
                             subset_families = NULL,
                             subset_genus = NULL,
                             subset_species = NULL,
                             subset_year = NULL,
                             subset_region = NULL,
                             subset_habitat = NULL,
                             limit = NULL,
                             agg_level = "year_locality",
                             Hill = TRUE,
                             richn_corr = TRUE,
                             digits = 2,
                             as_tibble = F) {
  # Bind these variables to stop R CMD check complaints
  if (!exists("con")) {
    con <- NULL
  }

  checkCon()

  if (!is.null(subset_region)) {
    subset_region <- match.arg(subset_region, choices = c("Tr\u00f8ndelag", "\u00d8stlandet", "S\u00f8rlandet", "Nord-Norge"))
  }

  if (!is.null(subset_habitat)) {
    subset_habitat <- match.arg(subset_habitat, choices = c("Forest", "Semi-nat"))
  }

  if(!is.null(id_type)){
    id_type <- match.arg(id_type, choices = unique(get_id_types(include_project_years = FALSE)$identification_type))
  }

  if(!is.null(id_status)){
    id_status <- match.arg(id_status, choices = unique(get_id_types(include_project_years = TRUE)$identification_status))
  }


  dataset <- match.arg(dataset, choices = c(
    "NorIns",
    "OkoTrond",
    "TidVar",
    "Nerlands\u00f8ya",
    "HulEik"
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
  observations <- dplyr::tbl(con, dbplyr::in_schema("occurrences", "observations"))
  identifications <- dplyr::tbl(con, dbplyr::in_schema("events", "identifications"))
  sampling_trap <- dplyr::tbl(con, dbplyr::in_schema("events", "sampling_trap"))
  locality_sampling <- dplyr::tbl(con, dbplyr::in_schema("events", "locality_sampling"))
  year_locality <- dplyr::tbl(con, dbplyr::in_schema("events", "year_locality"))
  localities <- dplyr::tbl(con, dbplyr::in_schema("locations", "localities"))
  identification_techniques <- dplyr::tbl(con, dbplyr::in_schema("lookup", "identification_techniques"))
  traps <- dplyr::tbl(con, dbplyr::in_schema("locations", "traps"))

  ## Join the tables

  joined <- observations %>%
    dplyr::left_join(identifications,
      by = c("identification_id" = "id"),
      suffix = c("_obs", "_ids")
    ) %>%
    dplyr::left_join(identification_techniques,
      by = c("identification_name" = "identification_name"),
      suffix = c("_obs", "_idtechn")
    ) %>%
    dplyr::left_join(sampling_trap,
      by = c("sampling_trap_id" = "id"),
      suffix = c("_obs", "_st")
    ) %>%
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


  if (!is.null(id_type)) {
    joined <- joined %>%
      dplyr::filter(identification_type %in% id_type)
  }

  if (!is.null(id_status)) {
    joined <- joined %>%
      dplyr::filter(identification_status %in% id_status)
  }

  # Filter on region name
  if (!is.null(subset_region)) {
    subset_region <- c("", subset_region)
    joined <- joined %>%
      dplyr::filter(region_name %in% subset_region)
  }

  # Filter on habitat
  if (!is.null(subset_habitat)) {
    subset_habitat <- c("", subset_habitat)
    joined <- joined %>%
      dplyr::filter(habitat_type %in% subset_habitat)
  }

  # Filter on order
  if (!is.null(subset_orders)) {
    subset_orders <- c("", subset_orders) # To allow one-length subsets
    joined <- joined %>%
      dplyr::filter(id_order %IN% subset_orders)
  }

  # Filter on families
  if (!is.null(subset_families)) {
    subset_families <- c("", subset_families)
    joined <- joined %>%
      dplyr::filter(id_family %in% subset_families)
  }

  # Filter on species
  if (!is.null(subset_species)) {
    subset_species <- c("", subset_species)
    joined <- joined %>%
      dplyr::filter(species_latin_fixed %in% subset_species)
  }

  # Filter on year
  if (!is.null(subset_year)) {
    subset_year <- c("", subset_year)
    joined <- joined %>%
      dplyr::filter(year %in% subset_year)
  }

  # Filter on genus
  if (!is.null(subset_genus)) {
    subset_genus <- c("", subset_genus)
    joined <- joined %>%
      dplyr::filter(id_genus %in% subset_genus)
  }

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


  ## Aggregate data to chosen level
  ## Add more choices?

  res <- joined

  ## This is slow because we have to collect the data before we calculate Shannon index.
  ## Best would be to do the Shannon calc on the database side. Seems harder than I first thought.
  if (agg_level == "year_locality") {
    res <- res %>%
      dplyr::collect() %>%
      dplyr::group_by(year_locality_id, locality_id, species_latin_fixed) %>% ## Error here, it collapses to species level, doesn't keep duplicate species
      dplyr::summarise(
        no_asv_per_species = dplyr::n_distinct(sequence_id),
        .groups = "keep"
      ) %>%
      dplyr::group_by(year_locality_id, locality_id) %>%
      dplyr::summarise(
        no_species = dplyr::n_distinct(species_latin_fixed),
        shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
        mean_no_asv_per_species = round(mean(no_asv_per_species), digits),
        GDE_by_asv = round(calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr), digits),
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
        no_species,
        shannon_div,
        mean_no_asv_per_species,
        GDE_by_asv
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
      dplyr::group_by(start_date_obs, end_date_obs, sampling_name, year_locality_id, locality_id, species_latin_fixed) %>%
      dplyr::summarise(
        no_asv_per_species = dplyr::n_distinct(sequence_id),
        .groups = "keep"
      ) %>%
      dplyr::group_by(sampling_name, year_locality_id, locality_id) %>%
      dplyr::summarise(
        no_trap_days = mean(as.numeric(end_date_obs - start_date_obs)), ## to get the mean trap days from all traps within the sampling event (should be the same for all traps)
        no_species = dplyr::n_distinct(species_latin_fixed),
        shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
        mean_no_asv_per_species = round(mean(no_asv_per_species), digits),
        GDE_by_asv = calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr),
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
        no_species,
        shannon_div,
        mean_no_asv_per_species,
        GDE_by_asv
      ) %>%
      dplyr::arrange(
        year,
        region_name,
        habitat_type,
        locality,
        sampling_name
      )
  }

  if (agg_level == "region_habitat") {
    res <- res %>%
      dplyr::collect() %>%
      dplyr::group_by(
        region_name,
        habitat_type,
        species_latin_fixed
      ) %>%
      dplyr::summarise(
        no_asv_per_species = dplyr::n_distinct(sequence_id),
        .groups = "keep"
      ) %>%
      dplyr::group_by(
        region_name,
        habitat_type
      ) %>%
      dplyr::summarise(
        no_species = dplyr::n_distinct(species_latin_fixed),
        shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
        mean_no_asv_per_species = round(mean(no_asv_per_species), digits),
        GDE_by_asv = calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr),
        .groups = "keep"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        habitat_type,
        region_name,
        no_species,
        shannon_div,
        mean_no_asv_per_species,
        GDE_by_asv
      ) %>%
      dplyr::arrange(
        habitat_type,
        region_name
      )
  }


  if (agg_level == "region_habitat_year") {
    res <- res %>%
      dplyr::collect() %>%
      dplyr::group_by(
        region_name,
        habitat_type,
        year,
        species_latin_fixed
      ) %>%
      dplyr::summarise(
        no_asv_per_species = dplyr::n_distinct(sequence_id),
        .groups = "keep"
      ) %>%
      dplyr::group_by(
        region_name,
        habitat_type,
        year
      ) %>%
      dplyr::summarise(
        no_species = dplyr::n_distinct(species_latin_fixed),
        shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
        mean_no_asv_per_species = round(mean(no_asv_per_species), digits),
        GDE_by_asv = calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr),
        .groups = "keep"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        year,
        habitat_type,
        region_name,
        no_species,
        shannon_div,
        mean_no_asv_per_species,
        GDE_by_asv
      ) %>%
      dplyr::arrange(
        year,
        habitat_type,
        region_name
      )
  }

  if (agg_level == "total") {
    res <- res %>%
      dplyr::collect() %>%
      dplyr::group_by(species_latin_fixed) %>%
      dplyr::summarise(
        no_asv_per_species = dplyr::n_distinct(sequence_id),
        .groups = "keep"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(
        no_species = dplyr::n_distinct(species_latin_fixed),
        shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
        mean_no_asv_per_species = round(mean(no_asv_per_species), digits),
        GDE_by_asv = calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr)
      ) %>%
      dplyr::select(
        no_species,
        shannon_div,
        mean_no_asv_per_species,
        GDE_by_asv
      )
  }

  if (!is.null(limit)) {
    res <- joined %>%
      head(limit)
  }

  if (as_tibble) {
    res <- res %>%
      as_tibble()
  }

  return(res)
}
