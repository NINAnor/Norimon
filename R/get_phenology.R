#' get_phenology Get insect phenology data from the database
#'
#' This provides a summary of insect catches, divided into taxa of a chosen level over the season, typically to be plotted by plot_phenology().
#'
#' @param id_type Optional filtering on identification/sampling technique, check get_id_types() for available options. Defaults to NULL with no filtering.
#' @param taxonomic_level taxonomic level for the comparison
#' @param subset_year Optional subset of year
#' @param subset_region Optional subset of region
#' @param subset_habitat Optional subset of habitat type ("Forest" or "Semi-nat")
#' @param trap_type Optional subset of trap type
#' @param limit Optional limit the output to number of rows (for testing)
#' @param dataset Choose the dataset to fetch data from. Default "NorIns" for national insect monitoring data
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
#'
#'   beetles <- get_observations(subset_orders = "Coleoptera",
#'                               agg_level = "year_locality")
#'
#' }
#'
#'
#'

get_phenology <- function(dataset = "NorIns",
                          id_type = NULL,
                          taxonomic_level = NULL,
                          subset_year = NULL,
                          subset_region = NULL,
                          subset_habitat = NULL,
                          subset_order = NULL,
                          trap_type = "All",
                          limit = NULL,
                          digits = 2,
                          return_tibble = F) {
  if (!exists("con")) {
    con <- NULL
  }

  Norimon::checkCon()

  if (!is.null(subset_region)) {
    subset_region <- match.arg(subset_region, choices = c(
      "Østlandet",
      "Vestlandet",
      "Trøndelag",
      "Sørlandet",
      "Nord-Norge"
    ))
  }
  if (!is.null(subset_habitat)) {
    subset_habitat <- match.arg(subset_habitat, choices = c(
      "Forest",
      "Semi-nat"
    ))
  }

  if(!is.null(id_type)){
    id_type <- match.arg(id_type, choices = unique(get_id_types(include_project_years = F)$identification_type))
  }


  dataset <- match.arg(dataset, choices = c("NorIns", "TidVar"))

  taxonomic_level <- match.arg(taxonomic_level, choices = c("Order", "Family"))
  trap_type <- match.arg(trap_type, choices = c("All", "MF", "VF", NULL))
  observations <- dplyr::tbl(con, dbplyr::in_schema("occurrences", "observations"))
  identifications <- dplyr::tbl(con, dbplyr::in_schema("events", "identifications"))
  sampling_trap <- dplyr::tbl(con, dbplyr::in_schema("events", "sampling_trap"))
  locality_sampling <- dplyr::tbl(con, dbplyr::in_schema("events", "locality_sampling"))
  year_locality <- dplyr::tbl(con, dbplyr::in_schema("events", "year_locality"))
  localities <- dplyr::tbl(con, dbplyr::in_schema("locations", "localities"))
  identification_techniques <- dplyr::tbl(con, dbplyr::in_schema("lookup", "identification_techniques"))
  traps <- dplyr::tbl(con, dbplyr::in_schema("locations", "traps"))

  joined <- observations %>%
    left_join(identifications,
              by = c(identification_id = "id"),
              suffix = c("_obs", "_ids")
    ) %>%
    left_join(identification_techniques,
              by = c("identification_name" = "identification_name"),
              suffix = c("_obs", "_idtechn")
    ) %>%
    left_join(sampling_trap,
              by = c(sampling_trap_id = "id"), suffix = c("_obs", "_st")
    ) %>%
    left_join(locality_sampling,
              by = c(locality_sampling_id = "id"),
              suffix = c("_obs", "_ls")
    ) %>%
    left_join(year_locality,
              by = c(year_locality_id = "id"), suffix = c("_obs", "_yl")
    ) %>%
    left_join(localities, by = c(locality_id = "id"), suffix = c(
      "_obs",
      "_loc"
    )) %>%
    left_join(traps, by = c(
      trap_id = "id",
      year = "year", locality = "locality"
    )) %>%
    mutate(year = as.character(year))

  #Remove 2020 extra samples
  joined <- joined %>%
    mutate(weeks_sampled = ifelse(grepl(
      "2020",
      year
    ) & (grepl("1", .data$trap_short_name) | grepl(
      "3",
      trap_short_name
    )), 2, 4)) %>%
    mutate(weeks_sampled = ifelse(grepl(
      "2020",
      year
    ), .data$weeks_sampled, 2))

  joined <- joined %>% filter(.data$weeks_sampled == 2)

  if (!is.null(subset_order)) {
    joined <- joined %>% filter(.data$id_order == subset_order)
  }

  if (!is.null(id_type)) {
    joined <- joined %>%
      dplyr::filter(identification_type %in% id_type)
  }

  if (!is.null(subset_region)) {
    subset_region <- c("", subset_region)
    joined <- joined %>% filter(.data$region_name %in% subset_region)
  }

  if (!is.null(subset_habitat)) {
    subset_habitat <- c("", subset_habitat)
    joined <- joined %>% filter(.data$habitat_type %in% subset_habitat)
  }

  if (!is.null(subset_year)) {
    subset_year <- c("", subset_year)
    joined <- joined %>% filter(.data$year %in% subset_year)
  }

  if (!is.null(dataset)) {
    joined <- joined %>% filter(.data$project_short_name ==
                                  dataset)
  }

  if (!is.null(trap_type) & trap_type != "All") {
    joined <- joined %>% filter(grepl((trap_type), .data$sample_name))
  }

  res <- joined

  weights <- res %>%
    select(sampling_name, trap_name, wet_weight) %>%
    distinct() %>%
    group_by(sampling_name) %>%
    summarise(tot_wet_weight = sum(wet_weight,
                                   na.rm = TRUE
    )) %>%
    collect()

  if (taxonomic_level == "Order") {
    res <- res %>%
      group_by(
        start_date_obs,
        end_date_obs,
        sampling_name,
        year_locality_id,
        locality_id,
        id_order,
        species_latin_fixed
      ) %>%
      summarise(
        no_asv_per_species = as.integer(n_distinct(sequence_id)),
        species_read_ab = sum(no_reads, na.rm = TRUE),
        .groups = "keep"
      ) %>%
      collect() %>%
      group_by(
        start_date_obs,
        end_date_obs,
        sampling_name,
        year_locality_id,
        locality_id,
        id_order
      ) %>%
      summarise(
        no_trap_days = mean(as.numeric(.data$end_date_obs - .data$start_date_obs)),
        no_species = n_distinct(.data$species_latin_fixed),
        shannon_div = round(calc_shannon(.data$species_latin_fixed), digits),
        mean_no_asv_per_species = round(mean(.data$no_asv_per_species), digits),
        order_read_ab = sum(species_read_ab, na.rm = TRUE), #Get total read number per order and sample
        .groups = "keep"
      ) %>%
      group_by(
        start_date_obs,
        end_date_obs,
        sampling_name,
        year_locality_id,
        locality_id
      ) %>%
      mutate(rel_read_ab = round(order_read_ab / sum(order_read_ab, na.rm = TRUE), digits = 10)) %>% #Normalize read number per order to total sample read number
      ungroup() %>%
      left_join(weights, by = c(sampling_name = "sampling_name")) %>%
      mutate(taxa_biomass = rel_read_ab * tot_wet_weight) %>%
      left_join(localities,
                by = c(locality_id = "id"),
                copy = T
      ) %>%
      left_join(year_locality,
                by = c(year_locality_id = "id"),
                copy = T
      ) %>%
      mutate(
        sampling_number = as.integer(gsub(
          "(.*)(sampling-)(.*)",
          "\\3", sampling_name
        )), start_date_obs = as.Date(start_date_obs),
        end_date_obs = as.Date(end_date_obs)
      ) %>%
      select(
        year,
        locality,
        sampling_name,
        sampling_number,
        id_order,
        habitat_type,
        region_name,
        start_date_obs,
        end_date_obs,
        no_trap_days,
        no_species,
        shannon_div,
        mean_no_asv_per_species,
        rel_read_ab,
        taxa_biomass
      ) %>%
      arrange(
        year,
        region_name,
        habitat_type,
        locality,
        sampling_name
      )

    if (!is.null(limit)) {
      res <- joined %>% head(limit)
    }

    if (return_tibble) {
      res <- res %>% as_tibble()
    }

    class(res) <- c("phenology", class(res))
    attr(res, "taxonomic_level") <- "id_order"

  }

  if (taxonomic_level == "Family") {
    res <- res %>%
      group_by(
        start_date_obs,
        end_date_obs,
        sampling_name,
        year_locality_id,
        locality_id,
        id_order,
        id_family,
        species_latin_fixed
      ) %>%
      summarise(
        no_asv_per_species = as.integer(n_distinct(sequence_id)),
        species_read_ab = sum(no_reads, na.rm = TRUE),
        .groups = "keep"
      ) %>%
      collect() %>%
      group_by(
        start_date_obs,
        end_date_obs,
        sampling_name,
        year_locality_id,
        locality_id, id_order,
        id_family
      ) %>%
      summarise(
        no_trap_days = mean(as.numeric(.data$end_date_obs - .data$start_date_obs)),
        no_species = n_distinct(.data$species_latin_fixed),
        shannon_div = round(calc_shannon(.data$species_latin_fixed), digits),
        mean_no_asv_per_species = round(mean(.data$no_asv_per_species), digits),
        family_read_ab = sum(species_read_ab, na.rm = TRUE),
        .groups = "keep") %>%
      group_by(
        start_date_obs,
        end_date_obs,
        sampling_name,
        year_locality_id,
        locality_id
      ) %>%
      mutate(rel_read_ab = round(family_read_ab / sum(family_read_ab, na.rm = TRUE), digits = 10)) %>% #
      ungroup() %>%
      left_join(weights, by = c(sampling_name = "sampling_name")) %>%
      mutate(taxa_biomass = rel_read_ab * tot_wet_weight) %>%
      left_join(localities,
                by = c(locality_id = "id"),
                copy = T
      ) %>%
      left_join(year_locality,
                by = c(year_locality_id = "id"),
                copy = T
      ) %>%
      mutate(
        sampling_number = as.integer(gsub(
          "(.*)(sampling-)(.*)",
          "\\3", sampling_name
        )), start_date_obs = as.Date(start_date_obs),
        end_date_obs = as.Date(end_date_obs)
      ) %>%
      select(
        year,
        locality,
        sampling_name,
        sampling_number,
        id_order,
        id_family,
        habitat_type,
        region_name,
        start_date_obs,
        end_date_obs,
        no_trap_days,
        no_species,
        shannon_div,
        mean_no_asv_per_species,
        rel_read_ab,
        taxa_biomass
      ) %>%
      arrange(
        year, region_name, habitat_type, locality,
        sampling_name
      )

    if (!is.null(limit)) {
      res <- joined %>% head(limit)
    }

    if (return_tibble) {
      res <- res %>% as_tibble()
    }

    class(res) <- c("phenology", class(res))
    attr(res, "taxonomic_level") <- "id_family"

  }
  return(res)
}

