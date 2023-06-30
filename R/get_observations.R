#' get_observations Get insect observation data from the database
#'
#' @param id_type Type of identification type. Defaults to metabarcoding data
#' @param subset_orders Optional subset of order
#' @param subset_families Optional subset of families
#' @param subset_genus Optional subset of genus
#' @param subset_species Optional subset of species
#' @param subset_year Optional subset of year
#' @param subset_region Optional subset of region
#' @param subset_habitat Optional subset of habitat type ("Forest" or "Semi-nat")
#' @param trap_type Optional subset of trap type
#' @param limit Optional limit the output to number of rows (for testing)
#' @param dataset Choose the dataset to fetch data from. Default "NasIns" for national insect monitoring data
#' @param agg_level Aggregation level of data. "year_locality", "region_habitat", "region_habitat_year", "locality_sampling", "total". Default to year_locality
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
#'
#' \dontrun{
#'

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



get_observations <- function(id_type = c("metabarcoding"),
                        subset_orders = NULL,
                        subset_families = NULL,
                        subset_genus = NULL,
                        subset_species = NULL,
                        subset_year = NULL,
                        subset_region = NULL,
                        subset_habitat = NULL,
                        trap_type = "All",
                        limit = NULL,
                        dataset = "NasIns",
                        agg_level = "year_locality",
                        Hill = TRUE,
                        richn_corr = TRUE,
                        digits = 2,
                        as_tibble = F){

  #Bind these variables to stop R CMD check complaints
  if(!exists("con")) {con <- NULL}

  checkCon()

  if(!is.null(subset_region)){
  subset_region <- match.arg(subset_region, choices = c("\u00d8stlandet", "Tr\u00f8ndelag"))
  }

  if(!is.null(subset_habitat)){
    subset_habitat <- match.arg(subset_habitat, choices = c("Forest", "Semi-nat"))
  }

  id_type <- match.arg(id_type, choices = c("metabarcoding"))
  dataset <- match.arg(dataset, choices = c("NasIns",
                                            "OkoTrond",
                                            "TidVar",
                                            "Nerlands\u00f8ya"))

  agg_level <- match.arg(agg_level, choices = c("year_locality",
                                                "locality_sampling",
                                                "region_habitat",
                                                "region_habitat_year",
                                                "total",
                                                "none"))

  trap_type <- match.arg(trap_type,
                         choices = c("All", "MF", "VF", NULL))


  ##Set up table sources
  ##Probably needs updating after new batch of data. Also need to test filtering of different identification types
  observations <- dplyr::tbl(con, dbplyr::in_schema("occurrences", "observations"))
  identifications <- dplyr::tbl(con, dbplyr::in_schema("events", "identifications"))
  sampling_trap <- dplyr::tbl(con, dbplyr::in_schema("events", "sampling_trap"))
  locality_sampling <- dplyr::tbl(con, dbplyr::in_schema("events", "locality_sampling"))
  year_locality <- dplyr::tbl(con, dbplyr::in_schema("events", "year_locality"))
  localities <- dplyr::tbl(con, dbplyr::in_schema("locations", "localities"))
  identification_techniques <- dplyr::tbl(con, dbplyr::in_schema("lookup", "identification_techniques"))
  traps <- dplyr::tbl(con, dbplyr::in_schema("locations", "traps"))

  ##Join the tables

  joined <- observations %>%
    left_join(identifications,
              by = c("identification_id" = "id"),
              suffix = c("_obs", "_ids")) %>%
    left_join(identification_techniques,
              by = c("identification_name" = "identification_name"),
              suffix = c("_obs", "_idtechn")) %>%
    left_join(sampling_trap,
              by = c("sampling_trap_id" = "id"),
              suffix = c("_obs", "_st")) %>%
    left_join(locality_sampling,
              by = c("locality_sampling_id" = "id"),
              suffix = c("_obs", "_ls")) %>%
    left_join(year_locality,
              by = c("year_locality_id" = "id"),
              suffix = c("_obs", "_yl")) %>%
    left_join(localities,
              by = c("locality_id" = "id"),
              suffix = c("_obs", "_loc"))  %>%
    left_join(traps,
              by = c("trap_id" = "id",
                     "year" = "year",
                     "locality" = "locality")
    ) %>%
    mutate(year = as.character(year))



  ##Exclude 2020 4 week samplings

  joined <-  joined %>%
    mutate(weeks_sampled = ifelse(grepl("2020", year) & (grepl("1", .data$trap_short_name) | grepl("3", trap_short_name)), 2, 4)) %>%
    mutate(weeks_sampled = ifelse(grepl("2020", year), .data$weeks_sampled, 2))

  joined <- joined %>%
    filter(weeks_sampled == 2)


  if(id_type == "metabarcoding"){
    joined <- joined %>%
      filter(identification_type == "metabarcoding")
  }

  #Filter on region name
  if(!is.null(subset_region)){
    subset_region <- c("", subset_region)
    joined <- joined %>%
      filter(region_name %in% subset_region)
  }

  #Filter on habitat
  if(!is.null(subset_habitat)){
    subset_habitat <- c("", subset_habitat)
    joined <- joined %>%
      filter(habitat_type %in% subset_habitat)
  }

  #Filter on order
  if(!is.null(subset_orders)){
    subset_orders <- c("", subset_orders) #To allow one-length subsets
    joined <- joined %>%
      filter(id_order %IN% subset_orders)
  }

  #Filter on families
  if(!is.null(subset_families)){
    subset_families <- c("", subset_families)
    joined <- joined %>%
      filter(id_family %in% subset_families)
  }

  #Filter on species
  if(!is.null(subset_species)){
    subset_species <- c("", subset_species)
    joined <- joined %>%
      filter(species_latin_fixed %in% subset_species)
  }

  #Filter on year
  if(!is.null(subset_year)){
    subset_year <- c("", subset_year)
    joined <- joined %>%
      filter(year %in% subset_year)
  }

  #Filter on genus
  if(!is.null(subset_genus)){
    subset_genus <- c("", subset_genus)
    joined <- joined %>%
      filter(id_genus %in% subset_genus)
  }

  #filter on dataset

  if(!is.null(dataset)){
    joined <- joined %>%
      filter(project_short_name == dataset)
  }

  #filter on trap type (recommended to only take MF)
  if(!is.null(trap_type) & trap_type != "All"){
    joined <- joined %>%
      filter(grepl((trap_type), sample_name))
  }


  ##Aggregate data to chosen level
  ##Add more choices?

  res <- joined


  ##This is slow because we have to collect the data before we calculate Shannon index.
  ##Best would be to do the Shannon calc on the database side. Seems harder than I first thought.
  if(agg_level == "year_locality"){

    res <- res %>%
      collect() %>%
      group_by(year_locality_id, locality_id, species_latin_fixed) %>% ##Error here, it collapses to species level, doesn't keep duplicate species
      summarise(no_asv_per_species = n_distinct(sequence_id),
                .groups = "keep") %>%
      group_by(year_locality_id, locality_id) %>%
      summarise(no_species = n_distinct(species_latin_fixed),
                shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
                mean_asv_per_species = round(mean(no_asv_per_species), digits),
                GDE_by_asv = round(calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr), digits),
                .groups = "keep") %>%
      left_join(localities,
                by = c("locality_id" = "id"),
                copy = T) %>%
      left_join(year_locality,
                by = c("year_locality_id" = "id",
                       "locality_id" = "locality_id",
                       "ano_flate_id" = "ano_flate_id",
                       "ssbid" = "ssbid"),
                copy = T) %>%
      ungroup() %>%
      select(year,
             locality,
             habitat_type,
             region_name,
             no_species,
             shannon_div,
             mean_asv_per_species,
             GDE_by_asv) %>%
      arrange(year,
              region_name,
              habitat_type,
              locality)

  }


  if(agg_level == "locality_sampling"){

    res <- res %>%
      collect() %>%
      group_by(start_date_obs, end_date_obs, sampling_name, year_locality_id, locality_id, species_latin_fixed) %>%
      summarise(no_asv_per_species = n_distinct(sequence_id),
                .groups = "keep") %>%
      group_by(sampling_name, year_locality_id, locality_id) %>%
      summarise(no_trap_days = mean(as.numeric(end_date_obs - start_date_obs)), ##to get the mean trap days from all traps within the sampling event (should be the same for all traps)
                no_species = n_distinct(species_latin_fixed),
                shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
                mean_asv_per_species = round(mean(no_asv_per_species), digits),
                GDE_by_asv = calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr),
                .groups = "keep") %>%
      left_join(localities,
                by = c("locality_id" = "id"),
                copy = T) %>%
      left_join(year_locality,
                by = c("year_locality_id" = "id"),
                copy = T) %>%
      ungroup() %>%
      select(year,
             locality,
             sampling_name,
             habitat_type,
             region_name,
             no_trap_days,
             no_species,
             shannon_div,
             mean_asv_per_species,
             GDE_by_asv) %>%
      arrange(year,
              region_name,
              habitat_type,
              locality,
              sampling_name)

  }

  if(agg_level == "region_habitat"){

    res <- res %>%
      collect() %>%
      group_by(region_name,
               habitat_type,
               species_latin_fixed) %>%
      summarise(no_asv_per_species = n_distinct(sequence_id),
                .groups = "keep") %>%
      group_by(region_name,
               habitat_type) %>%
      summarise(no_species = n_distinct(species_latin_fixed),
                shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
                mean_asv_per_species = round(mean(no_asv_per_species), digits),
                GDE_by_asv = calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr),
                .groups = "keep") %>%
      ungroup() %>%
      select(habitat_type,
             region_name,
             no_species,
             shannon_div,
             mean_asv_per_species,
             GDE_by_asv) %>%
      arrange(habitat_type,
              region_name)

  }


  if(agg_level == "region_habitat_year"){

    res <- res %>%
      collect() %>%
      group_by(region_name,
               habitat_type,
               year,
               species_latin_fixed) %>%
      summarise(no_asv_per_species = n_distinct(sequence_id),
                .groups = "keep") %>%
      group_by(region_name,
               habitat_type,
               year) %>%
      summarise(no_species = n_distinct(species_latin_fixed),
                shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
                mean_asv_per_species = round(mean(no_asv_per_species), digits),
                GDE_by_asv = calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr),
                .groups = "keep") %>%
      ungroup() %>%
      select(year,
             habitat_type,
             region_name,
             no_species,
             shannon_div,
             mean_asv_per_species,
             GDE_by_asv) %>%
      arrange(year,
              habitat_type,
              region_name
      )

  }

  if(agg_level == "total"){

    res <- res %>%
      collect() %>%
      group_by(species_latin_fixed) %>%
      summarise(no_asv_per_species = n_distinct(sequence_id),
                .groups = "keep") %>%
      summarise(no_species = n_distinct(species_latin_fixed),
                shannon_div = round(calc_shannon(species_latin_fixed, no_asv_per_species, Hill = Hill), digits),
                mean_asv_per_species = round(mean(no_asv_per_species), digits),
                GDE_by_asv = calc_GDE(no_asv_per_species, Hill = Hill, richn_corr = richn_corr),
                .groups = "keep") %>%
      ungroup() %>%
      select(no_species,
             shannon_div,
             mean_asv_per_species,
             GDE_by_asv)


  }



  if(!is.null(limit)){
    res <- joined %>%
      head(limit)
  }

  if(as_tibble){
    res <- res %>%
      as_tibble()
  }



  return(res)

}

