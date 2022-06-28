#' community_matrix_from_db
#'
#' @param limit
#' @param id_type
#' @param trap_type
#' @param dataset
#' @param subset_orders
#' @param subset_families
#' @param subset_species
#' @param subset_habitat
#' @param subset_region
#' @param exclude_singletons
#' @param transposed_matrix
#' @param as_tibble
#'
#' @return
#' @export
#'
#' @examples
community_matrix_from_db <- function(limit = NULL,
                                     id_type = c("metabarcoding"),
                                     trap_type = c("MF", "VF", "All", NULL),
                                     dataset = c("NasIns"),
                                     subset_orders = NULL,
                                     subset_families = NULL,
                                     subset_species = NULL,
                                     subset_habitat = NULL,
                                     subset_region = c(NULL, "Østlandet", "Trøndelag"),
                                     exclude_singletons = F,
                                     transposed_matrix = F,
                                     as_tibble = F){

  checkCon()


  dataset <- match.arg(dataset,
                       choices = c("NasIns",
                                   "OkoTrond",
                                   "TidVar",
                                   "Nerlandsøya"))

  trap_type <- match.arg(trap_type,
                         choices = c("MF", "VF", "All", NULL))


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
              by = c("identification_name", "identification_name"),
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
              suffix = c("_obs", "_loc")) %>%
    left_join(traps,
              by = c("trap_id" = "id",
                     "year" = "year",
                     "locality" = "locality"))



  if(id_type == "metabarcoding"){
    joined <- joined %>%
      filter(identification_type == "metabarcoding")
  }

  if(!is.null(subset_orders)){
    subset_orders <- c("", subset_orders) #To allow one-length subsets
    joined <- joined %>%
      filter(id_order %IN% subset_orders)
  }

  if(!is.null(subset_families)){
    subset_families <- c("", subset_families)
    joined <- joined %>%
      filter(id_family %IN% subset_families)
  }

  if(!is.null(subset_species)){
    subset_species <- c("", subset_species)
    joined <- joined %>%
      filter(species_latin_fixed %IN% subset_species)
  }

  #Filter on region name
  if(!is.null(subset_region)){
    subset_region <- c("", subset_region)
    joined <- joined %>%
      filter(region_name %IN% subset_region)
  }

  #Filter on habitat type

  if(!is.null(subset_habitat)){
    subset_habitat <- c("", subset_habitat)
    joined <- joined %>%
      filter(habitat_type %IN% subset_habitat)
  }



  #filter on dataset

  if(!is.null(dataset)){
    joined <- joined %>%
      filter(project_short_name == dataset)
  }



  ##Aggregate data to choosen level


  ##Exclude 2020 4 week samplings

  joined <-  joined %>%
    mutate(year = as.character(year)) %>%
    mutate(weeks_sampled = ifelse(grepl("2020", year) & (grepl("1", trap_short_name) | grepl("3", trap_short_name)), 2, 4)) %>%
    mutate(weeks_sampled = ifelse(grepl("2020", year), weeks_sampled, 2))

  joined <- joined %>%
    filter(weeks_sampled == 2)

  #filter on dataset

  #filter on trap type (recommended to only take MF)
  if(!is.null(trap_type) & trap_type != "All"){
    joined <- joined %>%
      filter(grepl((trap_type), sample_name))
  }

  ##Aggregate data to choosen level
  ##Add more choices!

  res <- joined

  res <- res %>%
    collect() %>%
    select(year,
           locality,
           species_latin_fixed) %>%
    group_by(year,
             locality,
             species_latin_fixed) %>%
    summarise(count = n()) %>%
    mutate(present = 1)

  if(exclude_singletons){ ##exlude only species that when observed, was observed more than once each time
    to_exclude <- res %>%
      filter(count == 1) %>%
      select(species_latin_fixed) %>%
      pull()

    res <- res %>%
      filter(!(species_latin_fixed %in% to_exclude))
  }



  res <- res %>%
    select(-count) %>%
    pivot_wider(names_from = species_latin_fixed,
                values_from = present,
                values_fill = 0) %>%
    arrange(year,
            locality)




  if(!is.null(limit)){
    res <- joined %>%
      head(limit)
  }

  if(as_tibble){
    res <- res %>%
      as_tibble()
  }


  if(transposed_matrix){
    res <- res %>%
      select(-c(1:2)) %>%
      t()

  }



  return(res)

}

