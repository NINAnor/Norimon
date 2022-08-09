#' weights_from_db Get biomass data from the database
#'
#' @param limit Optional row limit on output (for testing)
#' @param trap_type Which trap types to fetch data for. "All" (defult), "MF", or "VF".
#' @param subset_region Optional subset of region. Currently allowing "Østlandet", "Trøndelag", "Sørlandet"
#' @param dataset Which dataset to fetch data for. Default "NasIns".
#' @param agg_level Aggregate level of data. "year_locality" (default), "locality_sampling", "total".
#' @param as_tibble Return as tibble? Boolean
#'
#' @return A tibble of sample weights
#' @export
#'
#' @examples
#'
#' weights_from_db()
#'
#'

weights_from_db <- function(limit = NULL,
                            trap_type = c("MF", "VF", "ALL"),
                            subset_region = c(NULL, "Østlandet", "Trøndelag", "Sørlandet"),
                            dataset = c("NasIns"),
                            agg_level = c("year_locality",
                                          "locality_sampling",
                                          "total"),
                            as_tibble = F){

  Norimon:::checkCon()



  dataset <- match.arg(dataset,
                       choices = c("NasIns",
                                   "OkoTrond",
                                   "TidVar",
                                   "Nerlandsøya"))

  agg_level <- match.arg(agg_level,
                         choices = c("year_locality",
                                     "locality_sampling",
                                     "total",
                                     "none"))

  trap_type <- match.arg(trap_type,
                         choices = c("ALL", "MF", "VF", NULL))


  ##Set up table sources
  ##Probably needs updating after new batch of data. Also need to test filtering of different identification types
  sampling_trap <- dplyr::tbl(con, dbplyr::in_schema("events", "sampling_trap"))
  locality_sampling <- dplyr::tbl(con, dbplyr::in_schema("events", "locality_sampling"))
  year_locality <- dplyr::tbl(con, dbplyr::in_schema("events", "year_locality"))
  localities <- dplyr::tbl(con, dbplyr::in_schema("locations", "localities"))
  traps <- dplyr::tbl(con, dbplyr::in_schema("locations", "traps"))


  ##Join the tables

  joined <- sampling_trap %>%
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
                     "year" = "year")
    ) %>%
    mutate(year = as.character(year))


  ##Exclude 2020 4 week samplings

  joined <-  joined %>%
    mutate(weeks_sampled = ifelse(grepl("2020", year) & (grepl("1", trap_short_name) | grepl("3", trap_short_name)), 2, 4)) %>%
    mutate(weeks_sampled = ifelse(grepl("2020", year), weeks_sampled, 2))

  joined <- joined %>%
    filter(weeks_sampled == 2)

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

  #Filter on region name
  if(!is.null(subset_region)){
    subset_region <- c("", subset_region)
    joined <- joined %>%
      filter(region_name %IN% subset_region)
  }

  ##Aggregate data to choosen level
  ##Add more choices?

  res <- joined



  ##This is slow because we have to collect the data before we calculate Shannon index.
  ##Best would be to do the Shannon calc on the database side. Seems harder than I first thought.
  if(agg_level == "year_locality"){

    res <- res %>%
      collect() %>%
      group_by(year_locality_id, locality_id) %>%
      summarise(sum_wet_weight = sum(wet_weight_bottle - weight_empty_bottle, na.rm = T),
                avg_wet_weight = mean(wet_weight_bottle - weight_empty_bottle, na.rm = T)) %>%
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
             sum_wet_weight,
             avg_wet_weight) %>%
      arrange(year,
              region_name,
              habitat_type,
              locality)

  }


  if(agg_level == "locality_sampling"){

    res <- res %>%
      collect() %>%
      group_by(sampling_name, year_locality_id, locality_id) %>%
      summarise(no_trap_days = mean(as.numeric(end_date_obs - start_date_obs)),
                sum_wet_weight = sum(wet_weight_bottle - weight_empty_bottle, na.rm = T),
                avg_wet_weight = mean(wet_weight_bottle - weight_empty_bottle, na.rm = T)) %>%
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
             sum_wet_weight,
             avg_wet_weight) %>%
      arrange(year,
              region_name,
              habitat_type,
              locality,
              sampling_name)

  }



  if(agg_level == "total"){

    res <- res %>%
      collect() %>%
      summarise(sum_wet_weight = sum(wet_weight_bottle - weight_empty_bottle, na.rm = T),
                avg_wet_weight = mean(wet_weight_bottle - weight_empty_bottle, na.rm = T)) %>%
      select(sum_wet_weight,
             avg_wet_weight)

  }

  if(!is.null(limit)){
    res <- joined %>%
      head(limit)
  }


  res <- res %>%
    mutate(sum_wet_weight = round(sum_wet_weight, 2),
           avg_wet_weight = round(avg_wet_weight, 2))

  if(as_tibble){
    res <- res %>%
      as_tibble()
  }



  return(res)

}


