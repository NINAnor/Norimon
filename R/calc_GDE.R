#' Calculate Genedic diversity evenness on a community.
#'
#' Experimental!
#'
#' @param community Community.
#' @param Hill Return as first order Hill number. Boolean
#' @param richn_corr Correct for species richness, dividing by number of species. Boolean
#'
#' @return Shannon diversity.
#' @examples
#'
#'
#' \dontrun{
#'    beetles_2021 <- get_observations(subset_orders = "Coleoptera",
#'    subset_year = 2021,
#'    agg_level = "none")
#'
#'
#'   shannon_beetles_2021 <- beetles_2021 %>%
#'   collect() %>%
#'   group_by(locality) %>%
#'   select(locality,
#'         no_asv_per_species) %>%
#'   summarise(shannon_div = calc_GDE(species_latin))
#'
#'
#' }
#' @import dplyr
#'
#' @export
#'



calc_GDE <- function(gdm,
                     Hill = TRUE,
                     richn_corr = TRUE) {

    #Temporary hack to get the (wrong, temporary) gdms below zero
    gdm_temp <- gdm / sum(gdm)

    out <- -sum(gdm_temp * log(gdm_temp)) # Calculate index

    if(Hill){
      out <- exp(out)
    }

    if(richn_corr){
      out <- out / length(gdm)
    }

    return(out)

}

