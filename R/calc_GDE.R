#' Calculate Genetic diversity evenness on a community.
#'
#' Experimental!
#'
#' @param GD A GDM calculation. (doi: https://doi.org/10.1101/2022.02.09.479762)
#' @param Hill Return as first order Hill number. Boolean
#' @param richn_corr Correct for species richness, dividing by number of species. Boolean
#'
#' @return Shannon diversity.
#' @examples
#' \dontrun{
#' beetles_2021 <- get_observations(
#'   subset_orders = "Coleoptera",
#'   subset_year = 2021,
#'   agg_level = "none"
#' )
#'
#'
#' shannon_beetles_2021 <- beetles_2021 %>%
#'   collect() %>%
#'   group_by(locality) %>%
#'   select(
#'     locality,
#'     no_asv_per_species
#'   ) %>%
#'   summarise(shannon_div = calc_GDE(species_latin))
#' }
#' @import dplyr
#'
#' @export
#'



calc_GDE <- function(GD,
                     Hill = TRUE,
                     richn_corr = TRUE) {
  # Temporary hack to get the (wrong, temporary) gdms below zero
  GDM_temp <- GD / sum(GD)

  out <- -sum(GDM_temp * log(GDM_temp)) # Calculate index

  if (Hill) {
    out <- exp(out)
  }

  if (richn_corr) {
    out <- out / length(GD)
  }

  return(out)
}
