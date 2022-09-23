#' Calculate Shannon index on a community.
#'
#' @param community Community.
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
#'         species_latin) %>%
#'   summarise(shannon_div = calc_shannon(species_latin))
#'
#'
#' }
#' @import dplyr
#'
#' @export
#'



calc_shannon <- function(community) {
  p <- table(community)/length(community) # Find proportions
  p <- p[p > 0] # Get rid of zero proportions (log zero is undefined)
  -sum(p * log(p)) # Calculate index
}
