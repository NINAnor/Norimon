#' Calculate Shannon index on a community.
#'
#' @param community Community.
#' @param no_asv_per_species Number of genetic variants per species.
#' @param augment_community Build up community by from species and number of occurences (TRUE), or use raw occurrences (FALSE)
#' @param only_sum_freq Only sum the frequencies together, instead of calculating Shannon information.
#' @param Hill Return as first order Hill number. Boolean
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
#'         species_latin) %>%
#'   summarise(shannon_div = calc_shannon(species_latin))
#'
#'
#' }
#' @import dplyr
#'
#' @export
#'



calc_shannon <- function(community,
                         no_asv_per_species,
                         augment_community = TRUE,
                         only_sum_freq = FALSE,
                         Hill = TRUE) {
    if(augment_community){
    community_aug <- rep(community, no_asv_per_species)
    p <- table(community_aug)/length(community_aug) # Find proportions
    } else{
      p <- community
    }

    p <- p[p > 0] # Get rid of zero proportions (log zero is undefined)
    if(only_sum_freq){
      out <- sum(p)
    } else {
    out <- -sum(p * log(p)) # Calculate index
    }

    if(Hill){
      out <- exp(out)
    }

    return(out)

}

