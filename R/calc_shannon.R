#' Calculate Shannon index on a community
#'
#' @param community Community.
#' @return Shannon diversity.
#' @examples
#' @Imports dplyr
#'



calc_shannon <- function(community) {
  p <- table(community)/length(community) # Find proportions
  p <- p[p > 0] # Get rid of zero proportions (log zero is undefined)
  -sum(p * log(p)) # Calculate index
}
