#' Bootstrap functions for calculating mean and lower and upper bounds. Not usually used.
#'
#' @param x Input vector of values to bootstrap
#' @param R Number of bootstrap samples. Default = 999
#'
#' @return Returns a vector of bootstrapped values
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' shannon_beetles %>%
#' group_by(year) %>%
#'   summarise(boot_mean = boot_mean(shannon_div),
#'             boot_lower25 = boot_lower(shannon_div),
#'             boot_upper975 = boot_upper(shannon_div))
#'
#' }
#'
#'


boot_mean <- function(x,
                      R = 999) {
  mean(replicate(R,
                 mean(sample(x,
                             size = length(x),
                             replace = TRUE)
                 )
  )
  )
}


