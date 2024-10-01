#' Bootstrap functions for calculating mean and lower and upper bounds. Not usually used.
#'
#' @param x Input vector of values to bootstrap
#' @param limit Confidence limit. e.g. 0.025 for lower 2.5%, and 0.975 for upper 97.5%
#' @param R Number of bootstrap samples. Default = 999
#'
#' @return Returns a vector of bootstrapped values
#' @export
#'
#' @examples
#' \dontrun{
#'
#' shannon_beetles %>%
#'   group_by(year) %>%
#'   summarise(
#'     boot_mean = boot_mean(shannon_div),
#'     boot_lower25 = boot_lower(shannon_div),
#'     boot_upper975 = boot_upper(shannon_div)
#'   )
#' }
#'
#' @export
#' @rdname boot_mean


boot_lower <- function(x,
                       limit = 0.025,
                       R = 999) {
  temp <- replicate(
    R,
    mean(sample(x,
      size = length(x),
      replace = TRUE
    ))
  )

  lower <- nth(temp, floor(R * limit), order_by = temp)

  return(lower)
}

#' @export
#' @rdname boot_mean

boot_upper <- function(x,
                       limit = 0.975,
                       R = 999) {
  temp <- replicate(
    R,
    mean(sample(x,
      size = length(x),
      replace = TRUE
    ))
  )

  upper <- nth(temp, ceiling(R * limit), order_by = temp)

  return(upper)
}
