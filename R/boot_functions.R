#' Bootstrap functions for calculating mean and lower and upper bounds
#'
#' @param x Input vector of values to bootstrap
#' @param limit Confidence limit. e.g. 0.025 for lower 2.5%, and 0.975 for upper 97.5%
#' @param R Number of bootstrap samples. Default = 999
#'
#' @return
#' @export
#'
#' @examples




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


#' @rdname boot_mean
#' @export
  boot_lower <- function(x,
                         limit = 0.025,
                         R = 999){
    temp <- replicate(R,
                      mean(sample(x,
                                  size = length(x),
                                  replace = TRUE)
                           )
                      )

    lower = nth(temp, floor(R * limit), order_by = temp)

    return(lower)
  }

#' @rdname boot_mean
#' @export
  boot_upper <- function(x,
                         limit = 0.975,
                         R = 999){
    temp <- replicate(R,
                      mean(sample(x,
                                  size = length(x),
                                  replace = TRUE)
                           )
                      )

    upper = nth(temp, ceiling(R * limit), order_by = temp)

    return(upper)
  }
