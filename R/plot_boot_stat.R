#' Plot a boot_stat object
#'
#' @param x boot_stat_object
#' @param ... additional parameters passed to geom_density_ridges_gradient, e.g. bandwidth
#'
#' @return
#' @export
#'
#' @examples
plot.boot_stat <- function(x, ...){

  if(!any(class(x) == "boot_stat")) stop("Input must be of class 'boot_stat'")

  df <- x[[2]] %>%
    as_tibble

  df <- df %>%
    mutate(across(!boot_values, as.factor))


  ##!! Set up automatic and choice of groups (y) and facets

    p <- ggplot(df,
         aes(x = boot_values, y = year, fill = stat(x))) +
    geom_density_ridges_gradient(...) +
    scale_fill_nina(name = "boot_values",
                    discrete = F)  +
    facet_wrap("region_name",scales = "fixed")


  p

}
