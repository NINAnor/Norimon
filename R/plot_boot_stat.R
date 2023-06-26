#' Plot a boot_stat object
#'
#' @param x boot_stat_object
#' @param palette named nina-palette (see NinaR::nina_palettes)
#' @param ... additional parameters passed to geom_density_ridges_gradient, e.g. bandwidth
#'
#' @return A ggridges plot of the bootrapped values in a boot_stat object.
#' @export
#'
#' @import ggplot2
#'
#' @examples
#'
#'\dontrun{
#'
#'
#'  beetles <- get_observations(subset_orders = "Coleoptera",
#'                       agg_level = "year_locality")
#'
#'
#'
#' beetle_shannon_boot <- bootstrap_value(beetles,
#'                                        value = shannon_div,
#'                                        groups = c("year",
#'                                                   "region_name"))
#'
#' plot(beetle_shannon_boot)
#' }
#'

plot.boot_stat <- function(x,
                           palette = c("blue-orange"),
                           ...){

    if(!any(class(x) == "boot_stat")) stop("Input must be of class 'boot_stat'")

    df <- x[[2]] %>%
      as_tibble

    df <- df %>%
      mutate(across(!boot_values, as.factor))

    cols <- colnames(df)[(colnames(df) != "boot_values")]
    x_axis_name <- stringr::str_to_sentence(attr(x, "value_name"))
    y_axis_name <- stringr::str_to_sentence("year")

    if(length(cols) > 1){
    p <- ggplot(df,
                aes(x = boot_values, y = year, fill = after_stat(x))) +
      ggridges::geom_density_ridges_gradient() +
      NinaR::scale_fill_nina(name = x_axis_name,
                      discrete = F,
                      palette = palette,
                      ...)  +
      xlab(x_axis_name) +
      ylab(y_axis_name) +
      facet_wrap(cols[2], scales = "fixed") +
      scale_y_discrete(limits=rev)
    } else {

      p <- ggplot(df,
                  aes(x = boot_values, y = year, fill = stat(x))) +
        ggridges::geom_density_ridges_gradient() +
        NinaR::scale_fill_nina(name = x_axis_name,
                        discrete = F,
                        palette = palette,
                        ...) +
        xlab(x_axis_name) +
        ylab(y_axis_name) +
        scale_y_discrete(limits=rev)

    }


  suppressMessages({
    print(p)
  })


}


