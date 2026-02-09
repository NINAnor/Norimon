#' plot_country_map
#'
#' @param map Map input, typically form Norimon::get_map()
#' @param style What style to plot, currently only "regions" implemented
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' nor <- get_map()
#'
#' plot_country_map(nor)
#'
#' }
#'


plot_country_map <- function(map,
                             style = "regions") {
  # par(mar = rep(0, 4))

  if(style == "regions"){

  p <- ggplot(map) +
    geom_sf(aes(fill = region)) +
    NinaR::scale_fill_nina(name = "") +
    guides(fill = "none") +
    theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
    ggthemes::theme_map()
  } else return(NULL)

  p

}
