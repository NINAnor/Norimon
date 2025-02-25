#' Summary plot of ano herb records
#'
#' Convenience function to clear up some code in reports
#'
#' @return A ggplot
#' @export
#'
#' @examples
#' \dontrun{
#'
#' plot_ano_herb_sum()
#' }
#'
plot_ano_herb_sum <- function() {
  Norimon::checkCon()

  ano_herb_agg <- dplyr::tbl(
    con,
    DBI::Id(
      schema = "views",
      table = "ano_herb_agg"
    )
  ) %>%
    dplyr::filter(project_short_name == "NorIns")


  loc_reg <- get_localities(dataset = "NorIns") %>%
    dplyr::mutate(habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type)) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      locality,
      region_name
    )

  xlim <- ano_herb_agg %>%
    dplyr::summarise(
      min = min(year, na.rm = TRUE),
      max = max(year, na.rm = TRUE)
    ) %>%
    dplyr::collect() %>%
    as.vector()

  ano_herb_agg %>%
    dplyr::left_join(loc_reg,
      by = c("locality" = "locality"),
      copy = TRUE
    ) %>%
    dplyr::group_by(region_name) %>%
    ggplot2::ggplot(.) +
    ggplot2::geom_point(
      aes(
        x = year,
        y = ano_median_cover,
        size = ano_median_no_spec,
        col = region_name
      ),
      alpha = 0.7,
      position = position_jitterdodge(
        dodge.width = 0.4,
        jitter.height = 2,
        jitter.width = 0
      )
    ) +
    ggplot2::scale_x_continuous(breaks = seq(xlim[[1]], xlim[[2]])) +
    NinaR::scale_color_nina(name = "Region") +
    ggplot2::scale_size(name = "Artsantall\nkarplanter") +
    ggplot2::guides(color = guide_legend(override.aes = list(size = 4))) +
    ggplot2::theme(
      legend.title = element_text(size = 8),
      legend.margin = margin(c(0, 0, 0, 0), unit = "pt")
    ) +
    ggplot2::xlab("\u00C5r") +
    ggplot2::ylab("Dekningsgrad %")
}
