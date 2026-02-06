#' plot_year_region_stats
#'
#' Conveniance function for plotting the status of the project in the staggered experimental design
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' year_region_stats <- get_year_region_stats(last_year = 2025)
#'
#' plot_year_region_stats(year_region_stats)
#'
#'
#' }
#'

plot_year_region_stats <- function(year_region_stats) {
  # par(mar = rep(0, 4))

  raw_data <- year_region_stats

  fill_cols <- c(
    "Semi-nat" = "#E57200",
    "Skog" = "#7A9A01",
    "Ikke besøkt" = "white"
  )

  reg_cols <- tibble(
    region_name = c(
      "Sørlandet",
      "Østlandet",
      "Vestlandet",
      "Trøndelag",
      "Nord-Norge"
    ),
    color = c(
      "#E57200",
      "#008C95",
      "#7A9A01",
      "#93328E",
      "#004F71"
    )
  )

  plot_data <- raw_data %>%
    dplyr::group_by(region_name) %>%
    dplyr::mutate(year = as.integer(as.character(year))) %>%
    dplyr::mutate(group_id = cur_group_id()) |>
    dplyr::mutate(custom_y = (year %% 5 + 1) + ((group_id *5)-5)) %>%
    dplyr::mutate(
      custom_x = ifelse(habitat_type == "Semi-nat",
                        as.integer(as.character(year)) - 0.2,
                        as.integer(as.character(year)) + 0.2
      ),
      visited = ifelse(visits > 0, "Ja", "Nei")
    )


  yline_pos <- tibble(hline = seq(0,
                                  25,
                                  by = 5
                                  ) + 0.5)


  ytext_pos <- tibble(ytext = seq(0,
                                  20,
                                  by = 5
                                  ) + (6) / 2)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = custom_x,
      y = custom_y
    )
  ) +
    ggplot2::geom_hline(aes(yintercept = hline),
               lty = 3,
               data = yline_pos
    ) +
    ggplot2::geom_tile(
      ggplot2::aes(
        fill = visited,
        color = habitat_type
      ),
      width = .3,
      height = .9,
      lwd = 1
    ) +
    ggplot2::scale_fill_manual(
      name = "Registrert",
      values = c("black", "white")
    ) +
    ggplot2::scale_color_manual(
      name = "Habitattype",
      values = fill_cols,
      aesthetics = "colour"
    ) +
    ggplot2::ylab("") +
    ggplot2::scale_x_continuous(
      name = "År",
      breaks = unique(plot_data$year)
    ) +
    ggplot2::scale_y_continuous(
      breaks = ytext_pos$ytext,
      labels = c(
        "<b style='color:#E57200'>Sørlandet</b>",
        "<b style='color:#008C95'>Østlandet</b>",
        "<b style='color:#7A9A01'>Vestlandet</b>",
        "<b style='color:#93328E'>Trøndelag</b>",
        "<b style='color:#004F71'>Nord-Norge</b>"
      )
    ) +
    ggplot2::theme(
      panel.background = element_blank(),
      axis.text.y = ggtext::element_markdown(),
      plot.margin = margin(0, 0, 0, 0, "cm")
    )

  p
}



