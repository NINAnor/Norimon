#' ts_plot Plot a time series plot of indicator values
#'
#'
#' @param x A boot_stat object
#' @param palette The palette to be used in scale_fill_nina and scale_color_nina
#' @param ... Additional parameters passed to scale_fill_nina. e.g. palette
#'
#' @return a ggplot
#' @export
ts_plot <- function(x,
                    palette = "blue-orange",
                    ...) {
  UseMethod("ts_plot", x)
}

#' @export
ts_plot.boot_stat <- function(x,
                              palette = "blue-orange",
                              ...) {
  if (!any(class(x) == "boot_stat")) stop("Input must be of class 'boot_stat'")


  df <- x[[1]] %>%
    tidyr::as_tibble() %>%
    mutate(across(starts_with("year"), ~ as.factor(.)))

  y_value_name <- stringr::str_replace(stringr::str_to_sentence(attr(x, "value_name")), "_", " ")
  x_value_name <- paste0(stringr::str_to_sentence(attr(x, "temporal_resolution")), " window")


  ggplot(df) +
    geom_ribbon(
      aes(
        x = year,
        ymin = boot_lower2.5,
        ymax = boot_upper97.5,
        group = region_name,
        fill = region_name
      ),
      alpha = 0.5
    ) +
    geom_point(aes(
      x = year,
      y = boot_value,
      color = region_name,
      group = region_name
    )) +
    geom_line(aes(
      x = year,
      y = boot_value,
      color = region_name,
      group = region_name
    )) +
    NinaR::scale_color_nina() +
    NinaR::scale_fill_nina(name = "Region name") +
    ylab(y_value_name) +
    xlab(x_value_name) +
    guides(color = FALSE)
}
