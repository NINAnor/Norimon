#' Phen_peak_plot
#' Plot the peaks of a sinusiod phenology model output.
#'
#' @param obj a gllvm sinusoid model.
#' @param mark_peaks Mark peaks? boolean
#' @param pred_time x-axis length, prediction interval. Defaults to 365 days.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' phen_peak_plot(gllvm_phen_model)
#'
#' }
#'
phen_peak_plot <- function(obj,
                           mark_peaks = TRUE,
                           pred_time = 1:365){

  x <- coef(obj)
  val_names = names(x$Intercept)

  pred <- matrix(ncol = length(val_names) , nrow = length(pred_time))
  colnames(pred) <- val_names

  for(i in val_names){
    pred[, i] <- x$Intercept[i] + x$Xcoef[i, 1] * cos((2*pi/max(pred_time)) * pred_time) +  x$Xcoef[i, 2] * sin((2*pi/max(pred_time)) * pred_time)

  }
  pred <- cbind(pred_time, pred)

  to_plot <- pivot_longer(as_tibble(pred),
                          cols= 2:ncol(pred)) |>
    mutate(date = as.Date(pred_time - 1, origin = "2020-01-01"))

  p <- ggplot(to_plot) +
    geom_line(aes(x = date,
                  y = value,
                  color = name)) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b",
                 name = "M\u00e5ned")

  if(mark_peaks){
    peak_x <- atan2(x$Xcoef[, 2], x$Xcoef[, 1]) / (2 * pi / max(pred_time))
    peak_y <- x$Intercept + sqrt((x$Xcoef[, 2])^2 + (x$Xcoef[, 1])^2)

    peaks <- tibble(name = names(peak_x),
                    peak_x = peak_x,
                    peak_y = peak_y)

    peaks <- peaks |>
      mutate(peak_x = ifelse(peak_x < 0, max(pred_time) + peak_x, peak_x)) |>
      mutate(date = as.Date(peak_x - 1, origin = "2020-01-01"))

    plot_info <- ggplot_build(p)
    y_range <- plot_info$layout$panel_params[[1]]$y.range
    min_y <- y_range[1]
    max_y <- y_range[2]

    p <- p +
      geom_segment(aes(x = date,
                       xend = date,
                       y = min_y,
                       yend = peak_y,
                       col = name),
                   lty = 2,
                   data = peaks)
  }

  return(p)

}
