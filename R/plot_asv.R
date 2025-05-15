#' plot_asv
#'
#' @param species Character string of latin species name to plot
#' @param background relative path to a background image.
#' @param pie_scale scaling factor relative to map size (how large should the pies be) 0-1
#' @param size another (?) sizing
#' @param caption Character for caption, or TRUE/FALSE for no or default species as caption.
#' @param title Optional title. Null or character.
#' @param scale_to_sum_reads Should slices be scaled to the sum of the reads? Boolean
#' @param subset_years Optional subset on years (e.g. 2020:2024 or character vector of years)
#' @param ...
#'
#' @return a ggplot object
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' plot_asv("Erebia ligea") #'
#' }


plot_asv <- function(species = NULL,
                     background = NULL,
                     pie_scale = 0.5,
                     size = 0.1,
                     caption = TRUE,
                     title = NULL,
                     scale_to_sum_reads = TRUE,
                     subset_years = NULL,
                     ...) {

  jon_asv <- get_asv_loc(species = species) %>%
    dplyr::mutate(scale_sum_reads = scale(sum_reads, center = min(sum_reads), scale = diff(range(sum_reads))))
  # %>%
  #  filter(locality %in% c("Skog_02", "Semi-nat_11"))


  if(is.null(background)){
    background <- system.file("figures", "hele_norge.png", package = "Norimon")
  }
  if(!file.exists(background)) stop("Background image file not found")

  if (!is.null(subset_years)) {
    subset_years <- as.numeric(subset_years)
  }

  tt <- terra::rast(background)
  ext_background <- as.vector(terra::ext(tt))
  img <- png::readPNG(background)
  g <- grid::rasterGrob(img, interpolate = TRUE)

  if (scale_to_sum_reads) {
    jon_asv$r <- log(jon_asv$sum_reads) * diff(ext_background[1:2]) / 100 * pie_scale
  } else {
    jon_asv$r <- diff(ext_background[1:2]) / 100 * pie_scale
  }

  jon_asv <- jon_asv %>%
    dplyr::mutate(locality = factor(locality, levels = unique(locality[order(dplyr::desc(r))]))) %>%
    dplyr::arrange(dplyr::desc(r))

  if (!is.null(subset_years)) {
    subset_years <- c(NA, subset_years) # To allow one-length subsets
    jon_asv <- jon_asv %>%
      dplyr::filter(year %in% subset_years)
  }


  p1 <- ggplot() +
    annotation_custom(g,
      xmin = ext_background[1],
      xmax = ext_background[2],
      ymin = ext_background[3],
      ymax = ext_background[4]
    ) +
    lapply(
      split(jon_asv, jon_asv$locality),
      function(d) {
        ggforce::geom_arc_bar(
          aes(
            x0 = x_25833,
            y0 = y_25833,
            r = r,
            r0 = 0,
            amount = perc_reads,
            fill = sequence_id
          ),
          size = size,
          data = d,
          stat = "pie",
          inherit.aes = TRUE
        )
      }
    ) +
    coord_fixed() +
    xlim(ext_background[1:2]) +
    ylim(ext_background[3:4]) +
    theme(
      legend.position = "none",
      panel.border = element_blank(),
      # axis.line=element_line(color = "black"),
      axis.text.x = element_blank(), # remove x axis labels
      axis.ticks.x = element_blank(), # remove x axis ticks
      axis.text.y = element_blank(), # remove y axis labels
      axis.ticks.y = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "pt"),
      plot.caption.position = "panel",
      plot.caption = element_text(
        vjust = 10,
        hjust = 0.5,
        size = 14,
        margin = margin(0, 0, 0, 0)
      ),
      plot.title = element_text(
        hjust = 0.5,
        vjust = 0
      )
    ) +
    xlab("") +
    ylab("") +
    NinaR::scale_fill_nina()

  if (caption != FALSE) {
    if(caption == TRUE){
    p1 <- p1 +
      labs(caption = species)
    } else {
      p1 <- p1 +
        labs(caption = caption)
    }
  }

  if (!is.null(title)) {
    p1 <- p1 +
      labs(title = title)
  }

  p1
}
