#' Plot a phenology object
#'
#' @param x a phenology object
#' @param y_value What y_value to plot (taxa_biomass, no_species)
#' @param x_axis_type What x type to use (date, sampling number)
#' @param scale_to_max Scale each taxa within hear btw 1-100. Boolean. Default = TRUE
#' @param hide_legend Boolean. Default = FALSE
#'
#' @param ... additional parameters, not currently implemented
#'
#' @return a ggplot of the phenology
#' @export
#'
#' @examples
#' \dontrun{
#'
#' order_phen <- get_phenology(taxonomic_level = "Order")
#' plot(order_phen)
#' }
#'



plot.phenology <- function(x,
                           y_value = "taxa_biomass",
                           x_axis_type = "sampling_number",
                           aggregation = "Smooth",
                           scale_to_max = TRUE,
                           hide_legend = FALSE,
                           ...) {
  if (!"phenology" %in% class(x)) {
    stop("Input object must be of class 'phenology'")
  }

  id_group <- attr(x, "taxonomic_level")

  x_axis_type <- match.arg(x_axis_type,
                           choices = c("sampling_number",
                                       "date",
                                       "temperature_sum"))

  if (id_group == "id_order") {
    subset_orders <- c(
      "Diptera", "Coleoptera", "Hymenoptera",
      "Hemiptera", "Lepidoptera"
    )

    x <- x %>%
      ungroup() %>%
      filter(id_order %in% subset_orders) %>%
      mutate(sampling_number = as.integer(sampling_number))

    if(x_axis_type == "sampling_number"){

      x <- x %>%
        group_by(year, sampling_number, id_order) %>%
        summarise(taxa_biomass = mean(taxa_biomass, na.rm = TRUE),
                  .groups = "keep")


      p <- ggplot(x) +
        geom_path(aes(
          x = sampling_number,
          y = taxa_biomass,
          color = id_order
        ), lwd = 2) +
        facet_grid(
          rows = vars(year),
          cols = NULL,
          scales = "fixed"
        ) +
        scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
        scale_color_nina(name = "Orden") +
        ylab("Relative biomasse (g.)") +
        xlab("Samplenummer")

    }

    if(x_axis_type == "date"){

      if(scale_to_max){

        x <- x |>
          group_by(year, locality, id_order) |>
          mutate(taxa_biomass = scales::rescale(taxa_biomass,
                                                to = c(0, 100))
          )
      }

      x <- x %>%
        group_by(year, end_date_obs, id_order)
      #%>%
      # summarise(taxa_biomass = mean(taxa_biomass, na.rm = TRUE),
      #          .groups = "keep")

      p <- ggplot(x) +
        geom_smooth(aes(
          x = lubridate::yday(end_date_obs),
          y = taxa_biomass,
          color = id_order
        ),
        lwd = 2) +
        facet_grid(
          rows = vars(year),
          cols = NULL,
          scales = "fixed"
        ) +
        # scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
        ylim(c(0, max(x$taxa_biomass))) +
        scale_color_nina(name = "Orden") +
        ylab("Biomasse (g.)") +
        xlab("Samplingdato")

    }

    if(x_axis_type == "temperature_sum"){

      if(scale_to_max){

        x <- x |>
          group_by(year, locality, id_order) |>
          mutate(taxa_biomass = scales::rescale(taxa_biomass,
                                                to = c(0, 100))
          )
      }

      x <- x %>%
        group_by(year, end_date_obs, id_order)
      #%>%
      # summarise(taxa_biomass = mean(taxa_biomass, na.rm = TRUE),
      #          .groups = "keep")

      p <- ggplot(x) +
        geom_smooth(aes(
          x = csum,
          y = taxa_biomass,
          color = id_order
        ),
        lwd = 2) +
        facet_grid(
          rows = vars(year),
          cols = NULL,
          scales = "fixed"
        ) +
        # scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
        ylim(c(0, max(x$taxa_biomass))) +
        scale_color_nina(name = "Orden") +
        ylab("Biomasse (g.)") +
        xlab("Temperatursum")


    }


  }

  if (id_group == "id_family") {

    x <- x %>%
      ungroup() %>%
      mutate(sampling_number = as.integer(sampling_number))

    if(x_axis_type == "sampling_number"){

      x <- x %>%
        group_by(year, sampling_number, id_family) %>%
        summarise(taxa_biomass = mean(taxa_biomass, na.rm = TRUE))

      p <- ggplot(x) +
        geom_path(aes(
          x = sampling_number,
          y = taxa_biomass,
          color = id_family
        ), lwd = 2) +
        facet_grid(
          rows = vars(year),
          cols = NULL,
          scales = "fixed"
        ) +
        scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
        scale_color_nina(name = "Familie") +
        ylab("Biomasse (g.)") +
        xlab("Samplenummer")

    }

    if(x_axis_type == "date"){

      if(scale_to_max){
        x <- x |>
          group_by(year, locality, id_family) |>
          mutate(taxa_biomass = scales::rescale(taxa_biomass,
                                                to = c(0, 100),
                                                na.rm = TRUE)
          )
      }

      p <- ggplot(x) +
        geom_smooth(aes(
          x = lubridate::yday(end_date_obs),
          y = taxa_biomass,
          color = id_family
        ),
        lwd = 2) +
        facet_grid(
          rows = vars(year),
          cols = NULL,
          scales = "fixed"
        ) +
        #scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
        ylim(c(0, max(x$taxa_biomass, na.rm = TRUE))) +
        scale_color_nina(name = "Familie") +
        ylab("Biomasse (g.)") +
        xlab("Samplingdato")

    }

  }

  if(hide_legend){
    p <- p +
      theme(legend.position = "")
  }

  p

}

