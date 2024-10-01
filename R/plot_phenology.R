#' Plot a phenology object
#'
#' @param x a phenology object
#' @param y_value What y_value to plot (taxa_biomass, no_species)
#' @param x_axis_type What x type to use (date, sampling number)
#' @param aggregation type of line aggregation (geom_smooth, etc)
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
                           x_axis_type = "Date",
                           aggregation = "Smooth",
                           ...) {
  if (!"phenology" %in% class(x)) stop("Input object must be of class 'phenology'")

  id_group <- attr(x, "taxonomic_level")

  if (id_group == "id_order") {
    subset_orders <- c("Diptera", "Coleoptera", "Hymenoptera", "Hemiptera", "Lepidoptera")

    x <- x %>%
      ungroup() %>%
      filter(id_order %in% subset_orders) %>%
      mutate(sampling_number = as.integer(sampling_number))

    x %>%
      select(sampling_number) %>%
      distinct()

    x %>%
      select(year, id_order, sampling_number) %>%
      distinct()

    # summing to sampling number
    # Why does 2020 lack data on sampling 8?

    x <- x %>%
      group_by(
        year,
        sampling_number,
        id_order
      ) %>%
      summarise(taxa_biomass = mean(taxa_biomass, na.rm = TRUE))

    p <- ggplot(x) +
      geom_path(aes(x = sampling_number, y = taxa_biomass, color = id_order),
        lwd = 2
      ) +
      facet_grid(
        cols = vars(year),
        rows = NULL,
        scales = "free_x"
      )
  }

  if (id_group == "id_family") {
    # subset_orders <- c("Diptera", "Coleoptera", "Hymenoptera", "Hemiptera", "Lepidoptera")

    x <- x %>%
      ungroup() %>%
      # filter(id_family %in% subset_orders) %>%
      mutate(sampling_number = as.integer(sampling_number))

    x %>%
      select(sampling_number) %>%
      distinct()

    x %>%
      select(year, id_family, sampling_number) %>%
      distinct()

    # summing to sampling number
    # Why does 2020 lack data on sampling 8?

    x <- x %>%
      group_by(
        year,
        sampling_number,
        id_family
      ) %>%
      summarise(taxa_biomass = mean(taxa_biomass, na.rm = TRUE))

    p <- ggplot(x) +
      geom_path(aes(x = sampling_number, y = taxa_biomass, color = id_family),
        lwd = 2
      ) +
      facet_grid(
        cols = vars(year),
        rows = NULL,
        scales = "free_x"
      )
  }


  p
}
