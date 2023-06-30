#' map_plot Plots bootstrap values from boot_stat object on map
#'
#' @param x A boot_stat object
#' @param whole_country Plot whole country (and not only regions with values). Boolean
#' @param alpha_from_sd Boolean, compute alpha (transparancy) values from sd?
#' @param alpha_range Range of alpha values
#' @param ... Additional parameters passed to scale_fill_nina. e.g. palette
#'
#' @return A ggplot
#'
#' @examples
#'
#'
#' \dontrun{
#'
#'
#' connect_to_insect_db()
#'
#' beetles <- get_observations(subset_orders = "Coleoptera",
#'                       agg_level = "year_locality")
#'
#'
#' beetle_asv_div_boot <- bootstrap_value(beetles,
#'                                       value = mean_asv_per_species,
#'                                       groups = c("year",
#'                                                  "region_name"))
#'
#'
#' map_plot(beetle_asv_div_boot,
#'          palette = "blue-orange")
#'
#' }
#' @export
map_plot <- function(x,
                     whole_country = FALSE,
                     alpha_from_sd = FALSE,
                     alpha_range = c(0.3, 0.9),
                     palette = "blue-orange",
                     ...){
  UseMethod("map_plot", x)
}


#' @export
map_plot.boot_stat <- function(x,
                               whole_country = FALSE,
                               alpha_from_sd = FALSE,
                               alpha_range = c(0.3, 0.9),
                               palette = "blue-orange",
                               ...){

  checkCon()



  df <- x[[1]] %>%
    tidyr::as_tibble()

  value_name <- stringr::str_to_sentence(attr(x, "value_name"))




  if(!whole_country){

    values_fylke <- df %>%
      left_join(region_fylke(),
                by = c("region_name" = "region_name"))

    map_to_get <- df %>%
      select(region_name) %>%
      distinct() %>%
      pull()

    map <- get_map(region_subset = map_to_get)

    map <- map %>%
      left_join(values_fylke,
                by = c("fylke" = "fylke"))
  } else {

    possible_years <- df %>%
      select(-c(region_name, boot_value, boot_sd, boot_lower2.5, boot_upper97.5))

    all_fylkes <- region_fylke() %>%
      select(region_name)

    all_fylke_years <- crossing(possible_years, all_fylkes)

    suppressMessages({

      values_fylke <- df %>%
      right_join(all_fylke_years) %>%
      left_join(region_fylke(),
                by = c("region_name" = "region_name"))
    })

    map <- get_map()

    map <- map %>%
      left_join(values_fylke,
                by = c("fylke" = "fylke"))

  }


  if(alpha_from_sd){
    p <- map %>%
      ggplot(.) +
      geom_sf(aes(fill = boot_value,
                  alpha = 1/boot_sd)) +
      NinaR::scale_fill_nina(name = value_name,
                             discrete = FALSE,
                             ...) +
      scale_alpha(range = alpha_range,
                  guide = "none")

  } else {

  p <- map %>%
    ggplot(.) +
    geom_sf(aes(fill = boot_value)) +
    NinaR::scale_fill_nina(name = value_name,
                           discrete = FALSE,
                           ...)
  }

  if("year" %in% colnames(map)){
    p <- p +
      facet_wrap("year")
    }


  p
}
