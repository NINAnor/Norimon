#' map_plot Plots bootstrap values from boot_stat object on map
#'
#' @param x A boot_stat object
#' @param whole_country Plot whole country (and not only regions with values). Boolean
#' @param ... Additional parameters passed to scale_fill_nina. e.g. palette
#'
#' @return A ggplot
#'
#' @examples
#'
#'
#' \dontrun{
#'
#'         source("~/.rpgpass")
#'
#' connect_to_database()
#'
#' rm(list = c("username", "password"))
#'
#' beetles <- obs_from_db(subset_orders = "Coleoptera",
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
#'
#'
#' @export
#'
map_plot <- function(x, ...){
  UseMethod("map_plot")
}

#' @export
map_plot.boot_stat <- function(x,
                               whole_country = FALSE,
                               ...){



  df <- x[[1]] %>%
    as_tibble()

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
      select(-c(region_name, boot_value, boot_lower25, boot_upper975))

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


  p <- map %>%
    ggplot(.) +
    geom_sf(aes(fill = boot_value)) +
    NinaR::scale_fill_nina(name = value_name,
                           discrete = FALSE,
                           ...)

  if("year" %in% colnames(map)){
    p <- p +
      facet_wrap("year")
    }


  p
}
