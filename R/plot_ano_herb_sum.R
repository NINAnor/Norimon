#' Summary plot of ano herb records
#'
#' Convenience function to clear up some code in reports
#'
#' @return A ggplot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' plot_ano_herb_sum()
#'
#'
#' }
#'


plot_ano_herb_sum <- function(){

  Norimon:::checkCon()

  ano_herb_agg <- tbl(con,
                      Id(schema = "views",
                         table = "ano_herb_agg")) %>%
    filter(project_short_name == "NasIns")


  loc_reg <- get_localities(dataset = "NasIns") %>%
    mutate(habitat_type = ifelse(habitat_type == "Forest", "Skog", habitat_type)) %>%
    st_drop_geometry() %>%
    select(locality,
           region_name)

  xlim <- ano_herb_agg %>%
    summarise(min = min(year, na.rm = TRUE),
              max = max(year, na.rm = TRUE)) %>%
    collect() %>%
    as.vector()

  ano_herb_agg %>%
    left_join(loc_reg,
              by = c("locality" = "locality"),
              copy = TRUE) %>%
    group_by(region_name) %>%
    ggplot(.) +
    geom_point(aes(x = year,
                   y = ano_median_cover,
                   size = ano_median_no_spec,
                   col = region_name),
               alpha = 0.7,
               position = position_jitterdodge(dodge.width = 0.4,
                                               jitter.height = 2,
                                               jitter.width = 0)) +
    scale_x_continuous(breaks= seq(xlim[[1]], xlim[[2]])) +
    scale_color_nina(name = "Region") +
    scale_size(name = "Artantall\nkarplanter") +
    guides(color = guide_legend(override.aes = list(size=5))) +
    xlab("År") +
    ylab("Dekningsgrad %")

}
