#' clim_trend_plot
#'
#' Summarises mean values of the weather (temperature and precipitation) during chosen months for the localities within a chosen region.
#' @encoding UTF-8
#' @param region Which region to plot? Currently available: c("Trøndelag", "Østlandet", "Sørlandet", "Nord-Norge").
#' @param dataset Which dataset to use? Is used to select the localities within regions.
#' @param from_year Plot data from what year= Integer.
#' @param to_year Plot data to what year? Integer.
#' @param from_month Summarise data from what month?. Integer
#' @param to_month Summarise data to what month? Integer
#'
#' @return A plot (a grob containing two ggplots)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' clim_trend_plot(
#'   region = "Trøndelag",
#'   from_year = 2010,
#'   to_year = 2022,
#'   from_month = 6,
#'   to_month = 7
#' )
#' }
#'
clim_trend_plot <- function(region = c("Tr\u00f8ndelag", "\u00d8stlandet", "S\u00f8rlandet", "Nord-Norge"),
                            dataset = c("NasIns", "TidVar", "\u00d8koTrond", "HulEik"),
                            from_year = 2010,
                            to_year = 2023,
                            from_month = 6,
                            to_month = 7) {
  from_year <- from_year
  focus_year <- to_year

  from_month <- from_month
  to_month <- to_month

  region <- match.arg(region, c("Tr\u00f8ndelag", "\u00d8stlandet", "S\u00f8rlandet", "Nord-Norge"))
  dataset <- match.arg(dataset, c("NasIns", "TidVar", "\u00d8koTrond", "HulEik"))

  loc_in_region_q <- "
  SELECT l.locality
  FROM events.year_locality yl,
  locations.localities l
  WHERE yl.year = ?id1
  AND yl.locality_id = l.id
  AND l.region_name = ?id2
  AND yl.project_short_name = ?id3

  "

  loc_in_region_san <- DBI::sqlInterpolate(con,
    loc_in_region_q,
    id1 = focus_year,
    id2 = region,
    id3 = dataset
  )

  loc_in_region <- DBI::dbGetQuery(
    con,
    loc_in_region_san
  ) %>%
    pull()


  clim_data <- get_climate_data(locality = loc_in_region)



  clim_data_agg <- clim_data %>%
    mutate(
      month = lubridate::month(date),
      year = lubridate::year(date)
    ) %>%
    filter(
      month >= from_month,
      month <= to_month,
      year >= from_year,
      year <= to_year
    ) %>%
    group_by(year, locality) %>%
    summarise(
      sum_temp = sum(daily_mean_temp),
      sum_precip = sum(daily_sum_precip)
    ) %>%
    group_by(year) %>%
    summarise(
      mean_sum_temp = mean(sum_temp),
      sd_sum_temp = sd(sum_temp),
      mean_sum_precip = mean(sum_precip),
      sd_sum_precip = sd(sum_precip)
    ) %>%
    ungroup()

  p1 <- ggplot(clim_data_agg) +
    geom_pointrange(
      aes(
        x = year,
        y = mean_sum_temp,
        ymin = mean_sum_temp - sd_sum_temp,
        ymax = mean_sum_temp + sd_sum_temp
      ),
      color = nina_colors[2],
      lwd = 1.2
    ) +
    geom_line(
      aes(
        x = year,
        y = mean_sum_temp
      ),
      color = nina_colors[2],
      lwd = 1.2
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_continuous(breaks = from_year:to_year) +
    ylab("Temperatursum") +
    xlab("\u00C5r")


  p2 <- ggplot(clim_data_agg) +
    geom_pointrange(
      aes(
        x = year,
        y = mean_sum_precip,
        ymin = mean_sum_precip - sd_sum_precip,
        ymax = mean_sum_precip + sd_sum_precip
      ),
      color = nina_colors[3],
      lwd = 1.2
    ) +
    geom_line(
      aes(
        x = year,
        y = mean_sum_precip
      ),
      color = nina_colors[3],
      lwd = 1.2
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_continuous(breaks = from_year:to_year) +
    ylab("Nedb\u00f8rsum") +
    xlab("\u00C5r")


  gridExtra::marrangeGrob(
    list(
      p1,
      p2
    ),
    top = "",
    ncol = 1,
    nrow = 2
  )
}
