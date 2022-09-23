#' plot_climate_comparison
#'
#' Plot the normal climate range and a chosen year as a comparison. Code adapted from Brad Boehmke and Scott Ogletree.
#'
#' @param climate_data A tibble of climate data (typically from get_climate_data())
#' @param variable Which variable to choose. "temperature"(default), "precipitation", or "snow_depth"
#' @param focus_year Focus on "latest" year (default) or optional year as numerical value (e.g. 2021)
#' @param clip_to_1990 Logical, should historical records be clipped to >1990.
#' @param y_high_limit The y-axis limit of the plot.
#' @param language Figure text in "Norwegian" or "English"
#'
#' @return A ggplot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' connect_to_insect_db()
#' get_climate_data("Semi-nat_01") %>%
#' plot_climate_comparison()
#'
#' }
#'



plot_climate_comparison <- function(climate_data = NULL,
                                    variable = c("temperature",
                                                 "precipitation",
                                                 "snow_depth"),
                                    focus_year = "latest",
                                    clip_to_1990 = TRUE,
                                    y_high_limit = 60,
                                    language = c("Norwegian",
                                                 "English")){


  # 2017-01-17
  # Code by Scott Ogletree
  # Based on code from https://rpubs.com/bradleyboehmke/weather_graphic,  Brad Boehmke January 2, 2015

  ##Todo, fix small issues with temperature and snow variants.
  ##Check reason for data into the future....!
  #Finish language versions
  #Clean up

  #Preliminaries
  ##define some functions to avoid no non-missing errors

  if(focus_year <= 1990 & clip_to_1990){ stop("Can't clip to 1990 if focus_year < 1990")}


  my_min <- function(x, ...) {if (length(x)>0) min(x, ...) else Inf}
  my_max <- function(x, ...) {if (length(x)>0) max(x, ...) else Inf}
  my_mean <- function(x, ...) {if (length(x)>0) mean(x, ...) else Inf}


  language <- match.arg(language,
                        c("Norwegian",
                          "English")
  )


  text_table <- list("Norwegian" = c("s vær i ",
                                     "Data representer gjennomsnittlig ",
                                     ".\nReferansedata strekker seg fra ",
                                     " dager med høyere",
                                     "verdier enn referansedata",
                                     " dager med lavere",
                                     "verdier enn referansedata",
                                     "Historisk maksimum",
                                     "Historisk minimum",
                                     "95% konfidanse"),
                     "English" = c("'s weather in ",
                                   "Data represents average ",
                                   ".\nReference data beginning from ",
                                   " days with higher",
                                   "values than reference data",
                                   " days with lower",
                                   "values han reference data",
                                   "Historical max",
                                   "Historical low",
                                   "95% confidence"))

  variable <- match.arg(variable, c("temperature",
                                    "precipitation",
                                    "snow_depth")
                        )
  legend_variable <- variable

  legend_table <- list("Norwegian" = list(temperature = "temperatur",
                                           precipitation = "nedbør",
                                           snow_depth = "snødybde"),
                          "English" = list(temperature = "temperature",
                                           precipitation = "precipitation",
                                           snow_depth = "snow depth")

  )


  old_LC_TIME <- Sys.getlocale(category = "LC_TIME")
  if(language == "Norwegian") Sys.setlocale("LC_TIME", "nb_NO.UTF-8")

  if(language == "Norwegian"){
    month_lab <- c("jan.",
               "febr.",
               "mar.",
               "apr.",
               "mai",
               "jun.",
               "jul.",
               "aug.",
               "sept.",
               "okt.",
               "nov.",
               "des."
    )
  } else {month_lab <- month.abb}

  variable <- switch(variable,
         temperature = sym("daily_mean_temp"),
         precipitation = sym("daily_sum_precip"),
         snow_depth = sym("daily_mean_snow_depth")
  )

  variable <- enquo(variable)

  placename <- climate_data %>%
    distinct(locality) %>%
    pull()

  climate_data <- climate_data %>%
    mutate(year = as.numeric(format(date,'%Y')),
           month = as.numeric(format(date,'%m')),
           day = as.numeric(format(date,'%d')),
           new_day = lubridate::yday(date))

  if(clip_to_1990){
    climate_data <- climate_data %>%
      filter(year >= 1990)
  }

  latest_year <- climate_data %>%
    summarise(max(year)) %>%
    pull()

  if(focus_year == "latest"){
    focus_year <- latest_year
  }


  #Set up average and present data
  past <- climate_data %>%
    filter(year != focus_year) %>%
    group_by(new_day) %>%
    mutate(upper = my_max(!!variable, na.rm = TRUE), # identify max value for each day
           lower = min(!!variable, na.rm = TRUE), # identify min value for each day
           avg = mean(!!variable, na.rm = TRUE),  # calculate mean value for each day
           se = sd(!!variable, na.rm = TRUE)/sqrt(length(!!variable))) %>%  # calculate standard error of mean
    mutate(avg_upper = avg + (qt(1-.05/2, nrow(.)) * se),  # calculate 95% CI for mean (get the appropriate t-value for the number of observations)
           avg_lower = avg - (qt(1-.05/2, nrow(.)) * se)) %>%  # calculate 95% CI for mean
    ungroup()

  first_date <- past %>%
    summarise(first_date = min(date, na.rm = TRUE)) %>%
    mutate(first_date = format(first_date, "%d %B, %Y")) %>%
    pull()

  present <- climate_data %>%
    filter(year == focus_year)

  past_lows <- past %>%
    group_by(new_day) %>%
    summarise(past_low = min(!!variable, na.rm = TRUE))

  present_lows <- present %>%
    left_join(past_lows,
              by = c("new_day" = "new_day")) %>%
    filter(!!variable < past_low) %>%
    arrange(date)

  past_highs <- past %>%
    group_by(new_day) %>%
    summarise(past_high = my_max(!!variable, na.rm = TRUE))

  present_highs <- present %>%
    left_join(past_highs,
              by = c("new_day" = "new_day")) %>%
    filter(!!variable > past_high)



dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

a <- dgr_fmt(seq(-30, y_high_limit, by = 10))


#Build the plot, add daily min and max
p <- ggplot(past, aes(new_day, !!variable)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(past, mapping = aes(x = new_day,
                                     ymin = lower,
                                     ymax = upper),
                 colour = "wheat2",
                 alpha=.1)

#add confidence bound
p <- p +
  geom_linerange(past,
                 mapping = aes(x = new_day,
                               ymin = avg_lower,
                               ymax = avg_upper),
                 colour = "wheat4")

#Add focus year
p <- p +
  geom_line(present, mapping = aes(x = new_day,
                                   y = !!variable,
                                   group = 1)) +
  geom_vline(xintercept = 0,
             colour = "wheat4",
             linetype = 1,
             size = 1)

#Add horizontal lines
p <- p +
  geom_hline(yintercept = -30, colour = "white", linetype = 1, size = .25) +
  geom_hline(yintercept = -20, colour = "white", linetype = 1, size = .25) +
  geom_hline(yintercept = -10, colour = "white", linetype = 1, size = .25) +
  geom_hline(yintercept = 0, colour = "white", linetype = 1, size = .25) +
  geom_hline(yintercept = 10, colour = "white", linetype = 1, size = .25) +
  geom_hline(yintercept = 20, colour = "white", linetype = 1, size = .25) +
  geom_hline(yintercept = 30, colour = "white", linetype = 1, size = .25) +
  geom_hline(yintercept = 40, colour = "white", linetype = 1, size = .25) +
  geom_hline(yintercept = 50, colour = "white", linetype = 1, size = .25) +
  geom_hline(yintercept = 60, colour = "white", linetype = 1, size = .25)

#Add vertical lines
p <- p +
  geom_vline(xintercept = 31, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype = 3, size =.25) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype = 3, size =.25)


#Add scale
p <- p +
  coord_cartesian(ylim = c(-30, y_high_limit)) +
  scale_y_continuous(breaks = seq(-30, y_high_limit, by = 10), labels = a) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(15, 45, 75, 105, 135, 165, 195, 228, 258, 288, 320, 350),
                     labels = month_lab)


#Add high and low datapoints
p <- p +
  geom_point(data = present_lows, aes(x = new_day, y = !!variable), colour = "blue3") +
  geom_point(data = present_highs, aes(x = new_day, y = !!variable), colour = "firebrick3")


high_annot_coord <- present_highs %>%
  filter(!!variable == my_max(!!variable, na.rm = TRUE)) %>%
  select(new_day,
         !!variable) %>%
  as.vector()

no_high_days <- nrow(present_highs)

p <- p +
  annotate("segment", x = high_annot_coord[[1]], xend = high_annot_coord[[1]] + 10,
           y = high_annot_coord[[2]], yend = high_annot_coord[[2]] + 5, colour = "firebrick3") +
  annotate("text", x = high_annot_coord[[1]] + 12, y = high_annot_coord[[2]] + 4 ,
           label = paste0(no_high_days, text_table[[language]][4]), size = 3,
           colour = "firebrick3",
           hjust = 0,
           vjust = 0) +
  annotate("text", x = high_annot_coord[[1]] + 12, y = high_annot_coord[[2]] + 2,
           label = text_table[[language]][5], size = 3, colour = "firebrick3",
           hjust = 0,
           vjust = 0)



low_annot_coord <- present_lows %>%
  #filter(!!variable == suppressWarnings(min(!!variable, na.rm = TRUE))) %>%
  slice(1) %>%
  select(new_day,
         !!variable) %>%
  as.vector()

no_low_days <- nrow(present_lows)

p <- p +
  annotate("segment", x = low_annot_coord[[1]], xend = low_annot_coord[[1]] + 10,
           y = low_annot_coord[[2]], yend = low_annot_coord[[2]] - 5, colour = "blue3") +
  annotate("text", x = low_annot_coord[[1]] + 12, y = low_annot_coord[[2]] - 4 ,
           label = paste0(no_low_days, text_table[[language]][6]), size = 3,
           colour = "blue3",
           hjust = 0,
           vjust = 0) +
  annotate("text", x = low_annot_coord[[1]] + 12, y = low_annot_coord[[2]] - 6,
           label = text_table[[language]][7], size = 3, colour = "blue3",
           hjust = 0,
           vjust = 0)


p <- p +
  ggtitle(paste(placename, text_table[[language]][1], focus_year, sep = "")) +
  theme(plot.title=element_text(face = "bold", hjust = .012, vjust = .8, colour = "#3C3C3C", size = 20)) +
  annotate("text",
           x = 14,
           y = y_high_limit - 5,
           label = stringr::str_to_sentence(legend_table[[language]][[legend_variable]]),
           size = 4,
           fontface = "bold",
           hjust = 0)


p <- p +
  annotate("text",
           x = 14,
           y = y_high_limit - 15,
           label = paste0(text_table[[language]][2], legend_table[[language]][[legend_variable]],
                          text_table[[language]][3],
                          first_date, "."),
           size = 3,
           colour="gray30",
           hjust = 0,
           vjust = 0)




#Annotate max and 95% intervals
legend_pos <- tibble(x = 300, y = y_high_limit - 30)

legend_data <- data.frame(x = seq(legend_pos$x - 6, legend_pos$x + 1), y = rnorm(8, legend_pos$y + 10, 2))

p <- p +
  annotate("segment",
           x = legend_pos$x,
           xend = legend_pos$x,
           y = legend_pos$y,
           yend = legend_pos$y +
             20,
           colour = "wheat2",
           size = 3) +
  annotate("segment",
           x = legend_pos$x,
           xend = legend_pos$x,
           y = legend_pos$y + 7,
           yend = legend_pos$y + 13,
           colour = "wheat4",
           size = 3) +
  geom_line(data = legend_data, aes(x = x, y = y)) +
  annotate("segment",
           x = legend_pos$x + 2,
           xend = legend_pos$x + 4,
           y = legend_pos$y + 12.3,
           yend = legend_pos$y + 12.3,
           colour = "wheat4",
           size = .5) +
  annotate("segment",
           x = legend_pos$x + 2,
           xend = legend_pos$x + 4,
           y = legend_pos$y + 7.7,
           yend = legend_pos$y + 7.7,
           colour = "wheat4",
           size = .5) +
  annotate("segment",
           x = legend_pos$x + 4,
           xend = legend_pos$x + 4,
           y = legend_pos$y + 7.7,
           yend = legend_pos$y + 12.3,
           colour = "wheat4",
           size = .5) +
  annotate("text",
           x = legend_pos$x + 7,
           y = legend_pos$y + 9.75,
           label = text_table[[language]][10],
           size = 2,
           colour = "gray30",
           hjust = 0,
           vjust = 0) +
  annotate("text",
           x = legend_pos$x - 10,
           y = legend_pos$y + 9.75,
           label = paste0(focus_year, stringr::str_to_sentence(legend_table[[language]][[legend_variable]])),
           size = 2,
           colour = "gray30",
           hjust = 1,
           vjust = 0) +
  annotate("text",
           x = legend_pos$x + 7,
           y = legend_pos$y + 19,
           label = text_table[[language]][8],
           size = 2,
           colour = "gray30",
           hjust = 0,
           vjust = 0) +
  annotate("text",
           x = legend_pos$x + 7,
           y = legend_pos$y,
           label = text_table[[language]][9],
           size = 2,
           colour = "gray30",
           hjust = 0,
           vjust = 0)

suppressWarnings(print(p))

invisible(Sys.setlocale(category = "LC_TIME", old_LC_TIME))

}
