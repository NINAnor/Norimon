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

  my_min <- function(x, ...) {if (length(x)>0) min(x, ...) else Inf}
  my_max <- function(x, ...) {if (length(x)>0) max(x, ...) else Inf}
  my_mean <- function(x, ...) {if (length(x)>0) mean(x, ...) else Inf}

  variable <- match.arg(variable, c("temperature",
                                    "precipitation",
                                    "snow_depth")
                        )

  legend_variable <- switch(variable,
                            temperature = "temperatur",
                            precipitation = "nedbør",
                            snow_depth = "snødybde"
  )

  language <- match.arg(language,
                        c("Norwegian",
                          "English")
  )

  if(language == "Norwegian"){
    month_lab <- c("januari",
               "februar",
               "mars",
               "april",
               "mai",
               "juni",
               "juli",
               "august",
               "september",
               "oktober",
               "november",
               "desember"
    )
  } else {month_lab <- month.name}

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
    mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
           avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
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
    filter(!!variable < past_low)

  past_highs <- past %>%
    group_by(new_day) %>%
    summarise(past_high = my_max(!!variable, na.rm = TRUE))

  present_highs <- present %>%
    left_join(past_highs,
              by = c("new_day" = "new_day")) %>%
    filter(!!variable > past_high)



# dgr_fmt <- function(x, ...) {
#   parse(text = paste(x, "*degree", sep = ""))
# }
#
# a <- dgr_fmt(seq(-30, y_high_limit, by = 10))


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
           label = paste0("Det var ", no_high_days, " dager med høyere"), size = 3,
           colour = "firebrick3",
           hjust = 0,
           vjust = 0) +
  annotate("text", x = high_annot_coord[[1]] + 12, y = high_annot_coord[[2]] + 2,
           label = "verdier en tidligere år", size = 3, colour = "firebrick3",
           hjust = 0,
           vjust = 0)



low_annot_coord <- present_lows %>%
  filter(!!variable == suppressWarnings(min(!!variable, na.rm = TRUE))) %>%
  select(new_day,
         !!variable) %>%
  as.vector()

no_low_days <- nrow(present_lows)

p <- p +
  annotate("segment", x = low_annot_coord[[1]], xend = low_annot_coord[[1]] + 10,
           y = low_annot_coord[[2]], yend = low_annot_coord[[2]] - 5, colour = "blue3") +
  annotate("text", x = low_annot_coord[[1]] + 12, y = low_annot_coord[[2]] - 4 ,
           label = paste0("Det var ", no_low_days, " dager med lavere"), size = 3,
           colour = "blue3",
           hjust = 0,
           vjust = 0) +
  annotate("text", x = low_annot_coord[[1]] + 12, y = low_annot_coord[[2]] - 6,
           label = "verdier en tidligere år", size = 3, colour = "blue3",
           hjust = 0,
           vjust = 0)


p <- p +
  ggtitle(paste(placename, "s vær i ", focus_year, sep = "")) +
  theme(plot.title=element_text(face = "bold", hjust = .012, vjust = .8, colour = "#3C3C3C", size = 20)) +
  annotate("text",
           x = 14,
           y = y_high_limit - 5,
           label = stringr::str_to_sentence(legend_variable),
           size = 4,
           fontface = "bold",
           hjust = 0)


p <- p +
  annotate("text",
           x = 14,
           y = y_high_limit - 15,
           label = paste0("Data representer gjennomsnittlig ", legend_variable,
                          ".\nData strekker seg fra ",
                          first_date, "."),
           size = 3,
           colour="gray30",
           hjust = 0,
           vjust = 0)




#Annotate max and 95% intervals

#legend_data <- data.frame(x = seq(175, 182), y = rnorm(8, 15, 2))
# p <- p +
#   annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "wheat2", size=3) +
#   annotate("segment", x = 181, xend = 181, y = 12, yend = 18, colour = "wheat4", size=3) +
#   geom_line(data=legend_data, aes(x=x,y=y)) +
#   annotate("segment", x = 183, xend = 185, y = 17.7, yend = 17.7, colour = "wheat4", size=.5) +
#   annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, colour = "wheat4", size=.5) +
#   annotate("segment", x = 185, xend = 185, y = 12.2, yend = 17.7, colour = "wheat4", size=.5) +
#   annotate("text", x = 200, y = 14.75, label = "NORMAL RANGE", size=2, colour="gray30") +
#   annotate("text", x = 155, y = 14.75, label = "2016 TEMPERATURE", size=2, colour="gray30") +
#   annotate("text", x = 193, y = 25, label = "RECORD HIGH", size=2, colour="gray30") +
#   annotate("text", x = 193, y = 5, label = "RECORD LOW", size=2, colour="gray30")

suppressWarnings(print(p))

}
