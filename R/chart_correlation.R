#' chart_correlation Modified from PerformanceAnalytics::chart.Correlation
#'
#' @param R Tibble (or data frame) with values for correlation plot
#' @param histogram Plot histogram? Defaults to TRUE
#' @param method Correlation method. c("pearson", "kendall", "spearman")
#' @param color Color of stars and lines
#' @param ... additional parameters passed to panel.cor
#'
#' @return A correlation plot.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' locality_sampling_loggerdata <- get_logger_data(
#'   dataset = "NorIns",
#'   agg_level = "locality_sampling"
#' )
#'
#' locality_sampling_loggerdata %>%
#'   select(
#'     Temp_MX2301A = avg_values_MX2301A_temperature,
#'     Temp_MX2202 = avg_values_MX2202_temperature,
#'     RL = avg_values_MX2301A_rh,
#'     Lys = avg_values_MX2202_light
#'   ) %>%
#'   filter(Lys >= 100) %>%
#'   chart_correlation(.,
#'     histogram = TRUE,
#'     method = "pearson",
#'     color = ninaColors()[1]
#'   )
#' }
#'

#density, hist, lines, par, rug

chart_correlation <- function(R,
                              histogram = TRUE,
                              method = c("pearson",
                                         "kendall",
                                         "spearman"
                                         ),
                              color = "red",
                              ...) {
  x <- PerformanceAnalytics::checkData(R, method = "matrix")
  if (missing(method)) {
    method <- method[1]
  }
  cormeth <- method
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs",
                        method = cormeth, cex.cor, ...) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr))
    graphics::par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) {
      cex <- 0.8 / strwidth(txt)
    }
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value,
      corr = FALSE, na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c(
        "***",
        "**", "*", ".", " "
      )
    )
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3) / 1.3)
    text(0.8, 0.8, Signif, cex = cex, ..., col = color)
  }
  # f <- function(t) {
  #   stats::dnorm(t, mean = mean(x), sd = sd.xts(x))
  # }

  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel <- function(x, ... = NULL) {
    graphics::par(new = TRUE)
    graphics::hist(x,
      col = "light gray", probability = TRUE, axes = FALSE,
      main = "", breaks = "FD"
    )
    stats::lines(stats::density(x, na.rm = TRUE), lwd = 1, col = color)
    stats::rug(x)
  }
  if (histogram) {
    suppressWarnings(graphics::pairs(x,
      gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor,
      diag.panel = hist.panel, col.smooth = color
    ))
  } else {
    graphics::pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor)
  }
}
