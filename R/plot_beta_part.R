#' plot_beta_part
#'
#' @param input
#'
#' @return
#' @export
#'
#'
#' @examples
plot_beta_part <- function(input){

  ylims <- c(0, max(input$beta_sim)*1.1)

  p_sor <- ggplot(input) +
    geom_point(aes(x = distance,
                   y = beta_sor)) +
    geom_smooth(aes(x = distance,
                    y = beta_sor),
                method = "lm",
                color = ninaPalette()[2]) +
    ylim(ylims) +
    ylab(unname(TeX("Total forskjell $\\beta_{sor}$"))) +
    xlab("Avstand mellom lokaliteter (km)")



  p_sim <- ggplot(input) +
    geom_point(aes(x = distance,
                   y = beta_sim)) +
    geom_smooth(aes(x = distance,
                    y = beta_sim),
                method = "lm",
                color = ninaPalette()[2]) +
    ylim(ylims) +
    ylab(unname(TeX("Turnover $\\beta_{sim}$"))) +
    xlab("Avstand mellom lokaliteter (km)")

  p_nes <- ggplot(input) +
    geom_point(aes(x = distance,
                   y = beta_sne)) +
    geom_smooth(aes(x = distance,
                    y = beta_sne),
                method = "lm",
                color = ninaPalette()[2]) +
    ylim(ylims) +
    ylab(unname(TeX("Nestedness $\\beta_{nes}$"))) +
    xlab("Avstand mellom lokaliteter (km)")

  marrangeGrob(list(p_sor,
                    p_sim,
                    p_nes
  ),
  top = "",
  nrow = 3,
  ncol = 1)

}
