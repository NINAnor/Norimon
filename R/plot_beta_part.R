#' plot_beta_part
#'
#' @param input Tibble or dataframe of betapartition + locality distance from the combine_dist_to_comm_mat function
#'
#' @return Returns a plot of the beta diversity partition of the overall diversity
#' @export
#'
#'
#'
#'
#' @examples
#'
#' \dontrun{
#' connect_to_insect_db()
#'
#' seminat_trond_comm_mat <- get_community_matrix(trap_type = "MF",
#'                                           dataset = "NasIns",
#'                                           subset_habitat = "Semi-nat",
#'                                           subset_region = "Trøndelag",
#'                                           as_tibble = T,
#'                                           transposed_matrix = F
#' )
#'
#' seminat_trond_dist_beta <- combine_dist_to_comm_mat(comm_mat = seminat_trond_comm_mat,
#'                                                region_name = "('Trøndelag')",
#'                                                habitat_type = "Semi-nat")
#'
#' plot_beta_part(seminat_trond_dist_beta)
#'
#' }
#'

plot_beta_part <- function(input){

  ylims <- c(0, max(input$beta_sim)*1.1)

  p_sor <- ggplot(input) +
    geom_point(aes(x = distance,
                   y = beta_sor)) +
    geom_smooth(aes(x = distance,
                    y = beta_sor),
                method = "lm",
                color = NinaR::ninaPalette()[2]) +
    ylim(ylims) +
    ylab(unname(latex2exp::TeX("Total forskjell $\\beta_{sor}$"))) +
    xlab("Avstand mellom lokaliteter (km)")



  p_sim <- ggplot(input) +
    geom_point(aes(x = distance,
                   y = beta_sim)) +
    geom_smooth(aes(x = distance,
                    y = beta_sim),
                method = "lm",
                color = NinaR::ninaPalette()[2]) +
    ylim(ylims) +
    ylab(unname(latex2exp::TeX("Turnover $\\beta_{sim}$"))) +
    xlab("Avstand mellom lokaliteter (km)")

  p_nes <- ggplot(input) +
    geom_point(aes(x = distance,
                   y = beta_sne)) +
    geom_smooth(aes(x = distance,
                    y = beta_sne),
                method = "lm",
                color = NinaR::ninaPalette()[2]) +
    ylim(ylims) +
    ylab(unname(latex2exp::TeX("Nestedness $\\beta_{nes}$"))) +
    xlab("Avstand mellom lokaliteter (km)")

  gridExtra::marrangeGrob(list(p_sor,
                    p_sim,
                    p_nes
  ),
  top = "",
  nrow = 3,
  ncol = 1)

}
