#' Bootstrap value
#'
#' @param df dataframe (or tibble)
#' @param value Column to bootstrap, "no_species" (default), "shannon_div", "mean_asv_per_species"
#' @param groups Optional grouping variables as character vector.
#' @param lower_limit Lower limit to confidence intervals, e.g. 0.025 for lower 2.5%
#' @param upper_limit Upper limit to confidence intervals, e.g. 0.975 for upper 97.5%
#' @param R Number of bootstrap samples
#'
#' @return Returns an object of class boot_stat.
#' @export
#'
#' @examples
#'
#'
#'\dontrun{
#'
#'
#' beetles <- get_observations(subset_order = "Coleoptera",
#'                       agg_level = "year_locality")
#'
#'
#' beetle_shannon_boot <- bootstrap_value(beetles,
#'                                        value = shannon_div,
#'                                        groups = c("year",
#'                                                   "region_name"))
#' beetle_shannon_boot
#'
#'}
#'
#'
#'
bootstrap_value <- function(df,
                            value = c(no_species, shannon_div, mean_asv_per_species),
                            groups,
                            lower_limit = 0.025,
                            upper_limit = 0.975,
                            R = 999){


  groupings <- groups
  value_quote <- enquo(value)

  bootstrap_values <- df %>%
    group_by_at(groupings) %>%
    summarise(boot_values = list(replicate(R,
                                       mean(sample(!!value_quote,
                                                   size = length(!!value_quote),
                                                   replace = TRUE)
                                       )
    )),
    .groups = "keep") %>%
    unnest(cols = c(boot_values))


  bootstrap_summary <- bootstrap_values %>%
    group_by_at(groupings) %>%
      summarise(boot_value = mean(boot_values),
                boot_sd = sd(boot_values),
                boot_lower2.5 = nth(boot_values, floor(R * 0.025), order_by = boot_values),
                boot_upper97.5 = nth(boot_values, ceiling(R * 0.975), order_by = boot_values),
                .groups = "drop")




  out <- list("bootstrap_summary" = bootstrap_summary,
              "bootstrap_values" = bootstrap_values)


  class(out) <- c("boot_stat", "list")

  attr(out, "value_name") <- deparse(substitute(value))


  return(out)


}



