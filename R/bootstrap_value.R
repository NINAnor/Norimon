#' Bootstrap value
#'
#' @param df dataframe (or tibble)
#' @param value Column to bootstrap
#' @param groupings Optional grouping variables as character vector.
#' @param lower_limit Lower limit to confidence intervals, e.g. 0.025 for lower 2.5%
#' @param upper_limit Upper limit to confidence intervals, e.g. 0.975 for upper 97.5%
#' @param R Number of bootstrap samples
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'\dontrun{
#'
#'
#' beetles <- obs_from_db(subset_order = "Coleoptera",
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
                            value,
                            groups,
                            lower_limit = 0.025,
                            upper_limit = 0.975,
                            R = 999){

  groupings <- groups
  value <- enquo(value)

  out <- df %>%
    group_by_at(groupings) %>%
    summarise(boot_mean = boot_mean(!!value),
              boot_lower25 = boot_lower(!!value),
              boot_upper975 = boot_upper(!!value))

    return(out)


}

