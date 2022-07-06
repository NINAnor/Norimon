#' Bootstrap value
#'
#' @param df dataframe (or tibble)
#' @param value Column to bootstrap, "no_species" (default), "shannon_div", "mean_asv_per_species"
#' @param groupings Optional grouping variables as character vector.
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
      summarise(boot_value= mean(boot_values),
              boot_lower25 = nth(boot_values, floor(R * 0.025), order_by = boot_values),
              boot_upper975 = nth(boot_values, ceiling(R * 0.975), order_by = boot_values),
              .groups = "drop")




  out <- list("bootstrap_summary" = bootstrap_summary,
              "bootstrap_values" = bootstrap_values)


  class(out) <- c("boot_stat", "list")

  attr(out, "value_name") <- deparse(substitute(value))


  return(out)


}

#' @export
print.boot_stat <- function(x){
  print(x[[1]])
}

#' @noRd
#' @export
"-.boot_stat" <- function(x, ...){

  bootstrap_values <- x[[2]]

  bootstrap_values$boot_values <- bootstrap_values$boot_values - ...

  bootstrap_summary <- bootstrap_values %>%
    dplyr::group_by(across(!boot_values)) %>%
    dplyr::summarise(boot_mean = mean(boot_values),
              boot_lower25 = dplyr::nth(boot_values, floor(length(boot_values) * 0.025), order_by = boot_values),
              boot_upper975 = dplyr::nth(boot_values, ceiling(length(boot_values) * 0.975), order_by = boot_values),
              .groups = "drop")


  out <- list("bootstrap_summary" = bootstrap_summary,
              "bootstrap_values" = bootstrap_values)


  class(out) <- c("boot_stat", "list")


  return(out)

}



#' @export
boot_contrast <- function(x, ...){
  UseMethod("boot_contrast")
}

#' @export
boot_contrast.boot_stat <- function(x,
                               level = NULL){

  level = enquo(level)

  contrast_bootstrap_values <- x[[2]] %>%
    as_tibble() %>%
    filter(!!level)

  bootstrap_values <- x[[2]]

  bootstrap_values$boot_values <- bootstrap_values$boot_values - contrast_bootstrap_values$boot_values #implictly gets reused

  bootstrap_summary <- bootstrap_values %>%
    dplyr::group_by(across(!boot_values)) %>%
    dplyr::summarise(boot_mean = mean(boot_values),
                     boot_lower25 = dplyr::nth(boot_values, floor(length(boot_values) * 0.025), order_by = boot_values),
                     boot_upper975 = dplyr::nth(boot_values, ceiling(length(boot_values) * 0.975), order_by = boot_values),
                     .groups = "drop")


  out <- list("bootstrap_summary" = bootstrap_summary,
              "bootstrap_values" = bootstrap_values)


  class(out) <- c("boot_stat", "list")
  return(out)

}




