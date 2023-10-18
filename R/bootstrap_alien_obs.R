#' bootstrap_alien_obs
#'
#' Bootstraps alien species frequencies (number of localities with presences) for each alien species and weighs these together as summed Shannon information
#'
#' @param df dataframe (or tibble). Must be a table of presences and absences of all alien species in all localities. Typically from the table views.alien_obs and aggregated to year-locality level.
#' @param groups Optional grouping variables as character vector.
#' @param rolling_year_window Should the yearly values be calculated around a 5 year rolling window? Only applies if "year" is present in the groupings
#' @param lower_limit Lower limit to confidence intervals, e.g. 0.025 for lower 2.5%
#' @param upper_limit Upper limit to confidence intervals, e.g. 0.975 for upper 97.5%
#' @param Hill Calculate Shannon information as Hill number. Boolean
#' @param R Number of bootstrap samples
#'
#' @return Returns an object of class boot_stat.
#' @export
#'
#' @examples
#'
#' #'\dontrun{
#'
#'
#' }
#'
#'
#'



bootstrap_alien_obs <- function(df,
                                groups = NULL,
                                rolling_year_window = TRUE,
                                lower_limit = 0.025,
                                upper_limit = 0.975,
                                Hill = TRUE,
                                R = 999){

  if("locality" %in% groups & rolling_year_window) stop("Can't use rolling window with stratified by localities, they only occur once per rolling window")

  df <- df

  calc_shannon_alien <- function(x,
                                 ...) {

    p <- x[x > 0] # Get rid of zero proportions (log zero is undefined)
    out <- -sum(p * log(p)) # Calculate index

    if(Hill){
      out <- exp(out)
    }

    return(out)

  }

  #Expand dataset to include up to 5 year window around every year
  if("year" %in% groups & rolling_year_window){

    out <- NULL

    for(subset_year in unique(df$year)){

      subset <- df %>%
        filter(year >= subset_year - 2 & year <= subset_year + 2) %>%
        mutate(year_window = subset_year)

      out <- rbind(out, subset)

    }

    df <- out
    groups[groups == "year"] <- "year_window"
  }

  groupings <- groups
  groupings_loc <- c(groups, "locality")
  groupings_spec <- c(groups, "species_latin_fixed")


  one_boot <- function(x){ out <- x %>%
    group_by_at(groupings_spec) %>%
    slice_sample(prop = 1, replace = TRUE) %>%
    group_by_at(groupings_spec) %>%
    summarise(freq = sum(present) / n(),
              .groups = "drop") %>%
    group_by_at(groupings) %>%
    summarise(boot_values = calc_shannon_alien(freq),
              .groups = "drop")

  return(out)
  }


  no_cores <- pmin(detectCores() - 1, 10) ##set maximum cores to 10, otherwise all but 1
  cl <- parallel::makeCluster(no_cores)
  suppressWarnings(parallel::clusterEvalQ(cl, require(dplyr)))
  parallel::clusterExport(cl, c("df", "one_boot", "groupings_spec", "groupings", "calc_shannon_alien"))
  parallel::clusterSetRNGStream(cl)

  ##<5 sec with R=999, and 10 cores
  boot_temp_res <- parallel::parSapply(cl, 1:R, function(i, ...) one_boot(df))
  stopCluster(cl)
  ##51 sec
  #system.time(boot_temp_res <- replicate(R, one_boot(df)))
  boot_temp_res_list <- apply(boot_temp_res, 2, function(x) as.data.frame(x))
  bootstrap_values <- do.call("rbind", boot_temp_res_list)



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

  attr(out, "value_name") <- "alien_score"


  return(out)
}