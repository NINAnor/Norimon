#' @export
print.boot_stat <- function(x){

  var_name <- sym(attr(x, "value_name"))
  out <- x[[1]] %>%
    rename(!!var_name := boot_value)

  print(out)
}

#' @noRd
#' @export
"-.boot_stat" <- function(x, ...){

  bootstrap_values <- x[[2]]

  if("numeric" %in% class(...)){

    if(length(...) > 1) stop("Can only subtract a single value")

  bootstrap_values$boot_values <- bootstrap_values$boot_values - ...

  bootstrap_summary <- bootstrap_values %>%
    dplyr::group_by(across(!boot_values)) %>%
    dplyr::summarise(boot_value = mean(boot_values),
                     boot_sd = sd(boot_values),
                     boot_lower2.5 = dplyr::nth(boot_values, floor(length(boot_values) * 0.025), order_by = boot_values),
                     boot_upper97.5 = dplyr::nth(boot_values, ceiling(length(boot_values) * 0.975), order_by = boot_values),
                     .groups = "drop")

  } else

  if("boot_stat" %in% class(...)){


    subtr_values <- ...[[2]]

    bootstrap_values$boot_values <- bootstrap_values$boot_values - subtr_values$boot_values

    bootstrap_summary <- bootstrap_values %>%
      dplyr::group_by(across(!boot_values)) %>%
      dplyr::summarise(boot_value = mean(boot_values),
                       boot_sd = sd(boot_values),
                       boot_lower2.5 = dplyr::nth(boot_values, floor(length(boot_values) * 0.025), order_by = boot_values),
                       boot_upper97.5 = dplyr::nth(boot_values, ceiling(length(boot_values) * 0.975), order_by = boot_values),
                       .groups = "drop")


  } else {
    stop("Object to subtract must be either of class numeric or boot_stat")
  }

  out <- list("bootstrap_summary" = bootstrap_summary,
              "bootstrap_values" = bootstrap_values)


  attr(out, "value_name") <- attr(x, "value_name")

  class(out) <- c("boot_stat", "list")


  return(out)

}




#' @noRd
#' @export
"/.boot_stat" <- function(x, ...){

  bootstrap_values <- x[[2]]

  if("numeric" %in% class(...)){

    if(length(...) > 1) stop("Can only divide by a single value")

    bootstrap_values$boot_values <- exp(log(bootstrap_values$boot_values) - log(...))

    bootstrap_summary <- bootstrap_values %>%
      dplyr::group_by(across(!boot_values)) %>%
      dplyr::summarise(boot_value = mean(boot_values),
                       boot_sd = sd(boot_values),
                       boot_lower2.5 = dplyr::nth(boot_values, floor(length(boot_values) * 0.025), order_by = boot_values),
                       boot_upper97.5 = dplyr::nth(boot_values, ceiling(length(boot_values) * 0.975), order_by = boot_values),
                       .groups = "drop")

  } else

    if("boot_stat" %in% class(...)){


      subtr_values <- ...[[2]]

      bootstrap_values$boot_values <- exp(log(bootstrap_values$boot_values) - log(subtr_values$boot_values))

      bootstrap_summary <- bootstrap_values %>%
        dplyr::group_by(across(!boot_values)) %>%
        dplyr::summarise(boot_value = mean(boot_values),
                         boot_sd = sd(boot_values),
                         boot_lower2.5 = dplyr::nth(boot_values, floor(length(boot_values) * 0.025), order_by = boot_values),
                         boot_upper97.5 = dplyr::nth(boot_values, ceiling(length(boot_values) * 0.975), order_by = boot_values),
                         .groups = "drop")


    } else {
      stop("Numerator must be either of class numeric or boot_stat")
    }

  out <- list("bootstrap_summary" = bootstrap_summary,
              "bootstrap_values" = bootstrap_values)


  attr(out, "value_name") <- attr(x, "value_name")

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
    dplyr::summarise(boot_value = mean(boot_values),
                     boot_sd = sd(boot_values),
                     boot_lower2.5 = dplyr::nth(boot_values, floor(length(boot_values) * 0.025), order_by = boot_values),
                     boot_upper97.5 = dplyr::nth(boot_values, ceiling(length(boot_values) * 0.975), order_by = boot_values),
                     .groups = "drop")


  out <- list("bootstrap_summary" = bootstrap_summary,
              "bootstrap_values" = bootstrap_values)


  attr(out, "value_name") <- attr(x, "value_name")

  class(out) <- c("boot_stat", "list")
  return(out)

}




