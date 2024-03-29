% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_value.R
\name{bootstrap_value}
\alias{bootstrap_value}
\title{Bootstrap value}
\usage{
bootstrap_value(
  df,
  value = c(no_species, shannon_div, mean_no_asv_per_species, sum_wet_weight,
    avg_wet_weight),
  groups = NULL,
  rolling_year_window = TRUE,
  lower_limit = 0.025,
  upper_limit = 0.975,
  R = 999
)
}
\arguments{
\item{df}{dataframe (or tibble)}

\item{value}{Column to bootstrap, "no_species" (default), "shannon_div", "mean_no_asv_per_species"}

\item{groups}{Optional grouping variables as character vector.}

\item{rolling_year_window}{Should the yearly values be calculated around a 5 year rolling window? Only applies if "year" is present in the groupings}

\item{lower_limit}{Lower limit to confidence intervals, e.g. 0.025 for lower 2.5\%}

\item{upper_limit}{Upper limit to confidence intervals, e.g. 0.975 for upper 97.5\%}

\item{R}{Number of bootstrap samples}
}
\value{
Returns an object of class boot_stat.
}
\description{
Bootstrap value
}
\examples{


\dontrun{


beetles <- get_observations(subset_order = "Coleoptera",
                      agg_level = "year_locality")


beetle_shannon_boot <- bootstrap_value(beetles,
                                       value = shannon_div,
                                       groups = c("year",
                                                  "region_name"))
beetle_shannon_boot

}



}
