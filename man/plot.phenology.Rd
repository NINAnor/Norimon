% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_phenology.R
\name{plot.phenology}
\alias{plot.phenology}
\title{Plot a phenology object}
\usage{
\method{plot}{phenology}(
  x,
  y_value = "taxa_biomass",
  x_axis_type = "Date",
  aggregation = "Smooth",
  ...
)
}
\arguments{
\item{x}{a phenology object}

\item{y_value}{What y_value to plot (taxa_biomass, no_species)}

\item{x_axis_type}{What x type to use (date, sampling number)}

\item{aggregation}{type of line aggregation (geom_smooth, etc)}

\item{...}{additional parameters, not currently implemented}
}
\value{
a ggplot of the phenology
}
\description{
Plot a phenology object
}
\examples{

\dontrun{

order_phen <- get_phenology(taxonomic_level = "Order")
plot(order_phen)

}



}
