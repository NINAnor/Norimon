% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_biomass.R
\encoding{UTF-8}
\name{get_biomass}
\alias{get_biomass}
\title{get_biomass Get biomass data from the database}
\usage{
get_biomass(
  limit = NULL,
  subset_orders = NULL,
  subset_families = NULL,
  subset_genus = NULL,
  subset_species = NULL,
  subset_year = NULL,
  subset_habitat = NULL,
  trap_type = "MF",
  subset_region = NULL,
  dataset = "NorIns",
  agg_level = "year_locality",
  as_tibble = F
)
}
\arguments{
\item{limit}{Optional row limit on output (for testing)}

\item{subset_orders}{Optional subset of order}

\item{subset_families}{Optional subset of families}

\item{subset_genus}{Optional subset of genus}

\item{subset_species}{Optional subset of species}

\item{subset_year}{Optional subset of year}

\item{subset_habitat}{Optional subset of habitat type ("Forest" or "Semi-nat")}

\item{trap_type}{Which trap types to fetch data for. "All" (defult), "MF", or "VF".}

\item{subset_region}{Optional subset of region. Currently allowing "Trøndelag", "Østlandet", "Sørlandet"}

\item{dataset}{Which dataset to fetch data for. Default "NorIns".}

\item{agg_level}{Aggregate level of data. "year_locality" (default), "locality_sampling", "total".}

\item{as_tibble}{Return as tibble? Boolean}
}
\value{
A tibble of sample weights
}
\description{
get_biomass Get biomass data from the database
}
\examples{
\dontrun{

connect_to_insect_db()
biomass <- get_biomass(
  agg_level = "year_locality",
  trap_type = "MF"
)
}

}
