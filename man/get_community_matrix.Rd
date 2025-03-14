% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_community_matrix.R
\name{get_community_matrix}
\alias{get_community_matrix}
\title{get_community_matrix}
\usage{
get_community_matrix(
  limit = NULL,
  id_type = c("metabarcoding"),
  trap_type = "MF",
  dataset = "NorIns",
  subset_years = NULL,
  subset_orders = NULL,
  subset_families = NULL,
  subset_species = NULL,
  subset_habitat = NULL,
  subset_region = NULL,
  exclude_singletons = F,
  transposed_matrix = F,
  as_tibble = F
)
}
\arguments{
\item{limit}{Optional row limit on output (for testing).}

\item{id_type}{Type of identification data. Currently only default "metabarcoding" available.}

\item{trap_type}{Optional subset of trap types. "MF" (default), "VF", "All".}

\item{dataset}{Optional selection of dataset. Default to "NorIns" for national insect monitoring scheme.}

\item{subset_years}{Optional subset of years. Numerical vector.}

\item{subset_orders}{Optional subset of orders. Character vector.}

\item{subset_families}{Optional subset of families Character vector.}

\item{subset_species}{Optional subset of families. Character vector.}

\item{subset_habitat}{Optional subset of habitat type. Character vector.}

\item{subset_region}{Optional subset of region. Character vector.}

\item{exclude_singletons}{Should we exclude singletons (species only found once). Boolean.}

\item{transposed_matrix}{Transpose matrix? Boolean.}

\item{as_tibble}{Output as tibble? Boolean.}
}
\value{
A tibble or dataframe of a community matrix
}
\description{
get_community_matrix
}
\examples{
\dontrun{

connect_to_insect_db()

beetles_comm_2022 <- get_community_matrix(
  subset_orders = "Coleoptera",
  subset_years = 2021
)
}

}
