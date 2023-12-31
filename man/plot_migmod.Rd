% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_migmod.R
\name{plot_migmod}
\alias{plot_migmod}
\title{Function to plot synthetic migration estimates}
\usage{
plot_migmod(
  df,
  mmfit,
  sending = c("SE", "FI", "IT", "PL"),
  receiving = c("SE", "FI", "IT", "PL"),
  years = 2010:2019
)
}
\arguments{
\item{df}{A data frame with migration data for sending countries, receiving countries, data quality measures and covariates}

\item{mmfit}{A \code{stanfit} object produced by the function \code{migmod_m} that contains estimates of the model parameters and synthetic estimates of the "true" migration.}

\item{sending}{A vector of ISO2 codes with sending countries to be used in the model (recommended to keep SE and FI)}

\item{receiving}{A vector of ISO2 codes with receiving countries to be used in the model (recommended to keep SE and FI)}

\item{years}{A vector of years to be used in the model (2010 to 2019)}
}
\value{
An object of class \code{ggplot}.
}
\description{
Function to plot synthetic migration estimates
}
