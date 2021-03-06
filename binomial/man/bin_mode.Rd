% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binom.R
\name{bin_mode}
\alias{bin_mode}
\title{Binomial Mode Function}
\usage{
bin_mode(trials, prob)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{success probability}
}
\value{
mode, as a numeric value. 2 modes if value is an exact integer.
}
\description{
Computes mode for binomial variable with given trials and prob. Uses auxiliary function.
}
\examples{
bin_mode(10, 0.3)
}
