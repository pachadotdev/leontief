% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leontief-matrices.R
\name{output_allocation}
\alias{output_allocation}
\title{Output allocation}
\usage{
output_allocation(X, d)
}
\arguments{
\item{X}{transaction matrix}

\item{d}{final demand vector}
}
\description{
Output allocation
}
\examples{
set.seed(200100)
X <- matrix(rnorm(100), nrow = 10)
d <- rnorm(10)
output_allocation(X, d)
}
