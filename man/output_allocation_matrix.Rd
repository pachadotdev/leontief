\name{Output allocation matrix}
\alias{output_allocation_matrix}
\title{Output allocation matrix}
\description{
  Returns output allocation matrix after passing an input-output matrix
  and a final demand vector as input.
}
\usage{
output_allocation_matrix(X,d)
}
\arguments{
  \item{X}{input-output matrix}
  \item{d}{final demand vector}
}
\value{
  \code{output_allocation_matrix()} returns a matrix, when the X matrix is not a
  square matrix or the d vector has a different number of elements than the
  number of rows in X it returns an error.
}
\references{
  Suahasil Nazara & Dong Guo & Geoffrey J.D. Hewings & Chokri Dridi, 2004. 
  "PyIO: Input-Output Analysis with Python," Urban/Regional 0409002, University
  of Illinois at Urbana-Champaign, USA. 
}
\examples{
  set.seed(200100)
  X <- matrix(rnorm(100), nrow = 10)
  d <- rnorm(10)
  output_allocation_matrix(X,d)
}
\author{Mauricio Vargas}
