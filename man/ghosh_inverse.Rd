\name{Ghosh inverse matrix}
\alias{ghosh_inverse}
\title{ghosh inverse matrix}
\description{
  Returns Ghosh inverse matrix after passing an output allocation matrix
  as input.
}
\usage{
ghosh_inverse(B)
}
\arguments{
  \item{B}{output allocation matrix}
}
\value{
  \code{ghosh_inverse()} returns a matrix, when the B matrix is not square
  or the Ghosh matrix can't be inverted it returns an error.
}
\references{
  Suahasil Nazara & Dong Guo & Geoffrey J.D. Hewings & Chokri Dridi, 2004. 
  "PyIO: Input-Output Analysis with Python," Urban/Regional 0409002, University
  of Illinois at Urbana-Champaign, USA. 
}
\examples{
  set.seed(200100)
  B <- matrix(rnorm(100), nrow = 10)
  ghosh_inverse(B)
}
\author{Mauricio Vargas}
