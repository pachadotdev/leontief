\name{Leontief inverse matrix}
\alias{leontief_inverse}
\title{Leontief inverse matrix}
\description{
  Returns leontief inverse matrix after passing an input requirement matrix
  as input.
}
\usage{
leontief_inverse(A)
}
\arguments{
  \item{A}{input requirement matrix}
}
\value{
  \code{leontief_inverse()} returns a matrix, when the A matrix is not square
  or the Leontief matrix can't be inverted it returns an error.
}
\references{
  Suahasil Nazara & Dong Guo & Geoffrey J.D. Hewings & Chokri Dridi, 2004. 
  "PyIO: Input-Output Analysis with Python," Urban/Regional 0409002, University
  of Illinois at Urbana-Champaign, USA. 
}
\examples{
  set.seed(200100)
  A <- matrix(rnorm(100), nrow = 10)
  leontief_inverse(A)
}
\author{Mauricio Vargas}
