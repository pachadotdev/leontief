# leontief

<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/leontief)](https://cran.r-project.org/package=leontief)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test coverage](https://codecov.io/gh/pachadotdev/leontief/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pachadotdev/leontief?branch=master)
[![R-CMD-check](https://github.com/pachadotdev/leontief/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pachadotdev/leontief/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of leontief is to provide an implementation of the Input-Output 
model developed by Wassily Leontief. It represents the interdependencies between 
different sectors of a national economy or different regional economies.

## Installation

Stable version (CRAN):
``` r
install.packages("leontief")
```

Development version (GitHub):
``` r
remotes::install_github("pachadotdev/leontief")
```

## Example

This is a basic example which shows you how to obtain the input requirement matrix:

``` r
library(leontief)

# use a real input-output matrix and final demand vector
set.seed(200100)
X <- matrix(rnorm(100), nrow = 10)
d <- rnorm(10)

input_requirement_matrix(X,d)
```

Please read the vignette for the details.

## Code of Conduct

Please note that the leontief project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
