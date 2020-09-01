
# leontief

<!-- badges: start -->
[![R build status](https://github.com/pachamaltese/leontief/workflows/R-CMD-check/badge.svg)](https://github.com/pachamaltese/leontief/actions)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Codecov test coverage](https://codecov.io/gh/pachamaltese/leontief/branch/master/graph/badge.svg)](https://codecov.io/gh/pachamaltese/leontief?branch=master)
<!-- badges: end -->

The goal of leontief is to provide an implementation of the Input-Output 
model developed by Wassily Leontief. It represents the interdependencies between 
different sectors of a national economy or different regional economies.

## Installation

This package is not available from CRAN at the present time. It can be installed
from GitHub by running:
``` r
source("https://install-github.me/pachamaltese/leontief")
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

## Code of conduct

Please note that the 'leontief' project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
