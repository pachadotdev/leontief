
# inputoutput

<!-- badges: start -->
[![R build status](https://github.com/pachamaltese/inputoutput/workflows/R-CMD-check/badge.svg)](https://github.com/pachamaltese/inputoutput/actions)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test coverage](https://codecov.io/gh/pachamaltese/inputoutput/branch/master/graph/badge.svg)](https://codecov.io/gh/pachamaltese/inputoutput?branch=master)
<!-- badges: end -->

The goal of inputoutput is to provide functions for Input-Output (Macroeconomics)
analysis.

## Installation

This package is not available from CRAN at the present time. It can be installed
from GitHub by running:
``` r
source("https://install-github.me/pachamaltese/inputoutput")
```

## Example

This is a basic example which shows you how to obtain the input requirement matrix:

``` r
library(inputoutput)

# use a real input-output matrix and final demand vector
set.seed(200100)
X <- matrix(rnorm(100), nrow = 10)
d <- rnorm(10)

input_requirement_matrix(X,d)
```

## Code of conduct

Please note that the 'inputoutput' project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
