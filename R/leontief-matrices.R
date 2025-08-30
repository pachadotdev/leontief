#' Input requirement
#' @param X transaction matrix
#' @param d final demand vector
#' @examples
#' set.seed(200100)
#' X <- matrix(rnorm(100), nrow = 10)
#' d <- rnorm(10)
#' input_requirement(X, d)
#' @export
input_requirement <- function(X, d) {
  input_requirement_(X, d)
}

#' Augmented input requirement
#' @param X transaction matrix
#' @param w wage vector
#' @param c household consumption vector
#' @param d final demand vector
#' @examples
#' set.seed(200100)
#' X <- matrix(rnorm(100), nrow = 10)
#' w <- rnorm(10)
#' c <- rnorm(10)
#' d <- rnorm(10)
#' augmented_input_requirement(X, w, c, d)
#' @export
augmented_input_requirement <- function(X, w, c, d) {
  augmented_input_requirement_(X, w, c, d)
}

#' Output allocation
#' @param X transaction matrix
#' @param d final demand vector
#' @examples
#' set.seed(200100)
#' X <- matrix(rnorm(100), nrow = 10)
#' d <- rnorm(10)
#' output_allocation(X, d)
#' @export
output_allocation <- function(X, d) {
  output_allocation_(X, d)
}

#' Leontief inverse
#' @param A input requirement matrix
#' @examples
#' set.seed(200100)
#' A <- matrix(rnorm(100), nrow = 10)
#' leontief_inverse(A)
#' @export
leontief_inverse <- function(A) {
  leontief_inverse_(A)
}

#' Equilibrium output
#' @param L Leontief inverse matrix
#' @param d final demand vector
#' @examples
#' set.seed(200100)
#' L <- matrix(rnorm(100), nrow = 10)
#' d <- rnorm(10)
#' equilibrium_output(L, d)
#' @export
equilibrium_output <- function(L, d) {
  equilibrium_output_(L, d)
}

#' Output multiplier
#' @param L Leontief inverse matrix
#' @examples
#' set.seed(200100)
#' L <- matrix(rnorm(100), nrow = 10)
#' output_multiplier(L)
#' @export
output_multiplier <- function(L) {
  output_multiplier_(L)
}

#' Income multiplier
#' @param L Leontief inverse matrix
#' @param w wage vector
#' @export
income_multiplier <- function(L, w) {
  income_multiplier_(L, w)
}


#' Employment multiplier
#' @param L Leontief inverse matrix
#' @param e employment coefficients vector
#' @export
employment_multiplier <- function(L, e) {
  employment_multiplier_(L, e)
}

#' Employment number
#' @param L Leontief inverse matrix
#' @param e employment coefficients vector
#' @param c change in final demand
#' @export
employment_number <- function(L, e, c) {
  employment_number_(L, e, c)
}

#' Backward linkage
#' @param A input requirement matrix
#' @export
backward_linkage <- function(A) {
  backward_linkage_(A)
}

#' Forward linkage
#' @param A input requirement matrix
#' @export
forward_linkage <- function(A) {
  forward_linkage_(A)
}

#' Power of dispersion
#' @param L Leontief inverse matrix
#' @export
power_dispersion <- function(L) {
  power_dispersion_(L)
}

#' Power of dispersion coefficient of variation
#' @param L Leontief inverse matrix
#' @export
power_dispersion_cv <- function(L) {
  power_dispersion_cv_(L)
}

#' Sensitivity of dispersion coefficient of variation
#' @param L Leontief inverse matrix
#' @export
sensitivity_dispersion <- function(L) {
  sensitivity_dispersion_(L)
}

#' Sensititivy of dispersion coefficient of variation
#' @param L Leontief inverse matrix
#' @export
sensitivity_dispersion_cv <- function(L) {
  sensitivity_dispersion_cv_(L)
}

#' Multiplier product matrix
#' @param L Leontief inverse matrix
#' @export
multiplier_product_matrix <- function(L) {
  multiplier_product_matrix_(L)
}
