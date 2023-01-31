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
  X_square <- (nrow(X) == ncol(X))
  compatible_dimensions <- (nrow(X) == length(d))
  if (isFALSE(X_square)) {
    stop("Transaction matrix must be square.")
  }
  if (isFALSE(compatible_dimensions)) {
    stop("d is required to have the same number of elements as the number of rows in X.")
  }
  # for (j in 1:ncol(D)) {
  #   D[, j] <- d
  # }
  D <- t(apply(
    matrix(
      0,
      nrow = length(d), ncol = length(d)
    ), 2,
    function(x) {
      x + d
    }
  ))
  return(X / D)
}

#' Augmented input requirement
#'
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
  X_square <- (ncol(X) == nrow(X))
  compatible_dimensions <-
    (length(w) == nrow(X) &
      length(c) == nrow(X) & length(d) == nrow(X))
  if (isFALSE(X_square)) {
    stop("Transaction matrix must be square.")
  }
  if (isFALSE(compatible_dimensions)) {
    stop("w,c,d are required to have the same number of elements as the number of rows in X.")
  }
  D <- t(matrix(d, nrow = length(d), ncol = length(d)))
  Ac <- matrix(c / d, nrow = length(c), ncol = 1)
  Ar <- matrix(w / d, nrow = length(w), ncol = 1)
  Ar <- rbind(Ar, matrix(0, nrow = 1, ncol = 1))
  A <- cbind(X / D, Ac)
  A <- rbind(A, t(Ar))
  return(A)
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
  X_square <- (ncol(X) == nrow(X))
  compatible_dimensions <- (nrow(X) == length(d))
  if (isFALSE(X_square)) {
    stop("Transaction matrix must be square.")
  }
  if (isFALSE(compatible_dimensions)) {
    stop("d is required to have the same number of elements as the number of rows in X.")
  }
  D <- matrix(0, nrow(X), nrow(X))
  # for (i in 1:nrow(D)) {
  #   D[i, ] <- d
  # }
  D <- apply(
    matrix(0, nrow = length(d), ncol = length(d)), 2,
    function(x) {
      x + d
    }
  )
  return(X / D)
}

#' Leontief inverse
#' @param A input requirement matrix
#' @examples
#' set.seed(200100)
#' A <- matrix(rnorm(100), nrow = 10)
#' leontief_inverse(A)
#' @export
leontief_inverse <- function(A) {
  A_square <- (nrow(A) == ncol(A))
  if (isFALSE(A_square)) {
    stop("Input requirement matrix must be square.")
  }
  I <- diag(ncol(A))
  return(solve(I - A))
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
  L_square <- (nrow(L) == ncol(L))
  compatible_dimensions <- (nrow(L) == length(d))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  if (isFALSE(compatible_dimensions)) {
    stop("d is required to have the same number of elements as the number of rows in L.")
  }
  return(L %*% d)
}

#' Output multiplier
#' @param L Leontief inverse matrix
#' @examples
#' set.seed(200100)
#' L <- matrix(rnorm(100), nrow = 10)
#' output_multiplier(L)
#' @export
output_multiplier <- function(L) {
  L_square <- (nrow(L) == ncol(L))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  return(as.matrix(colSums(L)))
}

#' Income multiplier
#' @param L Leontief inverse matrix
#' @param w wage vector
#' @export
income_multiplier <- function(L, w) {
  L_square <- (nrow(L) == ncol(L))
  compatible_dimensions <- (nrow(L) == length(w))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  if (isFALSE(compatible_dimensions)) {
    stop("w is required to have the same number of elements as the number of rows in L.")
  }
  W <- diag(w)
  return(as.matrix(colSums(W %*% L)))
}


#' Employment multiplier
#' @param L Leontief inverse matrix
#' @param e employment coefficients vector
#' @export
employment_multiplier <- function(L, e) {
  L_square <- (nrow(L) == ncol(L))
  compatible_dimensions <- (nrow(L) == length(e))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  if (isFALSE(compatible_dimensions)) {
    stop("e is required to have the same number of elements as the number of rows in L.")
  }
  E <- diag(e)
  return(as.matrix(colSums(t(E) %*% L)))
}

#' Employment number
#' @param L Leontief inverse matrix
#' @param e employment coefficients vector
#' @param c change in final demand
#' @export
employment_number <- function(L, e, c) {
  L_square <- (nrow(L) == ncol(L))
  compatible_dimensions <- (nrow(L) == length(e))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  if (isFALSE(compatible_dimensions)) {
    stop("e and c are required to have the same number of elements as the number of rows in L.")
  }
  E <- diag(e)
  E %*% L %*% c
}

#' Backward linkage
#' @param A input requirement matrix
#' @export
backward_linkage <- function(A) {
  A_square <- (nrow(A) == ncol(A))
  if (isFALSE(A_square)) {
    stop("Input requirement matrix must be square.")
  }
  return(as.matrix(colSums(A)))
}

#' Forward linkage
#' @param A input requirement matrix
#' @export
forward_linkage <- function(A) {
  A_square <- (nrow(A) == ncol(A))
  if (isFALSE(A_square)) {
    stop("Input requirement matrix must be square.")
  }
  return(as.matrix(rowSums(A)))
}

#' Power of dispersion
#' @param L Leontief inverse matrix
#' @export
power_dispersion <- function(L) {
  L_square <- (nrow(L) == ncol(L))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  V <- sum(L)
  return(as.matrix(colSums(L) * (nrow(L) / V)))
}

#' Power of dispersion coefficient of variation
#' @param L Leontief inverse matrix
#' @export
power_dispersion_cv <- function(L) {
  L_square <- (nrow(L) == ncol(L))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  m <- colSums(L) / nrow(L)
  S <- apply(L, 1, function(x) {
    x - m
  })
  S <- sqrt(colSums(S^2) / (nrow(L) - 1))
  return(as.matrix(S / m))
}

#' Sensitivity of dispersion coefficient of variation
#' @param L Leontief inverse matrix
#' @export
sensitivity_dispersion <- function(L) {
  L_square <- (nrow(L) == ncol(L))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  V <- sum(L)
  return(as.matrix(rowSums(L) * (ncol(L) / V)))
}

#' Sensititivy of dispersion coefficient of variation
#' @param L Leontief inverse matrix
#' @export
sensitivity_dispersion_cv <- function(L) {
  L_square <- (nrow(L) == ncol(L))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  m <- rowSums(L) / nrow(L)
  S <- apply(L, 2, function(x) {
    x - m
  })
  S <- sqrt(colSums(S^2) / (nrow(L) - 1))
  return(as.matrix(S / m))
}

#' Multiplier product matrix
#' @param L Leontief inverse matrix
#' @export
multiplier_product_matrix <- function(L) {
  L_square <- (nrow(L) == ncol(L))
  if (isFALSE(L_square)) {
    stop("Leontief inverse matrix must be square.")
  }
  V <- sum(L)
  return((colSums(L) %*% t(rowSums(L))) / V)
}
