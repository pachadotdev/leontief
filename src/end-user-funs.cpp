// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

//' Input requirement matrix
//' @param X input-output matrix
//' @param d final demand vector
//' @examples
//' set.seed(200100)
//' X <- matrix(rnorm(100), nrow = 10)
//' d <- rnorm(10)
//' input_requirement_matrix(X,d)
//' @export
// [[Rcpp::export]]
arma::mat input_requirement_matrix(const arma::mat & X, const arma::vec & d) {
    bool X_square = (X.n_rows == X.n_cols);
    bool compatible_dimensions = (X.n_rows == d.n_elem);
    if(X_square == FALSE) {
        Rcpp::stop("Input-output matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        Rcpp::stop("d is required to have the same number of elements as the number of rows in X.");
    }
    
    arma::mat D = arma::zeros<arma::mat>(d.n_elem,d.n_elem);
    D.each_col() += d;
    D = D.t();
    return (X / D);
}

//' Output allocation matrix
//' @param X input-output matrix
//' @param d final demand vector
//' @examples
//' set.seed(200100)
//' X <- matrix(rnorm(100), nrow = 10)
//' d <- rnorm(10)
//' output_allocation_matrix(X,d)
//' @export
// [[Rcpp::export]]
arma::mat output_allocation_matrix(const arma::mat & X, const arma::vec & d) {
    bool X_square = (X.n_rows == X.n_cols);
    bool compatible_dimensions = (X.n_rows == d.n_elem);
    if(X_square == FALSE) {
        Rcpp::stop("Input-output matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        Rcpp::stop("d is required to have the same number of elements as the number of rows in X.");
    }
    
    arma::mat D = arma::zeros<arma::mat>(d.n_elem,d.n_elem);
    D.each_col() += d;
    return (X / D);
}

//' Leontief inverse matrix
//' @param A input requirement matrix
//' @examples
//' set.seed(200100)
//' A <- matrix(rnorm(100), nrow = 10)
//' leontief_inverse(A)
//' @export
// [[Rcpp::export]]
arma::mat leontief_inverse(const arma::mat & A) {
    bool A_square = (A.n_rows == A.n_cols);
    if(A_square == FALSE) {
        Rcpp::stop("Input requirement matrix must be square.");
    }
    
    arma::mat I = arma::eye<arma::mat>(A.n_rows,A.n_cols);
    return arma::inv(I - A);
}

//' Ghosh inverse matrix
//' @param B output allocation matrix
//' @examples
//' set.seed(200100)
//' B <- matrix(rnorm(100), nrow = 10)
//' leontief_inverse(B)
//' @export
// [[Rcpp::export]]
arma::mat ghosh_inverse(const arma::mat & B) {
    bool B_square = (B.n_rows == B.n_cols);
    if(B_square == FALSE) {
        Rcpp::stop("Output allocation matrix must be square.");
    }
    
    arma::mat I = arma::eye<arma::mat>(B.n_rows,B.n_cols);
    return arma::inv(I - B);
}
