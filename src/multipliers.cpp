// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

//' Output multiplier
//' @param L Leontief inverse matrix
//' @examples
//' set.seed(200100)
//' L <- matrix(rnorm(100), nrow = 10)
//' output_multiplier(L)
//' @export
// [[Rcpp::export]]
arma::mat output_multiplier(const arma::mat & L) {
  bool L_square = (L.n_rows == L.n_cols);
  if(L_square == FALSE) {
    Rcpp::stop("Leontief inverse matrix must be square.");
  }
  
  return arma::sum(L, 0).t();
}

//' Income multiplier
//' @param L Leontief inverse matrix
//' @param w wage vector
//' @export
// [[Rcpp::export]]
arma::mat income_multiplier(const arma::mat & L, const arma::vec & w) {
  bool L_square = (L.n_rows == L.n_cols);
  bool compatible_dimensions = (L.n_rows == w.n_elem);
  if(L_square == FALSE) {
    Rcpp::stop("Leontief inverse matrix must be square.");
  }
  if(compatible_dimensions == FALSE) {
    Rcpp::stop("w is required to have the same number of elements as the number of rows in L.");
  }
  
  arma::mat W = arma::zeros<arma::mat>(w.n_elem,w.n_elem);
  W.each_col() += w;
  
  return (arma::sum((L % W), 0).t() / w);
}

//' Employment multiplier
//' @param L Leontief inverse matrix
//' @param e employment vector
//' @export
// [[Rcpp::export]]
arma::mat employment_multiplier(const arma::mat & L, const arma::vec & e) {
  bool L_square = (L.n_rows == L.n_cols);
  bool compatible_dimensions = (L.n_rows == e.n_elem);
  if(L_square == FALSE) {
    Rcpp::stop("Leontief inverse matrix must be square.");
  }
  if(compatible_dimensions == FALSE) {
    Rcpp::stop("e is required to have the same number of elements as the number of rows in L.");
  }
  
  arma::mat E = arma::zeros<arma::mat>(e.n_elem,e.n_elem);
  E.each_col() += e;
  
  return (arma::sum((L % E), 0).t() / e);
}

//' Input multiplier
//' @param G Ghosh inverse matrix
//' @examples
//' set.seed(200100)
//' G <- matrix(rnorm(100), nrow = 10)
//' output_multiplier(G)
//' @export
// [[Rcpp::export]]
arma::mat input_multiplier(const arma::mat & G) {
  bool G_square = (G.n_rows == G.n_cols);
  if(G_square == FALSE) {
    Rcpp::stop("Ghosh inverse matrix must be square.");
  }
  
  return arma::sum(G, 0).t();
}
