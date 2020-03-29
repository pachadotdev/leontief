// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

//' Backward linkage
//' @param A input requirement matrix
//' @export
// [[Rcpp::export]]
arma::mat backward_linkage(const arma::mat & A) {
  bool A_square = (A.n_rows == A.n_cols);
  if(A_square == FALSE) {
    Rcpp::stop("Input requirement matrix must be square.");
  }
  
  return (arma::sum(A,0).t());
}

//' Forward linkage
//' @param A input requirement matrix
//' @export
// [[Rcpp::export]]
arma::mat forward_linkage(const arma::mat & A) {
  bool A_square = (A.n_rows == A.n_cols);
  if(A_square == FALSE) {
    Rcpp::stop("Input requirement matrix must be square.");
  }
  
  return (arma::sum(A,1));
}

//' Power of dispersion
//' @param L Leontief inverse matrix
//' @export
// [[Rcpp::export]]
arma::mat power_dispersion(const arma::mat & L) {
  bool L_square = (L.n_rows == L.n_cols);
  if(L_square == FALSE) {
    Rcpp::stop("Leontief inverse matrix must be square.");
  }
  
  double V = arma::as_scalar(arma::sum(arma::vectorise(L)));
  return (arma::sum(L,0) * (L.n_rows / V)).t();
}

//' Power of dispersion coefficient of variation
//' @param L Leontief inverse matrix
//' @export
// [[Rcpp::export]]
arma::mat power_dispersion_cv(const arma::mat & L) {
  bool L_square = (L.n_rows == L.n_cols);
  if(L_square == FALSE) {
    Rcpp::stop("Leontief inverse matrix must be square.");
  }
  
  arma::mat m = arma::sum(L,0) / L.n_rows;
  arma::mat S = L;
  S.each_row() -= m;
  S = sqrt(arma::sum(S % S, 1) / (L.n_rows - 1));
  
  return (S / m.t());
}

//' Sensitivity of dispersion coefficient of variation
//' @param L Leontief inverse matrix
//' @export
// [[Rcpp::export]]
arma::mat sensitivity_dispersion(const arma::mat & L) {
  bool L_square = (L.n_rows == L.n_cols);
  if(L_square == FALSE) {
    Rcpp::stop("Leontief inverse matrix must be square.");
  }
  
  double V = arma::as_scalar(arma::sum(arma::vectorise(L)));
  return (arma::sum(L,1) * (L.n_rows / V));
}

//' Sensititivy of dispersion coefficient of variation
//' @param L Leontief inverse matrix
//' @export
// [[Rcpp::export]]
arma::mat sensitivity_dispersion_cv(const arma::mat & L) {
  bool L_square = (L.n_rows == L.n_cols);
  if(L_square == FALSE) {
    Rcpp::stop("Leontief inverse matrix must be square.");
  }
  
  arma::mat m = arma::sum(L,1) / L.n_rows;
  arma::mat S = L;
  S.each_col() -= m;
  S = sqrt(arma::sum(S % S, 0) / (L.n_rows - 1));
  
  return (S.t() / m);
}

//' Multiplier product matrix
//' @param L Leontief inverse matrix
//' @export
// [[Rcpp::export]]
arma::mat multiplier_product_matrix(const arma::mat & L) {
  bool L_square = (L.n_rows == L.n_cols);
  if(L_square == FALSE) {
    Rcpp::stop("Leontief inverse matrix must be square.");
  }
  
  double V = arma::as_scalar(arma::sum(arma::vectorise(L)));
  return ((arma::sum(L,0).t() * arma::sum(L,1).t()) / V);
}
