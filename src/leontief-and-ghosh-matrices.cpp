// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

//' Input requirement
//' @param X transaction matrix
//' @param d final demand vector
//' @examples
//' set.seed(200100)
//' X <- matrix(rnorm(100), nrow = 10)
//' d <- rnorm(10)
//' input_requirement(X,d)
//' @export
// [[Rcpp::export]]
arma::mat input_requirement(const arma::mat & X, const arma::vec & d) {
    bool X_square = (X.n_rows == X.n_cols);
    bool compatible_dimensions = (X.n_rows == d.n_elem);
    if(X_square == FALSE) {
        Rcpp::stop("Transaction matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        Rcpp::stop("d is required to have the same number of elements as the number of rows in X.");
    }
    
    arma::mat D = arma::zeros<arma::mat>(d.n_elem,d.n_elem);
    D.each_col() += d;
    D = D.t();
    return (X / D);
}

//' Augmented input requirement
//' @param X transaction matrix
//' @param w wage vector
//' @param c household consumption vector
//' @param d final demand vector
//' @examples
//' set.seed(200100)
//' X <- matrix(rnorm(100), nrow = 10)
//' w <- rnorm(10)
//' c <- rnorm(10)
//' d <- rnorm(10)
//' augmented_input_requirement(X,w,c,d)
//' @export
// [[Rcpp::export]]
arma::mat augmented_input_requirement(const arma::mat & X, const arma::vec & w,
                            const arma::vec & c, const arma::vec & d) {
    bool X_square = (X.n_rows == X.n_cols);
    bool compatible_dimensions = ((X.n_rows == w.n_elem) &
                                  (X.n_rows == c.n_elem) &
                                  (X.n_rows == d.n_elem));
    if(X_square == FALSE) {
        Rcpp::stop("Transaction matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        Rcpp::stop("w,c,d are required to have the same number of elements as the number of rows in X.");
    }
    
    arma::mat D = arma::zeros<arma::mat>(d.n_elem,d.n_elem);
    D.each_col() += d;
    D = D.t();
    
    arma::mat Ac = arma::zeros<arma::mat>(c.n_elem,1);
    Ac.each_col() += c / d;
    arma::mat Ar = arma::zeros<arma::mat>(w.n_elem,1);
    Ar.each_col() += w / d;
    Ar.insert_rows(Ar.n_rows, arma::zeros<arma::mat>(1,1));
    
    arma::mat A = X / D;
    A.insert_cols(A.n_rows,Ac);
    A.insert_rows(A.n_rows,Ar.t());
    return A;
}

//' Output allocation
//' @param X transaction matrix
//' @param d final demand vector
//' @examples
//' set.seed(200100)
//' X <- matrix(rnorm(100), nrow = 10)
//' d <- rnorm(10)
//' output_allocation(X,d)
//' @export
// [[Rcpp::export]]
arma::mat output_allocation(const arma::mat & X, const arma::vec & d) {
    bool X_square = (X.n_rows == X.n_cols);
    bool compatible_dimensions = (X.n_rows == d.n_elem);
    if(X_square == FALSE) {
        Rcpp::stop("Transaction matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        Rcpp::stop("d is required to have the same number of elements as the number of rows in X.");
    }
    
    arma::mat D = arma::zeros<arma::mat>(d.n_elem,d.n_elem);
    D.each_col() += d;
    return (X / D);
}

//' Leontief inverse
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

//' Ghosh inverse
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

//' Equilibrium output
//' @param L Leontief inverse matrix
//' @param d final demand vector
//' @examples
//' set.seed(200100)
//' L <- matrix(rnorm(100), nrow = 10)
//' d <- rnorm(10)
//' equilibrium_output(L,d)
//' @export
// [[Rcpp::export]]
arma::mat equilibrium_output(const arma::mat & L, const arma::vec & d) {
    bool L_square = (L.n_rows == L.n_cols);
    bool compatible_dimensions = (L.n_rows == d.n_elem);
    if(L_square == FALSE) {
        Rcpp::stop("Leontief inverse matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        Rcpp::stop("d is required to have the same number of elements as the number of rows in L.");
    }
    
    return L * d;
}
