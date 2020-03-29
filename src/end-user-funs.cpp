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

//' Household income multiplier
//' @param L Leontief inverse matrix
//' @param w wage vector
//' @export
// [[Rcpp::export]]
arma::mat household_income_multiplier(const arma::mat & L, const arma::vec & w) {
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
    
    return arma::sum((L % W), 0).t();
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

//' Backward linkage
//' @param L Leontief inverse matrix
//' @export
// [[Rcpp::export]]
arma::mat backward_linkage(const arma::mat & L) {
    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        Rcpp::stop("Leontief inverse matrix must be square.");
    }
    
    double V = arma::as_scalar(arma::sum(arma::vectorise(L)));
    return (arma::sum(L,0) * (L.n_rows / V)).t();
}

//' Backward linkage coefficient of variation
//' @param L Leontief inverse matrix
//' @export
// [[Rcpp::export]]
arma::mat backward_linkage_cv(const arma::mat & L) {
    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        Rcpp::stop("Leontief inverse matrix must be square.");
    }
    
    arma::mat m = arma::sum(L,0) / L.n_rows;
    arma::mat S = L;
    S.each_row() -= m;
    S = sqrt(arma::sum(S % S, 1) / (L.n_rows - 1));
    
    return(S / m.t());
}

//' Forward linkage
//' @param L Leontief inverse matrix
//' @export
// [[Rcpp::export]]
arma::mat forward_linkage(const arma::mat & L) {
    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        Rcpp::stop("Leontief inverse matrix must be square.");
    }
    
    double V = arma::as_scalar(arma::sum(arma::vectorise(L)));
    return (arma::sum(L,1) * (L.n_rows / V));
}

//' Forward linkage coefficient of variation
//' @param L Leontief inverse matrix
//' @export
// [[Rcpp::export]]
arma::mat forward_linkage_cv(const arma::mat & L) {
    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        Rcpp::stop("Leontief inverse matrix must be square.");
    }
    
    arma::mat m = arma::sum(L,1) / L.n_rows;
    arma::mat S = L;
    S.each_col() -= m;
    S = sqrt(arma::sum(S % S, 0) / (L.n_rows - 1));
    
    return(S.t() / m);
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
