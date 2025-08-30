#include <cpp11.hpp>
#include <cpp11armadillo.hpp>

using namespace arma;
using namespace cpp11;

[[cpp11::register]] doubles_matrix<> input_requirement_(const doubles_matrix<> &Xr, const doubles&dr) {
    mat X = as_mat(Xr);
    mat d = as_col(dr);

    bool X_square = (X.n_rows == X.n_cols);
    bool compatible_dimensions = (X.n_rows == d.n_elem);
    if(X_square == FALSE) {
        stop("Transaction matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        stop("d is required to have the same number of elements as the number of rows in X.");
    }
    
    mat D = zeros<mat>(d.n_elem,d.n_elem);
    D.each_col() += d;
    D = D.t();

    mat res = X / D;
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> augmented_input_requirement_(const doubles_matrix<> &Xr, const doubles&wr,
                            const doubles&cr, const doubles&dr) {
    mat X = as_mat(Xr);
    mat w = as_col(wr);
    mat c = as_col(cr);
    mat d = as_col(dr);

    bool X_square = (X.n_rows == X.n_cols);
    bool compatible_dimensions = ((X.n_rows == w.n_elem) &
                                  (X.n_rows == c.n_elem) &
                                  (X.n_rows == d.n_elem));
    if(X_square == FALSE) {
        stop("Transaction matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        stop("w,c,d are required to have the same number of elements as the number of rows in X.");
    }
    
    mat D = zeros<mat>(d.n_elem,d.n_elem);
    D.each_col() += d;
    D = D.t();
    
    mat Ac = zeros<mat>(c.n_elem,1);
    Ac.each_col() += c / d;
    mat Ar = zeros<mat>(w.n_elem,1);
    Ar.each_col() += w / d;
    Ar.insert_rows(Ar.n_rows, zeros<mat>(1,1));
    
    mat A = X / D;
    A.insert_cols(A.n_rows,Ac);
    A.insert_rows(A.n_rows,Ar.t());
    
    return as_doubles_matrix(A);
}

[[cpp11::register]] doubles_matrix<> output_allocation_(const doubles_matrix<> &Xr, const doubles&dr) {
    mat X = as_mat(Xr);
    mat d = as_col(dr);

    bool X_square = (X.n_rows == X.n_cols);
    bool compatible_dimensions = (X.n_rows == d.n_elem);
    if(X_square == FALSE) {
        stop("Transaction matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        stop("d is required to have the same number of elements as the number of rows in X.");
    }
    
    mat D = zeros<mat>(d.n_elem,d.n_elem);
    D.each_col() += d;

    mat res = X / D;
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> leontief_inverse_(const doubles_matrix<> &Ar) {
    mat A = as_mat(Ar);

    bool A_square = (A.n_rows == A.n_cols);
    if(A_square == FALSE) {
        stop("Input requirement matrix must be square.");
    }
    
    mat I = eye<mat>(A.n_rows,A.n_cols);

    mat res = inv(I - A);
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> equilibrium_output_(const doubles_matrix<> &Lr, const doubles&dr) {
    mat L = as_mat(Lr);
    mat d = as_col(dr);

    bool L_square = (L.n_rows == L.n_cols);
    bool compatible_dimensions = (L.n_rows == d.n_elem);
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        stop("d is required to have the same number of elements as the number of rows in L.");
    }
    
    mat res = L * d;
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> output_multiplier_(const doubles_matrix<> &Lr) {
    mat L = as_mat(Lr);

    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    
    mat res = sum(L, 0).t();
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> income_multiplier_(const doubles_matrix<> &Lr, const doubles&wr) {
    mat L = as_mat(Lr);
    mat w = as_col(wr);

    bool L_square = (L.n_rows == L.n_cols);
    bool compatible_dimensions = (L.n_rows == w.n_elem);
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        stop("w is required to have the same number of elements as the number of rows in L.");
    }
    
    mat W = zeros<mat>(w.n_elem,w.n_elem);
    W.diag() += w;
    
    mat res = sum(W * L,0).t();
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> employment_multiplier_(const doubles_matrix<> &Lr, const doubles&er) {
    mat L = as_mat(Lr);
    mat e = as_col(er);

    bool L_square = (L.n_rows == L.n_cols);
    bool compatible_dimensions = (L.n_rows == e.n_elem);
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        stop("e is required to have the same number of elements as the number of rows in L.");
    }
    
    mat E = zeros<mat>(e.n_elem,e.n_elem);
    E.diag() += e;
    
    mat res = sum(E * L,0).t();
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> employment_number_(const doubles_matrix<> &Lr, const doubles&er, const doubles&cr) {
    mat L = as_mat(Lr);
    mat e = as_col(er);
    mat c = as_col(cr);

    bool L_square = (L.n_rows == L.n_cols);
    bool compatible_dimensions = ((L.n_rows == e.n_elem) &(L.n_rows == c.n_elem));
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    if(compatible_dimensions == FALSE) {
        stop("e and c are required to have the same number of elements as the number of rows in L.");
    }
    
    mat E = zeros<mat>(e.n_elem,e.n_elem);
    E.diag() += e;
    
    mat res = (E * L) * c;
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> backward_linkage_(const doubles_matrix<> &Ar) {
    mat A = as_mat(Ar);

    bool A_square = (A.n_rows == A.n_cols);
    if(A_square == FALSE) {
        stop("Input requirement matrix must be square.");
    }
    
    mat res = sum(A,0).t();
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> forward_linkage_(const doubles_matrix<> &Ar) {
    mat A = as_mat(Ar);

    bool A_square = (A.n_rows == A.n_cols);
    if(A_square == FALSE) {
        stop("Input requirement matrix must be square.");
    }
    
    mat res = sum(A,1);
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> power_dispersion_(const doubles_matrix<> &Lr) {
    mat L = as_mat(Lr);

    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    
    double V = as_scalar(sum(vectorise(L)));

    mat res = (sum(L,0) * (L.n_rows / V)).t();
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> power_dispersion_cv_(const doubles_matrix<> &Lr) {
    mat L = as_mat(Lr);

    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    
    mat m = sum(L,0) / L.n_rows;
    mat S = L;
    S.each_row() -= m;
    S = sqrt(sum(S % S, 1) / (L.n_rows - 1));
    
    mat res = S / m.t();
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> sensitivity_dispersion_(const doubles_matrix<> &Lr) {
    mat L = as_mat(Lr);

    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    
    double V = as_scalar(sum(vectorise(L)));

    mat res = sum(L,1) * (L.n_rows / V);
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> sensitivity_dispersion_cv_(const doubles_matrix<> &Lr) {
    mat L = as_mat(Lr);

    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    
    mat m = sum(L,1) / L.n_rows;
    mat S = L;
    S.each_col() -= m;
    S = sqrt(sum(S % S, 0) / (L.n_rows - 1));
    
    mat res = S.t() / m;
    return as_doubles_matrix(res);
}

[[cpp11::register]] doubles_matrix<> multiplier_product_matrix_(const doubles_matrix<> &Lr) {
    mat L = as_mat(Lr);

    bool L_square = (L.n_rows == L.n_cols);
    if(L_square == FALSE) {
        stop("Leontief inverse matrix must be square.");
    }
    
    double V = as_scalar(sum(vectorise(L)));

    mat res = (sum(L,0).t() * sum(L,1).t()) / V;
    return as_doubles_matrix(res);
}
