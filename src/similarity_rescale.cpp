#include <RcppArmadillo.h>


// [[Rcpp::export]]
arma::sp_mat rescale_to_jaccard(arma::sp_mat& mat)
{
    const arma::vec diag(mat.diag());

    for (arma::sp_mat::iterator it = mat.begin(); it != mat.end(); ++it)
    {
        size_t row = it.row();
        size_t col = it.col();
        *it = *it / (diag[row] + diag[col] - *it);
    }
    return mat;
}


// [[Rcpp::export]]
arma::sp_mat rescale_to_lift(arma::sp_mat& mat)
{
    const arma::vec diag(mat.diag());

    for (arma::sp_mat::iterator it = mat.begin(); it != mat.end(); ++it)
    {
        size_t row = it.row();
        size_t col = it.col();
        *it = *it / (diag[row] * diag[col]);
    }
    return mat;
}

