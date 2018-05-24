#include <RcppArmadillo.h>

using namespace Rcpp;


// [[Rcpp::export]]
arma::sp_mat make_similarity_matrix_sp(int n_items, List groups, IntegerVector items)
{
    int n_users = groups.length();

    std::vector<unsigned int> rowind;
    arma::uvec colptr(n_users + 1);

    rowind.reserve(n_users);
    colptr[0] = 0;

    for (int g = 0; g < n_users; g++)
    {
        IntegerVector user = groups[g];
        std::vector<int> user_items(user.length(), 0);

        for (int i = 0; i < user.length(); i++)
        {
            user_items[i] = items[user[i]] - 1; // -1 converts to 0-based indexing for C++
        }

        // remove duplicated items for this user
        std::sort(user_items.begin(), user_items.end());
        std::vector<int>::iterator last = std::unique(user_items.begin(), user_items.end());
        user_items.erase(last, user_items.end());

        // append this user's values to the matrix vectors
        rowind.insert(rowind.end(), user_items.begin(), user_items.end());
        colptr[g + 1] = colptr[g] + user_items.size();
    }

    // vals is a vector of 1's, allocate it all in one hit
    arma::vec vals(rowind.size(), arma::fill::ones);

    // compressed sparse column representation of the item x user indicator matrix
    // take advantage of the fact that each user vector encodes the row-positions of the seen items
    arma::sp_mat item_user_mat(arma::conv_to<arma::uvec>::from(rowind), colptr, vals, n_items, n_users);

    // create result as sparse matrix multiply of item-user matrix with its transpose
    return item_user_mat * item_user_mat.t();
}


