#include <RcppArmadillo.h>
#include <RcppParallel.h>

using namespace Rcpp;


struct Rec
{
    int item;      // recommended item: use item index no. rather than text label, to avoid copying chars around
    double score;  // recommended score

    Rec(int item, double score) : item(item), score(score) {}

    static bool score_comp(const Rec& a, const Rec& b)
    {
        return a.score > b.score;
    }

    static bool item_comp(const Rec& a, const Rec& b)
    {
        return a.item > b.item;
    }
};


struct Rank_scores : public RcppParallel::Worker
{
    // inputs
    const arma::sp_mat& aff;
    const arma::sp_mat& sim;
    const int n_recs;
    const bool include_seed_items;
    const bool backfill;
    std::vector<Rec> popular_items;

    // outputs
    RcppParallel::RMatrix<double> rec_scores;
    RcppParallel::RMatrix<int> rec_items;

    // derived
    const int n_users;
    const int n_items;

    Rank_scores(const arma::sp_mat& aff, const arma::sp_mat& sim,
        const int n_recs, const bool include_seed_items, const bool backfill, const IntegerVector& pop_items,
        NumericMatrix& rec_scores, IntegerMatrix& rec_items) :

        aff(aff), sim(sim), n_recs(n_recs),
        include_seed_items(include_seed_items), backfill(backfill),
        rec_scores(rec_scores), rec_items(rec_items),
        n_users(aff.n_cols), n_items(aff.n_rows)
    {
        int n_pop = pop_items.length();
        popular_items.reserve(n_pop);
        for (int i = 0; i < n_pop; i++)
        {
            popular_items.emplace_back(pop_items[i], n_pop - i); // score here must be sorted in decreasing order
        }
        // pre-sort by item no. for input to std::set_difference later
        std::sort(popular_items.begin(), popular_items.end(), Rec::item_comp);
    }

    void operator()(size_t begin, size_t end)
    {
        // create dense matrices from sparse inputs: important for speed of accessing elements
        const arma::mat aff_dens(aff.cols(begin, end - 1));

        // do NOT use above matrix in this constructor: sparse * dense is slow
        const arma::mat scores(sim * aff.cols(begin, end - 1));

        const size_t chunksize = end - begin;
        for (size_t offset = 0; offset < chunksize; offset++)
        {
            const size_t i = begin + offset;
            const arma::vec score_i(scores.col(offset));
            const arma::vec aff_i(aff_dens.col(offset));

            std::vector<Rec> user_rec;
            user_rec.reserve(n_items);

            if (!include_seed_items)
            {
                // only keep recs that correspond to zero item affinities
                for (int j = 0; j < n_items; j++)
                {
                    if (aff_i[j] == 0)
                    {
                        user_rec.emplace_back(j, score_i[j]);
                    }
                }
                // make sure we don't return garbage: must have at least n_recs items
                for (int j = user_rec.size(); j < n_recs; j++)
                {
                    user_rec.emplace_back(0, 0.0);
                }
            }
            else
            {
                for (int j = 0; j < n_items; j++)
                {
                    user_rec.emplace_back(i, score_i[j]);
                }
            }

            std::sort(user_rec.begin(), user_rec.end(), Rec::score_comp);

            if (backfill && user_rec[n_recs - 1].score == 0)
            {
                backfill_recs(user_rec, aff_i);
            }

            for (int j = 0; j < n_recs; j++)
            {
                rec_scores(i, j) = user_rec[j].score;
                rec_items(i, j) = user_rec[j].item + 1;  // convert to 1-based indexing for R
            }
        }
    }

    // check for zero scores, replace with popular items
    // must exclude items that have already been recommended, optionally also items with nonzero affinity
    // if we are in here, we have at least one zero score in the top K
    void backfill_recs(std::vector<Rec>& recs, const arma::vec& user_aff)
    {
        std::vector<Rec> seen_items, unseen_popular_items;

        int firstzero = 0;
        seen_items.reserve(include_seed_items ? n_recs : n_items);
        for (; firstzero < n_recs && recs[firstzero].score != 0; firstzero++)
        {
            seen_items.push_back(recs[firstzero]);
        }

        // if required, expand the set of ineligible items to include those for which user has nonzero affinity
        if (!include_seed_items)
        {
            for (int i = 0; i < n_items; i++)
            {
                if (user_aff[i] > 0)
                {
                    seen_items.emplace_back(i, user_aff[i]);
                }
            }
        }

        // std::set_difference requires inputs to be sorted by comparison criterion (item no.)
        std::sort(seen_items.begin(), seen_items.end(), Rec::item_comp);

        std::set_difference(popular_items.begin(), popular_items.end(),
            seen_items.begin(), seen_items.end(),
            std::inserter(unseen_popular_items, unseen_popular_items.begin()),
            Rec::item_comp);

        std::sort(unseen_popular_items.begin(), unseen_popular_items.end(), Rec::score_comp);

        for (size_t i = firstzero, j = 0; i < recs.size() && j < unseen_popular_items.size(); i++, j++)
        {
            recs[i].item = unseen_popular_items[j].item;
        }
    }
};


// [[Rcpp::export]]
List user_predict_ranking(arma::sp_mat& aff, arma::sp_mat& sim,
    const int n_recs, const bool include_seed_items, const bool backfill, const IntegerVector& pop_items)
{
    const int n_users = aff.n_cols;
    const int chunksize = 100;

    NumericMatrix rec_scores(n_users, n_recs);
    IntegerMatrix rec_items(n_users, n_recs);

    Rank_scores rank_scores(aff, sim, n_recs, include_seed_items, backfill, pop_items, rec_scores, rec_items);
    RcppParallel::parallelFor(0, n_users, rank_scores, chunksize);

    return List::create(rec_scores, rec_items);
}

