#' Fit a SAR model
#'
#' @param x A data frame.
#' @param user,item,time,event,weight For the default method, vectors to use as the user IDs, item IDs, timestamps, event types, and transaction weights for SAR. For the `data.frame` method, the names of the columns in the data frame `x` to use for these variables.
#' @param support_threshold The SAR support threshold. Items that do not occur at least this many times in the data will be considered "cold".
#' @param allowed_items A character or factor vector of allowed item IDs to use in the SAR model. If supplied, this will be used to categorise the item IDs in the data.
#' @param by_user Should the analysis be by user ID, or by user ID and timestamp? Defaults to userID only.
#' @param similarity Similarity metric to use; defaults to Jaccard.
#' @param half_life The decay period to use when weighting transactions by age.
#' @param catalog_data A dataset to use for building the cold-items feature model.
#' @param catalog_formula: A formula for the feature model used to compute similarities for cold items.
#' @param cold_to_cold: Whether the cold-items feature model should include the cold items themselves in the training data, or only warm items.
#' @param cold_item_model: The type of model to use for cold item features. 
#' @param ... Further arguments to pass to the cold-items feature model.
#' @details
#' Smart Adaptive Recommendations (SAR) is a fast, scalable, adaptive algorithm for personalized recommendations based on user transaction history and item descriptions. It produces easily explainable/interpretable recommendations and handles "cold item" and "semi-cold user" scenarios.
#'
#' Central to how SAR works is an item-to-item _co-occurrence matrix_, which is based on how many times two items occur for the same users. For example, if a given user buys items \eqn{i_1} and \eqn{i_2}, then the cell \eqn{(i_1, i_2)} is incremented by 1. From this, an item _similarity matrix_ can be obtained by rescaling the co-occurrences according to a given metric. Options for the metric include Jaccard (the default), lift, and counts (which means no rescaling).
#'
#' Note that the similarity matrix in SAR thus only includes information on which users transacted which items. It does not include any other information such as item ratings or features, which may be used by other recommender algorithms.
#'
#' #' The SAR implementation in R should be usable on datasets with up to a few million rows and several thousand items. The main constraint is the size of the similarity matrix, which in turn depends (quadratically) on the number of unique items. The implementation has been successfully tested on the MovieLens 20M dataset, which contains about 138,000 users and 27,000 items. For larger datasets, it is recommended to use the [Azure web service API][az_rec_service].
#'
#' @section Cold items:
#'
#' SAR has the ability to handle cold items, meaning those which have not been seen by any user, or which have only been seen by a number of users less than `support_threshold`. This is done by using item features to predict similarities. The method used for this is set by the `cold_items_model` argument:
#'
#' * If this is `NULL` (the default), a manual algorithm is used that correlates each feature in turn with similarity, and produces a predicted similarity based on which features two items have in common.
#' * If this is the name of a modelling function, such as `"lm"` or `"randomForest"`, a model of that type is fit on the features and used to predict similarity. In particular, use `"lm"` to get a model that is (approximately) equivalent to that used by the Azure web service API.
#'
#' The data frame and features used for cold items are given by the `catalog_data` and `catalog_formula` arguments. `catalog_data` should be a data frame whose first column is item ID. `catalog_formula` should be a one-sided formula (no LHS).
#'
#' This feature is currently experimental, and subject to change.
#'
#' @return
#' An S3 object representing the SAR model. This is essentially the item-to-item similarity matrix in sparse format, along with the original transaction data used to fit the model.
#'
#' @seealso
#' [Description of SAR](https://github.com/Microsoft/Product-Recommendations/blob/master/doc/sar.md) at the [Product Recommendations API repo](https://github.com/Microsoft/Product-Recommendations) on GitHub
#' @rdname sar
#' @export
sar <- function(...)
{
    UseMethod("sar")
}

#' @rdname sar
#' @export
sar.data.frame <- function(x, user="user", item="item", time="time", event="event", weight="weight", ...)
{
    model <- sar.default(user=x[[user]], item=x[[item]], time=x[[time]], event=x[[event]], weight=x[[weight]], ...)
    model$col_ids <- c(user=user, item=item, time=time, event=event, weight=weight)
    class(model) <- c("sar.data.frame", class(model))
    model
}


#' @rdname sar
#' @export
sar.default <- function(user, item, time, event=NULL, weight=NULL, support_threshold=1, allowed_items=NULL,
                        allowed_events=c(Click=1, RecommendationClick=2, AddShopCart=3, RemoveShopCart=-1, Purchase=4),
                        by_user=TRUE, similarity=c("jaccard", "lift", "count"), half_life=30,
                        catalog_data=NULL, catalog_formula=item ~ ., cold_to_cold=FALSE, cold_item_model=NULL, ...)
{
    if(missing(user) || is.null(user))
        stop("must supply column of user IDs")
    if(missing(item) || is.null(item))
        stop("must supply column of item IDs")
    if(missing(time) || is.null(time))
        stop("must supply column of event timestamps")

    if(!is.null(weight) || !is.null(event))
        message("Event types and weights are not using in training a SAR model")

    similarity <- match.arg(similarity)

    item <- if(is.null(allowed_items))
        as.factor(item)
    else factor(item, levels=sort(allowed_items))

    sim_mat <- make_similarity(user, item, time, support_threshold, by_user, similarity)
    pop_items <- attr(sim_mat, "pop_items")
    attr(sim_mat, "pop_items") <- NULL

    if(!is.null(catalog_data))
        sim_matrix <- get_cold_similarity(cold_item_model, sim_mat, catalog_formula, catalog_data, cold_to_cold,
                                          similarity, ...)

    out <- list(sim_mat=sim_mat, pop_items=pop_items,
                user=as.character(user), item=item, time=time, event=event, weight=weight, # save the data
                allowed_items=unique(allowed_items), allowed_events=allowed_events,
                by_user=by_user, support_threshold=support_threshold,
                half_life=half_life, similarity=similarity)
    class(out) <- "sar"
    out
}


make_similarity <- function(user, item, time, support_threshold, by_user, similarity)
{
    dat <- dplyr::data_frame(user, item, time)
    
    grps <- if(by_user)
        dplyr::quo(user)
    else c(dplyr::quo(user), dplyr::quo(time))

    # call out to C++ to compute actual matrix: 2 order of magnitude speedup
    sim_matrix <- make_similarity_matrix_sp(nlevels(item),
                                            attr(dplyr::group_by(dat, !!!grps), "indices"),
                                            item)

    # record popular items -- used for backfilling
    pop_items <- seq_len(nlevels(item))[order(diag(sim_matrix), decreasing=TRUE)] - 1

    # set all elements below support threshold to zero
    sim_matrix@x[sim_matrix@x < support_threshold] <- 0
    sim_matrix <- Matrix::drop0(sim_matrix)

    if(similarity == "lift")
        sim_matrix <- rescale_to_lift(sim_matrix)
    else if(similarity == "jaccard")
        sim_matrix <- rescale_to_jaccard(sim_matrix)

    dimnames(sim_matrix) <- list(levels(dat$item), levels(dat$item))

    attr(sim_matrix, "pop_items") <- pop_items
    sim_matrix
}


get_cold_similarity <- function(cold_item_model=NULL, sim_matrix, catalog_formula, catalog_data, cold_to_cold,
                                similarity, ...)
{
    # handle ~. formula correctly, assuming column 1 of catalog data is item ID
    if(any(all.vars(catalog_formula[[length(catalog_formula)]]) == "."))
    {
        vars <- names(catalog_data[-1])
        catalog_formula <- reformulate(vars)
    }

    cold_df <- if(is.null(cold_item_model))
        get_cold_similarity_nullmodel(sim_matrix, catalog_formula, catalog_data, cold_to_cold)
    else
    {
        if(is.character(cold_item_model))
            cold_item_model <- get(cold_item_model, mode="function")
        else if(!is.function(cold_item_model))
            stop("Must supply function or function name as string for cold item model")

        get_cold_similarity_model(sim_matrix, catalog_formula, catalog_data, cold_to_cold, cold_item_model, similarity)
    }

    wi <- match(cold_df$warm_item, rownames(sim_matrix))
    ci <- match(cold_df$cold_item, rownames(sim_matrix))
    sim_matrix[cbind(ci, wi)] <- cold_df$wt # fill in cold rows
    sim_matrix[cbind(wi, ci)] <- cold_df$wt # fill in cold columns

    sim_matrix
}


#' @export
print.sar <- function(x, ...)
{
    cat("SAR model\n")
    cat("Support threshold:", x$support_threshold, "\n")
    cat("Co-occurrence unit:", if(x$by_user) "user\n" else "user/time\n")
    cat("Similarity function:", x$similarity, "\n")
    cat("Decay period in days:", x$half_life, "\n")
    cat("Item count:", nrow(x$sim_mat), "\n")
    cat("User count:", nrow(x$aff_mat), "\n")
    if(!is.null(x$col_ids))
    {
        cat("Column names:\n")
        print(x$col_ids)
    }
    invisible(x)
}



