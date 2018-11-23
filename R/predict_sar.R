#' Get personalised recommendations from a SAR model
#'
#' @param object A SAR model object.
#' @param userdata A vector of user IDs, or a data frame containing user IDs and/or transactions. See below for the various ways to supply user information for predicting, and how they affect the results.
#' @param k The number of recommendations to obtain.
#' @param include_seed_items Whether items a user has already seen should be considered for recommendations.
#' @param backfill Whether to backfill recommendations with popular items.
#' @param reftime The reference time for discounting timestamps. If not supplied, defaults to the latest date in the training data and any new transactions supplied.
#' @details
#' The SAR model can produce personalised recommendations for a user, given a history of their transactions. This history can be based on either the original training data, or new events, based on the contents of `userdata` argument:
#' 1. A character vector of user IDs. In this case, personalised recommendations will be computed based on the transactions in the training data, _ignoring_ any transaction event IDs or weights.
#' 2. A data frame containing transaction item IDs, event types and/or weights, plus timestamps. In this case, all the transactions are assumed to be for a single (new) user. If the event types/weights are absent, all transactions are assigned equal weight.
#' 3. A data frame containing user IDs and transaction details as in (2). In this case, the recommendations are based on both the training data for the given user(s), plus the new transaction details.
#'
#' In SAR, the first step in obtaining personalised recommendations is to compute a user-to-item affinity matrix \eqn{A}. This is essentially a weighted crosstabulation with one row per unique user ID and one column per item ID. The cells in the crosstab are given by the formula
#' \deqn{sum(wt * 2^(-(t0 - time) / half_life))}
#' where `wt` is obtained from the `weight` and `event` columns in the data.
#'
#' The product of this matrix with the item similarity matrix \eqn{S} then gives a matrix of recommendation scores. The recommendation scores are sorted, any items that the user has previously seen are optionally removed, and the top-N items are returned as the recommendations.
#'
#' The latter step is the most computationally expensive part of the algorithm. SAR can execute this in multithreaded fashion, with the default number of threads being half the number of (logical) cores. Use the `set_sar_threads` function to set the number of threads to use.
#'
#' @seealso
#' [Making recommendations](https://github.com/Microsoft/Product-Recommendations/blob/master/doc/sar.md#making-recommendations) at the [Product Recommendations API repo](https://github.com/Microsoft/Product-Recommendations) on GitHub
#'
#' @return
#' For `user_predict`, a data frame containing one row per user ID supplied (or if no IDs are supplied, exactly one row).
#' @rdname user_predict
#' @export
user_predict <- function(object, userdata=NULL, k=10, include_seed_items=FALSE, backfill=FALSE, reftime)
{
    user_col <- object$col_ids["user"]
    item_col <- object$col_ids["item"]
    time_col <- object$col_ids["time"]
    event_col <- object$col_ids["event"]
    weight_col <- object$col_ids["weight"]

    if(is.null(userdata))
        stop("Must provide new transaction events or users")

    if(is.data.frame(userdata))
    {
        user <- as.character(userdata[[user_col]])
        item <- userdata[[item_col]]
        time <- userdata[[time_col]]
        event <- userdata[[event_col]]
        weight <- userdata[[weight_col]]
    }
    else
    {
        user <- as.character(userdata)
        item <- time <- event <- weight <- NULL
    }

    t0 <- if(!missing(reftime))
        max(object$time, time, reftime)
    else max(object$time, time)  # if time not supplied, reduces to max(object$time)
    unique_users <- unique(user)

    # if userids supplied, compute affinity matrix from training data for these users
    trn_aff <- if(length(user) > 0)
    {
        keep <- which(object$user %in% unique_users)

        wt <- calc_wt(NULL, NULL, object$allowed_events)  # events/weights not used for training data!
        make_affinity(object$user[keep], object$item[keep], object$time[keep], wt, t0,
                    object$half_life, object$allowed_items)
    }
    else 0

    # if new transaction events supplied, compute affinity matrix from these events
    new_aff <- if(length(item) > 0)
    {
        item <- factor(item, levels=levels(object$item))
        if(any(is.na(item)))
            stop("New item IDs detected")
        wt <- calc_wt(event, weight, object$allowed_events)
        make_affinity(user, item, time, wt, t0, object$half_life, object$allowed_items)
    }
    else 0

    # combine old and new affinity matrices, taking into account userids
    if(length(user) > 0)
    {
        tc <- colnames(trn_aff)
        nc <- colnames(new_aff)
        overlap <- intersect(tc, nc)

        # if both trn_aff and new_aff exist, there must be overlapping columns
        if(length(overlap) > 0)
            aff <- cbind(
                trn_aff[, base::setdiff(tc, overlap), drop=FALSE],
                trn_aff[, overlap, drop=FALSE] + new_aff[, overlap, drop=FALSE],
                new_aff[, base::setdiff(nc, overlap), drop=FALSE])
        else if(!is.null(nrow(trn_aff)) && nrow(trn_aff) > 0)
            aff <- trn_aff
        else if(!is.null(nrow(new_aff)) && nrow(new_aff) > 0)
            aff <- new_aff
        else stop("Bad affinity matrix calculation")
    }
    else aff <- new_aff

    # detect case where new user ID supplied but no transactions
    if(length(user) > 0 && length(item) == 0 && ncol(aff) != length(unique_users))
        warning("New user IDs detected without any transactions; these will be dropped")

    recs <- user_predict_ranking(aff, object$sim_mat, k, include_seed_items, backfill, object$pop_items)

    # if we don't fill in zero-score recs, NA them out
    if(!backfill)
    {
        zeros <- recs[[1]] == 0
        recs[[2]][zeros] <- NA_integer_
        recs[[1]][zeros] <- NA_real_
    }

    recs[[2]][] <- rownames(object$sim_mat)[recs[[2]]]
    colnames(recs[[1]]) <- paste0("score", seq_len(k))
    colnames(recs[[2]]) <- paste0("rec", seq_len(k))

    if(length(user) == 0)
        cbind.data.frame(recs[[2]], recs[[1]], stringsAsFactors=FALSE)
    else
    {
        out <- cbind.data.frame(user=colnames(aff), recs[[2]], recs[[1]], stringsAsFactors=FALSE)
        out <- out[match(unique(user), out$user, nomatch=0),]
        row.names(out) <- NULL
        out
    }
}


#' Get item-to-item recommendations from a SAR model
#'
#' @param object A SAR model object.
#' @param items A vector of item IDs.
#' @param k The number of recommendations to obtain.
#' @return
#' A data frame containing one row per item ID supplied.
#' @export
item_predict <- function(object, items, k=10)
{
    if(is.data.frame(items))
        items <- as.character(items$items)
    else items <- as.character(items)
    item_sim <- object$sim_mat[items,, drop=FALSE]

    ord <- apply(item_sim, 1, function(x)
    {
        order(x, decreasing=TRUE)[seq_len(k) + 1] # assuming largest elem will be on the diagonal
    })

    recs <- matrix(rownames(object$sim_mat)[ord], ncol=k, byrow=TRUE)

    scores <- t(sapply(seq_len(ncol(ord)), function(x)
    {
        item_sim[x, ord[, x]]
    }))

    recs <- cbind.data.frame(recs, scores, stringsAsFactors=FALSE)
    names(recs) <- c(paste0("rec", seq_len(k)), paste0("score", seq_len(k)))
    cbind(item=items, recs, stringsAsFactors=FALSE)
}


make_affinity <- function(user, item, time, wt, t0=max(time), half_life, allowed_items=NULL)
{
    # handle POSIXct datetimes; assume data is in days otherwise
    if(inherits(time, "POSIXct"))
        half_life <- half_life * 24 * 3600
    else if(!inherits(time, c("Date", "numeric")))
        stop("time variable must be numeric, POSIXct or Date")

    # quit early if no data supplied
    if(length(item) < 1)
        return(0)

    time <- as.numeric(time)
    t0 <- as.numeric(t0)
    if(length(wt) == 0)
        wt <- rep(1, length(time))
    if(half_life > 0)
        wt <- wt*2^((time - t0) / half_life)

    # use sparse=TRUE to work around dimension problems with user, item large
    # outputs item-user matrix, not user-item matrix for speed later on
    if(length(unique(user)) >= 1)
        return(xtabs(wt ~ item + user, sparse=TRUE))
    else
    {
        out <- xtabs(wt ~ item)
        Matrix::Matrix(unclass(out), ncol=1, dimnames=list(dimnames(out)[[1]], NULL), sparse=TRUE)
    }
}


calc_wt <- function(event=NULL, weight=NULL,
                    allowed_events=c(Click=1, RecommendationClick=2, AddShopCart=3, RemoveShopCart=-1, Purchase=4))
{
    if(is.null(event) && is.null(weight))
        numeric(0)
    else if(!is.null(weight))
        weight
    else
    {
        stopifnot(all(event %in% names(allowed_events)))
        allowed_events[event]
    }
}
