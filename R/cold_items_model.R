# construct a model frame for the feature regression model: used for training and prediction
feature_model_frame <- function(item1, item2, catalog_formula, catalog_data)
{
    items <- catalog_data[[1]]

    vars <- all.vars(catalog_formula[[length(catalog_formula)]])
    vars <- lapply(catalog_data[vars], function(x)
    {
        x1 <- x[match(item1, items)]
        x2 <- x[match(item2, items)]
        x1 == x2
    })
    dplyr::bind_cols(item1=item1, item2=item2, vars)
}


# fit the regression model for predicting similarities for cold items, using feature data
get_cold_similarity_model <- function(sim_matrix, catalog_formula, catalog_data, cold_to_cold,
                                      cold_item_model, similarity, ...)
{
    # create training dataset: all warm-warm item pairs + random selection of cold-warm item pairs
    pairs <- cold_model_sample(sim_matrix)

    # logit transform only makes sense if similarity is jaccard
    y <- as.numeric(sim_matrix[pairs])
    if(similarity == "jaccard")
        y <- logit(y)
    
    all_items <- rownames(sim_matrix)
    catalog_formula <- update(catalog_formula, y ~ .)

    model <- cold_item_model(formula=catalog_formula,
        data=dplyr::bind_cols(y=y,
            feature_model_frame(all_items[pairs[, 1]], all_items[pairs[, 2]], catalog_formula, catalog_data)), ...)

    cold_items <- all_items[diag(sim_matrix) == 0]
    warm_items <- setdiff(all_items, cold_items)

    df <- if(!cold_to_cold)
        expand.grid(warm_item=warm_items, cold_item=cold_items, stringsAsFactors=FALSE)
    else expand.grid(warm_item=all_items, cold_item=cold_items, stringsAsFactors=FALSE) %>%
        dplyr::filter(warm_item != cold_item)

    cold_pred <- predict(model,
        feature_model_frame(df$warm_item, df$cold_item, catalog_formula, catalog_data))

    # ???
    #if(inherits(model, "lm"))
    #{
        #b <- model$coefficients[1]
        #cold_pred[cold_pred > b] <- pmax(0, cold_pred[cold_pred > b])
    #}
    # presumably really trying to do:
    # assume we should back-transform for jaccard (?)
    if(similarity == "jaccard")
        cold_pred <- expit(cold_pred)

    # rescale weights so that cold similarities are always less than warm-warm similarities
    df$wt <- cold_pred * min(sim_matrix@x) / max(cold_pred)

    df
}


logit <- function(x)
{
    x <- pmax(1e-5, pmin(x, 1 - 1e-5))
    log(x / (1 - x))
}


expit <- function(x)
{
    1/(1 + exp(-x))
}


# create training dataset: all warm-warm item pairs + random selection of cold-warm item pairs
cold_model_sample <- function(sim_matrix)
{
    pairs <- which(sim_matrix != 0, arr.ind=TRUE)
    pairs <- pairs[pairs[, 1] > pairs[, 2],] # lower triangular portion only

    warm <- which(diag(sim_matrix) != 0)
    n <- nrow(sim_matrix)
    n_warm <- length(warm)
    p_warm <- nrow(pairs) / (n_warm * (n_warm - 1) / 2) # proportion of warm pairs

    if(p_warm >= 0.5)
    {
        # get everything
        pairs <- expand.grid(row=warm, col=warm) %>%
            dplyr::filter(row > col) %>%
            as.matrix
    }
    else
    {
        # TODO: make more scalable
        cold_pairs <- which(sim_matrix == 0, arr.ind=TRUE)
        cold_pairs <- cold_pairs[cold_pairs[, 1] > cold_pairs[, 2],]
        cold_pairs <- cold_pairs[sample(nrow(cold_pairs), n_warm),] # sample of same size as #warm pairs
        pairs <- rbind(pairs, cold_pairs)
    }
    pairs
}

