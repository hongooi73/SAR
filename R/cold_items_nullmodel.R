cold_wts <- function(sim_matrix, catalog_vars, items)
{
    warm_items <- rownames(sim_matrix)[diag(sim_matrix) > 0]

    df <- expand.grid(item1=warm_items, item2=warm_items, stringsAsFactors=FALSE)
    sim <- as.numeric(sim_matrix[warm_items, warm_items])

    vars <- sapply(catalog_vars, function(x)
    {
        x1 <- x[match(df$item1, items)]
        x2 <- x[match(df$item2, items)]
        max(0, cor(x1 == x2, sim, use="complete.obs", method="pearson"))
    })

    vars / sum(vars)
}


get_cold_similarity_nullmodel <- function(sim_matrix, catalog_formula, catalog_data, cold_to_cold=FALSE)
{
    # assume column 1 is item ID
    items <- catalog_data[[1]]

    vars <- all.vars(catalog_formula[[length(catalog_formula)]])
    wts <- cold_wts(sim_matrix, catalog_data[vars], items)

    all_items <- rownames(sim_matrix)
    cold_items <- all_items[diag(sim_matrix) == 0]
    warm_items <- setdiff(all_items, cold_items)

    df <- if(!cold_to_cold)
        expand.grid(warm_item=warm_items, cold_item=cold_items, stringsAsFactors=FALSE)
    else expand.grid(warm_item=all_items, cold_item=cold_items, stringsAsFactors=FALSE) %>%
        dplyr::filter(warm_item != cold_item)

    vars <- mapply(function(x, wt)
    {
        x1 <- x[match(df$warm_item, items)]
        x2 <- x[match(df$cold_item, items)]
        (x1 == x2) * wt
    }, catalog_data[vars], wts, SIMPLIFY=FALSE)

    wt <- rowSums(dplyr::bind_cols(vars))

    # rescale weights so that cold similarities are always less than warm-warm similarities
    wt <- wt * min(sim_matrix@x)

    dplyr::bind_cols(df, wt=wt)
}

