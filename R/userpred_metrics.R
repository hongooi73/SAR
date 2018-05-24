#' @export
user_pred_metrics <- function(recdata, testdata)
{
    user_col <- names(recdata)[[1]]

    testdata[[user_col]] <- as.character(testdata[[user_col]])
    user_subset <- intersect(recdata[[user_col]], testdata[[user_col]])
    recdata <- recdata[recdata[[user_col]] %in% user_subset,]
    testdata <- testdata[testdata[[user_col]] %in% user_subset,]

    testtab <- testdata %>%
        dplyr::group_by(user) %>%
        dplyr::do(dplyr::data_frame(testitems=list(unique(.$item))))

    recnames <- grep("rec", names(recdata), value=TRUE)
    n_recs <- length(recnames)
    rectab <- recdata[c("user", recnames)] %>%
        dplyr::group_by(user) %>%
        dplyr::do(dplyr::data_frame(recitems=list(as.character(.[-1]))))

    out <- dplyr::bind_cols(testtab, rectab[-1])
    out <- dplyr::group_by(out, user) %>%
        do(
        {
            testitems <- as.character(.$testitems[[1]])
            recitems <- .$recitems[[1]]
            n_hits <- length(intersect(recitems, testitems))
            dplyr::data_frame(prec=n_hits/n_recs, rec=n_hits/length(testitems), n_items=length(testitems), n_hits=n_hits)
        })
    out
}

