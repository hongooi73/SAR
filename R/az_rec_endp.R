#' @export
az_rec_endpoint <- R6Class("az_rec_endpoint",

public=list(
    url=NULL,
    admin_key=NULL,
    rec_key=NULL,
    name=NULL,
    storage=NULL,
    models=NULL,

    initialize=function(name, admin_key, rec_key, service_host="azurewebsites.net",
                        storage_key=NULL, storage_sas=NULL, storage_host="core.windows.net",
                        storage_endpoint=NULL)
    {
        if(is_url(name))
        {
            url <- name
            name <- sub("(^[^.]+)\\..+$", "\\1", httr::parse_url(name)$host)
        }
        else url <- paste0("https://", name, ".", service_host)

        self$url <- url
        self$admin_key <- admin_key
        self$rec_key <- rec_key
        self$name <- name

        if(is.null(storage_endpoint))
        {
            storage_host <- paste0("st.blob.", storage_host)
            stor_endp <- sub("ws\\..+$", storage_host, self$url)
            storage_endpoint <- storage_endpoint(stor_endp, key=storage_key, sas=storage_sas)
        }
        else stopifnot(inherits(storage_endpoint, "blob_endpoint"))
        self$storage <- storage_endpoint

        self$sync_model_list()
        invisible(NULL)
    },

    # store the list of model descriptions and IDs for convenience
    sync_model_list=function()
    {
        res <- httr::GET(self$get_service_url(), httr::add_headers("x-api-key"=self$admin_key))
        httr::stop_for_status(res)
        self$models <- as.data.frame(dplyr::bind_rows(httr::content(res)))
        self$models
    },

    get_swagger_url=function()
    {
        sprintf("%s/swagger", self$url)
    },

    get_service_url=function()
    {
        sprintf("%s/api/models", self$url)
    },

    get_model=function(description, id)
    {
        if(missing(id))
            id <- private$get_model_by_desc(description)
        az_rec_model$new(self$url, self$admin_key, self$rec_key, id=id)
    },

    train_model=function(description, container=NULL, usage_data=NULL, catalog_data=NULL, eval_data=NULL,
                         support_threshold=NULL, cooccurrence=NULL, similarity=NULL,
                         cold_items=NULL, cold_to_cold=NULL, user_affinity=NULL, backfill=NULL, include_seed_items=NULL,
                         half_life=NULL, user_to_items=NULL)
    {
        if(description %in% self$models$description)
            stop("Model already exists with description '", description, "'", call.=FALSE)

        # don't use funky match.call magic to avoid NSE hassles
        parms <- list(
            description=description,
            blobContainerName=container,
            usageRelativePath=usage_data,
            catalogFileRelativePath=catalog_data,
            evaluationUsageRelativePath=eval_data,
            supportThreshold=support_threshold,
            cooccurrenceUnit=cooccurrence,
            similarityFunction=similarity,
            enableColdItemPlacement=cold_items,
            enableColdToColdRecommendations=cold_to_cold,
            enableUserAffinity=user_affinity,
            enableBackfilling=backfill,
            allowSeedItemsInRecommendations=include_seed_items,
            decayPeriodInDays=half_life,
            enableUserToItemRecommendations=user_to_items)

        res <- az_rec_model$new(self$url, self$admin_key, self$rec_key, parms=parms)
        self$sync_model_list()
        res
    },

    delete_model=function(description, id, confirm=TRUE)
    {
        self$get_model(description, id)$delete(confirm=confirm)
    },

    upload_data=function(data, destfile, container=dirname(destfile))
    {
        f <- tempfile(fileext=".csv")
        on.exit(file.remove(f))
        write.table(data, f, row.names=FALSE, col.names=FALSE, sep=",")
        self$upload_csv(f, destfile, container)
    },

    upload_csv=function(srcfile, destfile=basename(srcfile), container=dirname(destfile))
    {
        if(missing(container) || container == ".")
            container <- "inputdata"
        self$storage %>% blob_container(container) %>% upload_blob(srcfile, destfile)
    },

    list_data=function(container="inputdata")
    {
        self$storage %>% blob_container(container) %>% list_blobs()
    },

    delete_data=function(data, container="inputdata", confirm=TRUE)
    {
        self$storage %>% blob_container(container) %>% delete_blob(data, confirm=confirm)
    }
),

private=list(

    get_model_by_desc=function(description)
    {
        match <- which(description == self$models$description)
        if(length(match) == 0)
            stop("No model found with description '", description, "'", call.=FALSE)
        else if(length(match) > 1)
            stop("More than one model with description '", description, "'", call.=FALSE)
        self$models$id[match]
    }
))


