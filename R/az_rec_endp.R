#' Azure product recommendations endpoint class
#'
#' Class representing the client endpoint to the product recommendations service.
#'
#' @docType class
#' @section Methods:
#' - `new(...)`: Initialize a client endpoint object. See 'Initialization' for more details.
#' - `get_model()`: Get an existing product recommendations model; return an object of class `rec_model`.
#' - `train_model(...)`: Train a new product recommendations model; return an object of class `rec_model`. See `Training` for more details.
#' - `upload_data(data, destfile)`: Upload a data frame to the endpoint, as a CSV file. By default, the name of the uploaded file will be the name of the data frame with a ".csv" extension.
#' - `upload_csv(srcfile, destfile)`: Upload a CSV file to the endpoint. By default, the name of the uploaded file will be the same as the source file.
#' - `sync_model_list()`: Update the stored list of models for this service.
#' - `get_swagger_url()`: Get the Swagger URL for this service.
#' - `get_service_url()`: Get the service URL, which is used to train models and obtain recommendations.
#'
#' @section Initialization:
#' The following arguments are used to initialize a new client endpoint:
#' - `name`: The name of the endpoint; see below. Alternatively, this can also be the full URL of the endpoint.
#' - `admin_key`: The administration key for the endpoint. Use this to retrieve, train, and delete models.
#' - `rec_key`: The recommender key for the endpoint. Use this to get recommendations.
#' - `service_host`: The hostname for the endpoint. For the public Azure cloud, this is `azurewebsites.net`.
#' - `storage_key`: The access key for the storage account associated with the service.
#' - `storage_sas`: A shared access signature (SAS) for the storage account associated with the service. You must provide either `storage_key` or `storage_sas` if you want to upload new datasets to the backend.
#' - `storage_host`: The hostname for the storage account. For the public Azure cloud, this is `core.windows.net`.
#' - `storage_endpoint`: The storage account endpoint for the service. By default, uses the account that was created at service creation.
#' - `data_container`: The default blob container for input datasets. Defaults to `"inputdata"`.
#'
#' Note that the name of the client endpoint for a product recommendations service is _not_ the name that was supplied when deploying the service. Instead, it is a randomly generated unique string that starts with the service name. For example, if you deployed a service called "myrec", the name of the endpoint is "myrecusacvjwpk4raost".
#'
#' @section Training:
#' To train a new model, supply the following arguments to the `train_model` method:
#' - `description`: A character string describing the model.
#' - `usage_data`: The training dataset. This is required.
#' - `catalog_data`: An optional dataset giving features for each item. Only used for imputing cold items.
#' - `eval_data`: An optional dataset to use for evaluating model performance.
#' - `support_threshold`: The minimum support for an item to be considered warm.
#' - `cooccurrence`: How to measure cooccurrence: either user ID, or user-by-time.
#' - `similarity`: The similarity metric to use; defaults to "Jaccard".
#' - `cold_items`: Whether recommendations should include cold items.
#' - `cold_to_cold`: Whether similarities between cold items should be computed.
#' - `user_affinity`: Whether event type and time should be considered.
#- - `backfill`: Whether to backfill recommendations with popular items.
#' - `include_seed_items`: Whether seed items (those already seen by a user) should be allowed as recommendations.
#' - `half_life`: The time decay parameter for computing user-item affinities.
#' - `user_to_items`: Whether user ID is used when computing personalised recommendations.
#' - `wait`: Whether to wait until the model has finished training.
#' - `container`: The container where the input datasets are stored. Defaults to the input container for the endpoint, usually `"inputdata"`.
#'
#' For detailed information on these arguments see the [API reference](https://github.com/Microsoft/Product-Recommendations/blob/master/doc/api-reference.md#train-a-new-model).
#'
#' @seealso
#' [az_rec_service] for the service itself, [rec_model] for an individual recommmendations model
#'
#' [API reference](https://github.com/Microsoft/Product-Recommendations/blob/master/doc/api-reference.md) and [SAR model description](https://github.com/Microsoft/Product-Recommendations/blob/master/doc/sar.md) at the Product Recommendations API repo on GitHub
#'
#' @format An R6 object of class `rec_endpoint`.
#' @export
rec_endpoint <- R6Class("rec_endpoint",

public=list(
    url=NULL,
    admin_key=NULL,
    rec_key=NULL,
    name=NULL,
    storage=NULL,
    models=NULL,
    data_container=NULL,

    initialize=function(name, admin_key, rec_key, service_host="azurewebsites.net",
                        storage_key=NULL, storage_sas=NULL, storage_host="core.windows.net",
                        storage_endpoint=NULL, data_container="inputdata")
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
        self$data_container <- data_container

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
            id <- private$get_id_by_desc(description)
        rec_model$new(self$url, self$admin_key, self$rec_key, id=id)
    },

    train_model=function(description,
                         usage_data=NULL, catalog_data=NULL, eval_data=NULL,
                         support_threshold=NULL, cooccurrence=NULL, similarity=NULL,
                         cold_items=NULL, cold_to_cold=NULL, user_affinity=NULL, backfill=NULL, include_seed_items=NULL,
                         half_life=NULL, user_to_items=NULL, wait=TRUE, container=self$data_container)
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

        res <- rec_model$new(self$url, self$admin_key, self$rec_key, parms=parms, wait=wait)
        self$sync_model_list()
        res
    },

    delete_model=function(description, id, confirm=TRUE)
    {
        self$get_model(description, id)$delete(confirm=confirm)
        invisible(self$sync_model_list())
    },

    upload_data=function(data, destfile, container=self$data_container)
    {
        if(missing(destfile))
            destfile <- paste0(as.character(substitute(data)), ".csv")
        f <- tempfile(fileext=".csv")
        on.exit(file.remove(f))
        write.table(data, f, row.names=FALSE, col.names=FALSE, sep=",")
        self$upload_csv(f, destfile, container)
    },

    upload_csv=function(srcfile, destfile=basename(srcfile), container=self$data_container)
    {
        self$storage %>% blob_container(container) %>% upload_blob(srcfile, destfile)
    },

    list_data=function(container=self$data_container)
    {
        self$storage %>% blob_container(container) %>% list_blobs()
    },

    delete_data=function(data, container=self$data_container, confirm=TRUE)
    {
        self$storage %>% blob_container(container) %>% delete_blob(data, confirm=confirm)
    },

    print=function(...)
    {
        cat("Product recommendations service endpoint\n")
        cat("Service URL:", self$get_service_url(), "\n")
        cat("Swagger URL:", self$get_swagger_url(), "\n")
        cat("Admin key:", if(is_empty(self$admin_key)) "<not supplied>\n" else "<hidden>\n")
        cat("Recommender key:", if(is_empty(self$rec_key)) "<not supplied>\n" else "<hidden>\n")
        cat("---\n")
        cat("Models:\n")
        print(self$models)
        cat("---\n")
        print(self$storage)
        cat("\n---\n")
        cat(AzureRMR::format_public_methods(self))
        invisible(NULL)
    }
),

private=list(

    get_id_by_desc=function(description)
    {
        match <- which(description == self$models$description)
        if(length(match) == 0)
            stop("No model found with description '", description, "'", call.=FALSE)
        else if(length(match) > 1)
            stop("More than one model with description '", description, "'", call.=FALSE)
        self$models$id[match]
    },

    get_desc_by_id=function(id)
    {
        match <- which(id == self$models$id)
        if(length(match) == 0)
            stop("No model found with ID '", id, "'", call.=FALSE)
        else if(length(match) > 1)
            stop("More than one model with ID '", id, "'", call.=FALSE)
        self$models$description[match]
    }
))


