#' @export
az_rec_service <- R6Class("az_rec_service", inherit=AzureRMR::az_template,

public=list(
    url=NULL,
    admin_key=NULL,
    rec_key=NULL,
    storage_key=NULL,

    initialize=function(token, subscription, resource_group, name,
                        hosting_plan,
                        storage_type=c("Standard_LRS", "Standard_GRS"),
                        insights_location=c("East US", "North Europe", "West Europe", "South Central US"),
                        default_container="inputdata",
                        ..., wait=TRUE)
    {
        # if no parameters were supplied, we want to retrieve an existing app
        existing_app <- missing(storage_type) && missing(hosting_plan) &&
                        missing(insights_location) && missing(default_container) &&
                        is_empty(list(...)) && missing(wait)

        if(!existing_app) # we want to deploy
        {
            storage_type <- match.arg(storage_type)
            insights_location <- match.arg(insights_location)

            template <- sar_template
            parameters <- list(accountType=storage_type,
                               hostingPlanSku=hosting_plan,
                               appInsightsLocation=insights_location,
                               deployPackageUri=sar_dll)

            super$initialize(token, subscription, resource_group, name, template, parameters, ..., wait=wait)
        }
        else super$initialize(token, subscription, resource_group, name)

        # get data members
        outputs <- self$properties$outputs
        self$url <- outputs$websiteUrl$value
        self$admin_key <- outputs$adminPrimaryKey$value
        self$rec_key <- outputs$recommendPrimaryKey$value
        self$storage_key <- sub("^AccountKey=", "",
            strsplit(outputs$storageConnectionString$value, ";")[[1]][3])

        # get the storage account and webapp
        outputs <- unlist(self$properties$outputResources)
        st_id <- grep("Microsoft.Storage/storageAccounts/.+$", outputs, ignore.case=TRUE, value=TRUE)[1]
        private$storage <- az_storage$new(self$token, self$subscription, id=st_id)

        app_id <- grep("Microsoft.Web/sites/.+$", outputs, ignore.case=TRUE, value=TRUE)[1]
        private$app <- az_resource$new(self$token, self$subscription, id=app_id)

        # create default blobcontainer for datasets
        if(!existing_app)
            private$storage$get_blob_endpoint() %>%
                create_blob_container(default_container, public_access="none")
    },

    start=function()
    {
        private$app$do_operation(http_verb="POST", "start")
    },

    stop=function()
    {
        private$app$do_operation(http_verb="POST", "stop")
    },

    get_rec_endpoint=function(key=self$storage_key, sas=NULL)
    {
        stor_endp <- private$storage$get_blob_endpoint(key=self$storage_key, sas=sas)
        az_rec_endpoint$new(self$url, self$admin_key, self$rec_key, storage_endpoint=stor_endp)
    }
),

private=list(
    storage=NULL,
    app=NULL,

    # override default method:
    # - this template lists hosting plan before site, must reorder to allow for dependency
    # - must also explicitly NOT delete empty plan as part of deleting app, or Azure gets confused
    # - do not delete resources which represent functionality provided by other resources
    free_resources=function()
    {
        resources <- self$properties$outputResources[c(1, 2, 4, 3)]
        for(i in seq_along(resources))
        {
            id <- resources[[i]]$id

            # supply deployed_properties arg to prevent querying host for resource info
            try(az_resource$
                new(self$token, self$subscription, id=id, deployed_properties=list(NULL))$
                delete(confirm=FALSE, wait=TRUE))
        }
    }
))

