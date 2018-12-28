#' Azure product recommendations service class
#'
#' Class representing an Azure product recommendations service.
#'
#' @docType class
#' @section Methods:
#' - `new(token, subscription, resource_group, name, ...)`: Initialize a recommendations service object. See 'Initialization' for more details.
#' - `start()`: Start the service.
#' - `stop()`: Stop the service.
#' - `get_rec_endpoint()`: Return an object representing the client endpoint for the service.
#' - `set_data_container(data_container="inputdata")`: sets the name of the blob container to use for storing datasets.
#' - `delete(confirm=TRUE)`: Delete the service, after checking for confirmation.
#'
#' @section Initialization:
#' Generally, the easiest way to initialize a new recommendations service object is via the `create_rec_service` or `get_rec_service` methods of the [az_subscription] or [az_resource_group] classes.
#' 
#' To create a new recommendations service, supply the following additional arguments to `new()`:
#' - `hosting_plan`: The name of the hosting plan (essentially the size of the virtual machine on which to run the service). See below for the plans that are available.
#' - `storage_type`: The type of storage account to use. Can be `"Standard_LRS"` or `"Standard_GRS"`.
#' - `insights_location`: The location for the application insights service. Defaults to `"East US"`.
#' - `data_container`: The default blob storage container to use for saving input datasets. Defaults to `"inputdata"`.
#' - `wait`: Whether to wait until the service has finished provisioning. Defaults to TRUE.
#'
#' @seealso
#' [rec_endpoint], for the client interface to the recommendations service
#'
#' [List of Azure hosting plans](https://azure.microsoft.com/en-us/pricing/details/app-service/windows/)
#'
#' [Deployment instructions](https://github.com/Microsoft/Product-Recommendations/blob/master/deploy/README.md) at the Product Recommendations API repo on GitHub
#'
#' @examples
#' \dontrun{
#'
#' # recommended way of retrieving a resource: via a resource group object
#' svc <- resgroup$get_rec_service("myrec")
#'
#' # start the service backend
#' svc$start()
#'
#' # get the service endpoint
#' rec_endp <- svc$get_rec_endpoint()
#'
#' }
#' @format An R6 object of class `az_rec_service`, inheriting from `AzureRMR::az_template`.
#' @export
az_rec_service <- R6Class("az_rec_service", inherit=AzureRMR::az_template,

public=list(
    url=NULL,
    admin_key=NULL,
    rec_key=NULL,
    storage_key=NULL,
    data_container=NULL,

    initialize=function(token, subscription, resource_group, name, ...)
    {
        super$initialize(token, subscription, resource_group, name, ...)

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
    },

    start=function()
    {
        private$app$do_operation(http_verb="POST", "start")
        invisible(NULL)
    },

    stop=function()
    {
        private$app$do_operation(http_verb="POST", "stop")
        invisible(NULL)
    },

    get_rec_endpoint=function(key=self$storage_key, sas=NULL)
    {
        stor_endp <- private$storage$get_blob_endpoint(key=self$storage_key, sas=sas)
        rec_endpoint$new(self$url, self$admin_key, self$rec_key,
                         storage_endpoint=stor_endp, data_container=self$data_container)
    },

    set_data_container=function(data_container="inputdata")
    {
        stor_endp <- private$storage$get_blob_endpoint(key=self$storage_key)
        conts <- names(list_blob_containers(stor_endp))
        if(!(data_container %in% conts))
            create_blob_container(stor_endp, data_container, public_access="none")
        self$data_container <- data_container
    },

    print=function(...)
    {
        cat("<Azure product recommendations service ", self$name, ">\n", sep="")
        cat(AzureRMR::format_public_fields(self,
            exclude=c("subscription", "resource_group", "name", "id", "properties")))
        cat(AzureRMR::format_public_methods(self))
        invisible(NULL)
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

