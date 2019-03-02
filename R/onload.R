#' Create Azure recommender service
#'
#' Method for the [AzureRMR::az_resource_group] and [AzureRMR::az_subscription] classes.
#'
#' @rdname create_rec_service
#' @name create_rec_service
#' @aliases create_rec_service
#' @section Usage:
#' ```
#' ## R6 method for class 'az_subscription'
#' create_rec_service(name, location, hosting_plan, storage_type = c("Standard_LRS", "Standard_GRS"),
#'                    insights_location = c("East US", "North Europe", "West Europe", "South Central US"),
#'                    data_container = "inputdata", ..., wait = TRUE
#'
#' ## R6 method for class 'az_resource_group'
#' create_rec_service(name, hosting_plan, storage_type = c("Standard_LRS", "Standard_GRS"),
#'                    insights_location = c("East US", "North Europe", "West Europe", "South Central US"),
#'                    data_container = "inputdata", ..., wait = TRUE
#' ```
#' @section Arguments:
#' - `name`: The name of the recommender service.
#' - `location`: For the subscription method, the location/region for the service. For the resource group method, this is taken from the location of the resource group.
#' - `storage_type`: The replication strategy for the storage account for the service.
#' - `insights_location`: Location for the application insights service giving you details on the webapp usage.
#' - `data_container`: The name of the blob container within the storage account to use for storing datasets.
#' - `wait`: Whether to wait until the service has finished provisioning.
#' - `...` : Other named arguments to pass to the [az_template] initialization function.
#'
#' @section Details:
#' This method deploys a new recommender service. The individual resources created are an Azure webapp, a storage account, and an application insights service for monitoring. Within the storage account, a blob container is created with name given by the `data_container` argument for storing input datasets.
#'
#' For the az_subscription method, a resource group is also created to hold the resources. The name of the resource group will be the same as the name of the service.
#'
#' @section Value:
#' An object of class `az_rec_service` representing the deployed recommender service.
#'
#' @seealso
#' [get_rec_service], [delete_rec_service].
#'
#' The architecture for the web service is documented [here](https://github.com/Microsoft/Product-Recommendations/blob/master/doc/architecture.md), and the specific template deployed by this method is [here](https://raw.githubusercontent.com/Microsoft/Product-Recommendations/master/saw/recommendationswebapp/core/arm/resources.json).
#'
#' @examples
#' \dontrun{
#'
#' rg <- AzureRMR::az_rm$
#'     new(tenant="myaadtenant.onmicrosoft.com", app="app_id", password="password")$
#'     get_subscription("subscription_id")$
#'     get_resource_group("rgname")
#' 
#' # create a new recommender service
#' rg$create_rec_service("myrec", hosting_plan="S2")
#' 
#' }
NULL


#' Get existing Azure recommender service
#'
#' Method for the [AzureRMR::az_resource_group] and [AzureRMR::az_subscription] classes.
#'
#' @rdname get_rec_service
#' @name get_rec_service
#' @aliases get_rec_service
#' @section Usage:
#' ```
#' get_rec_service(name, data_container = "inputdata")
#' ```
#' @section Arguments:
#' - `name`: The name of the recommender service.
#' - `data_container`: The name of the blob container within the storage account to use for storing datasets.
#'
#' @section Value:
#' An object of class `az_rec_service` representing the deployed recommender service.
#'
#' @seealso
#' [create_rec_service], [delete_rec_service]
#'
#' @examples
#' \dontrun{
#'
#' rg <- AzureRMR::az_rm$
#'     new(tenant="myaadtenant.onmicrosoft.com", app="app_id", password="password")$
#'     get_subscription("subscription_id")$
#'     get_resource_group("rgname")
#' 
#' # get a recommender service
#' rg$get_rec_service("myrec")
#' 
#' }
NULL


#' Delete an Azure recommender service
#'
#' Method for the [AzureRMR::az_resource_group] and [AzureRMR::az_subscription] classes.
#'
#' @rdname delete_rec_service
#' @name delete_rec_service
#' @aliases delete_rec_service
#' @section Usage:
#' ```
#' delete_rec_service(name, confirm = TRUE, free_resources = TRUE)
#' ```
#' @section Arguments:
#' - `name`: The name of the recommender service.
#' - `confirm`: Whether to ask for confirmation before deleting.
#' - `free_resources`: Whether to delete the individual resources as well as the recommender template.
#'
#' @section Value:
#' NULL on successful deletion.
#'
#' @seealso
#' [create_rec_service], [delete_rec_service]
#'
#' @examples
#' \dontrun{
#'
#' rg <- AzureRMR::az_rm$
#'     new(tenant="myaadtenant.onmicrosoft.com", app="app_id", password="password")$
#'     get_subscription("subscription_id")$
#'     get_resource_group("rgname")
#' 
#' # delete a recommender service
#' rg$delete_rec_service("myrec")
#' 
#' }
NULL


.onLoad <- function(libname, pkgname)
{
    set_sar_threads()
    add_sar_methods()
}


# add class methods to resource group
add_sar_methods <- function()
{
    az_resource_group$set("public", "create_rec_service", overwrite=TRUE,
    function(name, hosting_plan="S2",
             storage_type=c("Standard_LRS", "Standard_GRS"),
             insights_location=c("East US", "North Europe", "West Europe", "South Central US"),
             data_container="inputdata",
             ..., wait=TRUE)
    {
        storage_type <- match.arg(storage_type)
        insights_location <- match.arg(insights_location)

        parameters <- list(accountType=storage_type,
                           hostingPlanSku=hosting_plan,
                           appInsightsLocation=insights_location,
                           deployPackageUri=sar_dll)

        res <- SAR::az_rec_service$new(self$token, self$subscription, self$name,
            name=name,
            template=sar_template, parameters=parameters,
            ..., wait=wait)

        res$set_data_container(data_container)
        res
    })

    az_resource_group$set("public", "get_rec_service", overwrite=TRUE,
    function(name, data_container="inputdata")
    {
        res <- SAR::az_rec_service$new(self$token, self$subscription, self$name, name)
        if(!is_empty(data_container))
            res$set_data_container(data_container)
        res
    })

    az_resource_group$set("public", "delete_rec_service", overwrite=TRUE,
    function(name, confirm=TRUE, free_resources=TRUE)
    {
        self$get_rec_service(name, NULL)$delete(confirm=confirm, free_resources=free_resources)
    })

    az_resource_group$set("public", "get_rec_endpoint", overwrite=TRUE,
    function(name, admin_key, rec_key, service_host="azurewebsites.net",
             storage_key=NULL, storage_sas=NULL, storage_host="core.windows.net",
             storage_endpoint=NULL)
    {
        SAR::az_rec_endpoint$new(name, admin_key, rec_key, service_host,
                            storage_key, storage_sas, storage_host, storage_endpoint)
    })

    ## add class methods to subscription

    az_subscription$set("public", "create_rec_service", overwrite=TRUE,
    function(name, location, resource_group=name, hosting_plan,
             storage_type=c("Standard_LRS", "Standard_GRS"),
             insights_location=c("East US", "North Europe", "West Europe", "South Central US"),
             data_container="inputdata",
             ..., wait=TRUE)
    {
        if(!is_resource_group(resource_group))
        {
            rgnames <- names(self$list_resource_groups())
            if(resource_group %in% rgnames)
            {
                resource_group <- self$get_resource_group(resource_group)
                mode <- "Incremental"
            }
            else
            {
                message("Creating resource group '", resource_group, "'")
                resource_group <- self$create_resource_group(resource_group, location=location)
                mode <- "Complete"
            }
        }
        else mode <- "Incremental" # if passed a resource group object, assume it already exists in Azure

        res <- try(resource_group$create_rec_service(name=name, hosting_plan=hosting_plan,
            storage_type=storage_type,
            insights_location=insights_location,
            data_container=data_container,
            ..., wait=wait, mode=mode))

        if(inherits(res, "try-error") && mode == "Complete")
        {
            resource_group$delete(confirm=FALSE)
            stop("Unable to create recommendation service")
        }
        res
    })

    az_subscription$set("public", "get_rec_service", overwrite=TRUE,
    function(name, resource_group=name, data_container="inputdata")
    {
        if(!is_resource_group(resource_group))
            resource_group <- self$get_resource_group(resource_group)

        resource_group$get_rec_service(name, data_container)
    })

    az_subscription$set("public", "delete_rec_service", overwrite=TRUE,
    function(name, confirm=TRUE, free_resources=TRUE, resource_group=name)
    {
        self$get_rec_service(name, resource_group)$delete(confirm=confirm, free_resources=free_resources)
    })
}
