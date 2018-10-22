.onLoad <- function(libname, pkgname)
{
    set_sar_threads()
    add_sar_methods()
}


# add class methods to resource group
add_sar_methods <- function()
{
    az_resource_group$set("public", "create_rec_service", overwrite=TRUE,
    function(name, hosting_plan,
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

        res <- az_rec_service$new(self$token, self$subscription, self$name, name,
                                  template=sar_template, parameters=parameters,
                                  ..., wait=wait)

        res$set_data_container(data_container)
        res
    })

    az_resource_group$set("public", "get_rec_service", overwrite=TRUE,
    function(name, data_container="inputdata")
    {
        res <- az_rec_service$new(self$token, self$subscription, self$name, name)
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
        az_rec_endpoint$new(name, admin_key, rec_key, service_host,
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
