.onLoad <- function(libname, pkgname)
{
    nc <- parallel::detectCores()
    nt <- as.integer(max(1, nc/2))
    RcppParallel::setThreadOptions(numThreads=nt)

    ## add class methods to resource group

    AzureRMR::az_resource_group$set("public", "create_rec_service", overwrite=TRUE,
    function(name, ..., wait=TRUE)
    {
        az_rec_service$new(self$token, self$subscription, self$name, name, ..., wait=wait)
    })

    AzureRMR::az_resource_group$set("public", "get_rec_service", overwrite=TRUE,
    function(name)
    {
        az_rec_service$new(self$token, self$subscription, self$name, name)
    })

    AzureRMR::az_resource_group$set("public", "delete_rec_service", overwrite=TRUE,
    function(name, confirm=TRUE, free_resources=TRUE)
    {
        self$get_rec_service(name)$delete(confirm=confirm, free_resources=free_resources)
    })

    AzureRMR::az_resource_group$set("public", "get_rec_endpoint", overwrite=TRUE,
    function(name, admin_key, rec_key, service_host="azurewebsites.net",
                storage_key=NULL, storage_sas=NULL, storage_host="core.windows.net",
                storage_endpoint=NULL)
    {
        az_rec_endpoint$new(name, admin_key, rec_key, service_host,
                            storage_key, storage_sas, storage_host, storage_endpoint)
    })

    ## add class methods to subscription

    AzureRMR::az_subscription$set("public", "create_rec_service", overwrite=TRUE,
    function(name, location, ..., resource_group=name)
    {
        if(is_resource_group(resource_group))
        {
            if(missing(location))
                location <- resource_group$location
            resource_group <- resource_group$name
        }

        rgnames <- names(self$list_resource_groups())
        exclusive_group <- !(resource_group %in% rgnames)
        if(exclusive_group)
        {
            message("Creating resource group '", resource_group, "'")
            self$create_resource_group(resource_group, location=location)
            mode <- "Complete"
        }
        else mode <- "Incremental"

        res <- try(az_rec_service$new(self$token, self$id, resource_group, name, ..., mode=mode))
        if(inherits(res, "try-error") && mode == "Complete")
            return(self$delete_resource_group(resource_group, confirm=FALSE))
        res
    })

    AzureRMR::az_subscription$set("public", "get_rec_service", overwrite=TRUE,
    function(name, resource_group=name)
    {
        if(is_resource_group(resource_group))
            resource_group <- resource_group$name

        az_rec_service$new(self$token, self$id, resource_group, name)
    })

    AzureRMR::az_subscription$set("public", "delete_rec_service", overwrite=TRUE,
    function(name, confirm=TRUE, free_resources=TRUE, resource_group=name)
    {
        self$get_rec_service(name, resource_group)$delete(confirm=confirm, free_resources=free_resources)
    })
}
