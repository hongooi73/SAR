.onLoad <- function(libname, pkgname)
{
    nc <- parallel::detectCores()
    nt <- as.integer(max(1, nc/2))
    RcppParallel::setThreadOptions(numThreads=nt)

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
}
