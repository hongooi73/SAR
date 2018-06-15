#' @param n_threads For `set_sar_threads`, the number of threads to use. Defaults to half the number of logical cores.
#'
#' @rdname user_predict
#' @export
set_sar_threads <- function(n_threads)
{
    if(missing(n_threads))
        n_threads <- max(1, parallel::detectCores()/2)
    RcppParallel::setThreadOptions(numThreads=as.integer(n_threads))
}
