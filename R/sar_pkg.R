#' @useDynLib SAR, .registration=TRUE
NULL

#' @importFrom Rcpp sourceCpp
#' @importFrom R6 R6Class
#' @importFrom dplyr %>%
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stats cor predict reformulate xtabs
NULL

#' @import Matrix
#' @import AzureRMR
#' @import AzureStor
NULL

globalVariables(c("self", "warm_item", "cold_item"), "SAR")
