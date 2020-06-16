#' Sample usage dataset
#'
#' Dataset of anonymised transaction records from the Microsoft online store.
#' @format A data frame with the following variables:
#' \describe{
#'  \item{user}{The user ID.}
#'  \item{item}{The item ID, corresponding to the items in the [ms_catalog] dataset.}
#'  \item{time}{The date and time of the transaction, in POSIXct format.}
#' }
#' @source Microsoft.
#' @seealso
#' [ms_catalog]
"ms_usage"


#' Sample catalog dataset
#'
#' Dataset of item descriptions from the Microsoft online store.
#' @format A data frame with the following variables:
#' \describe{
#'  \item{item}{The item ID, corresponding to the items in the [ms_usage] dataset.}
#'  \item{name}{A short description of the item.}
#'  \item{category}{The item category.}
#' }
#' @source Microsoft.
#' @seealso
#' [ms_usage]
"ms_catalog"
