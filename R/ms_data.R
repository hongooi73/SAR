#' Sample usage dataset
#'
#' A data frame of anonymised transaction records from the Microsoft online store. The variables are:
#' - `user`: The user ID.
#' - `item`: The item ID, corresponding to the items in the [ms_catalog] dataset.
#' - `time`: The date and time of the transaction, in POSIXct format.
#' 
#' @source Microsoft.
#' @seealso
#' [ms_catalog]
"ms_usage"


#' Sample catalog dataset
#'
#' A data frame containing a sample of item descriptions from the Microsoft online store. The variables are:
#' - `item`: The item ID, corresponding to the items in the [ms_usage] dataset.
#' - `name`: A short description of the item.
#' - `category`: The item category.
#'
#' @source Microsoft.
#' @seealso
#' [ms_usage]
"ms_catalog"
