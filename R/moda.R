#' Function to calculate mode
#'
#' @param v Vector with the data
#'
#' @return Numeric value with the mode of v
#'
#' @examples 
#' mode(c(2,3,5,3,2,6,2,3,5,3))
#'
#' @export
#'
#' @keywords statistics
#'
mode <- function(v) {
   v= v[which(!is.na(v))]
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

