#' Function to calculate mode
#'
#' @param v Vector with the data
#' @param na.rm If TRUE NAs are removed before calculations
#' @return Numeric value with the mode of v
#'
#' @examples 
#' moda(c(2,3,5,3,2,6,2,3,5,3))
#'
#' @export
#'
#' @keywords statistics
#'
#'
moda <- function(v, na.rm=FALSE) {
   if (na.rm) v <- v[which(!is.na(v))]
   if(length(which(is.na(v)))>0) return(NA)
   uniqv <- unique(v)
   i <- which(tabulate(match(v, uniqv))==max(tabulate(match(v, uniqv))))
   sort(uniqv[i])
}

