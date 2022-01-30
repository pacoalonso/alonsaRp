#' List of objects and its size:
#' 
#' @param obs The return of ls()
#' @return Paleta de color
#' 
#' @import grDevices
#' @import rgrass7
#' @import rgdal
#' @import RColorBrewer
#' @import classInt 
#'
#' @export
#'
sizeObjects <- function(obs) {
   sizes <- c()
   for (ob in 1:length(obs)) {
        sizes[ob] <- as.numeric(object.size(obs[ob]))
   }
   o <- order(sizes)
   return(data.frame(obs=obs[o], sizes=sizes[o]))
}
