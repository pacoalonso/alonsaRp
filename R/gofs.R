#' Function to calculate goodness of fit statistics
#'
#' @param obs Vector with observed data
#' @param pred Vector with predicted data
#' @return List with the values of rmse, r, nse and pbias
#'
#' @export
#'
#' @keywords statistics
#'
#'
gofs <-function(obs,pred) {
    rmse=sqrt(mean((obs-pred)^2))
    r=cor(obs,pred)
    nse=sum((obs-pred)^2) / sum((obs-mean(obs))^2)
    pbias= sum(obs-pred) / sum(obs)
    lista=c(rmse,r,nse,pbias)
    names(lista) = c("rmse","r","nse","pbias")
    return(lista)
}


