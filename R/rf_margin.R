#' Función para calcular el margen de Random Forest
#'
#' @param predVotos Votes for each class
#' @return Difference between the two most voted classes
#'
rf_margin <- function(predVotos) {
   margin=apply(predVotos,1,function(x) rev(sort(x))[1]-rev(sort(x))[2])
}

