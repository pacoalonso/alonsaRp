#' Función para calcular CM, OA, K, EC y EO
#'
#' @param pred Predicted values
#' @param ref Reference values)
#' @param labels Labels for the classes
#'
#' @return The confusion matrix
#'
#'
#' @export
#'
confusionMatrix <- function(pred, ref, labels=NULL) {
    CM=table(pred,ref)
    if (!is.null(labels)) row.names(CM) <- colnames(CM) <- labels
    #stat=kappa(CM)
    return(CM)
}

