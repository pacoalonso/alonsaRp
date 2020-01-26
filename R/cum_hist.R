#' Función para hacer histogramas acumulados
#'
#' @param x Vector of values
#' @param probability If TRUE shows probability instead of frequency
#' @param xlab Etiqueta para el eje de abcisas
#' @param ylab Etiqueta para el eje de ordenadas
#' @param main Tïtulo principal
#'
#'
#' @export
#'
cumHist <- function(x, probability=FALSE, main=NULL, xlab=deparse(substitute(x)), ylab=NULL) {
    xx = table(x)
    f = cumsum(as.numeric(xx))
    v = as.numeric(names(xx))
    ff = c(rep(f,each=2),0)
    vv = c(min(v),rep(v,each=2))
    if (probability) ff=ff/max(ff)
    if (is.null(ylab)) {if (probability) ylab="probability" else  ylab="frequency"}
    plot(vv,ff,type="l", xlab=xlab, ylab=ylab, main=main)
    for (i in 2:(length(vv)-1)) lines(c(vv[i],vv[i]),c(0,ff[i]))
    lines(c(min(v),max(v)),c(0,0))
}
