#' Convolución del hietograma con un hidrograma unitariopara obtener el hidrograma
#'
#' @param p es el hietograma en mm
#' @param hu es el HU como proporción
#' @param area es el área de la cuenca en km2
#' @param dt es el paso temporal de p y hu en horas
#'
#' @return hidrograma de crecida en m3/sec
#'
#' @export
#'
hu_conv <- function(p, hu, area, dt) {
   area = area * 1000000           # paso el área a m2
   dt = dt * 3600                  # paso el intervalo a segundos
   h=rep(0,length(p)+length(hu)-1) # preparo el vector de resultados
   for (t in 1:length(p)) {
        h[t:(t+length(hu)-1)] = h[t:(t+length(hu)-1)] + p[t]*hu
   }
   h=h/1000 # paso de l/m2 a m3/m2
   return(h*area/dt)
}

#' Deconvolución del hidrograma para obtener un HU
#' @param p es el hietograma en mm
#' @param q es el hidrograma en m3/s
#' @param prop Si prop==TRUE devuelve el HU como proporciones, si prop==FALSE devuelve el HU como m3/s/mm
#'
#' @return hidrograma unitario instantaneo
#'
#' @export
#'
HUdeconv <- function(p,q, prop=FALSE) {
    n=1+length(q)-length(p)
    u=rep(NA,n)
    for (i in 1:n){
       num = q[i]
       if (i>1) {
          for (j in 2:length(p)) { 
             if ((1+i-j)>0) num = num - p[j]*u[1+i-j]             
          }
       }
       u[i] = num/p[1]
    }
    if (prop) u = u/sum(u)
    return(u)
}

