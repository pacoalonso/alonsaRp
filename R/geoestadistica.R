#' Cálcula semivariogramas teóricos
#' @param tipo Tipo de semivariograma
#' @param c0 Pepita
#' @param c1 Meseta
#' @param a rango 
#' @param amp Amplitud para el semivariograma de efecto agujero
#' @param freq Algo del efecto agujero
#' @param h distancias
#'
#' return Semivariograma teórico
#'
#' export
#'
semivariograma <- function(tipo,c0,c1,a,h, amp, freq=1) {
    if (tipo=="esf") {
      g=c()
      for (i in 1:length(h)){
 	 if(h[i]>a){g[i]=c0+c1}
         if(h[i]<=a){g[i]=c0 + c1 * ( (3*h[i])/(2*a) - ((h[i]/a)^3)/2) }
         #cat(h[i],(3*h[i])/(2*a),((h[i]/a)^3)/2,g[i],"\n")
      }
      return(g)
    }

    if (tipo=="agujero") return(c0 + h/10 + amp*h*sin(2*pi*freq*h))
    if (tipo=="gaus")    return(c0+c1*(1-exp(-(h^2)/(a^2))))
    if (tipo=="expo")    return(c0 + c1 *(1-exp(-h/a)))
    if (tipo=="lin")     return(c0+c1*h)
}

