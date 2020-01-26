
#' Obtiene un hietograma de diseño para un episodio con intensidad máxima en 24 horas de I24, un valor de I1/I24 igual a I1I24 y una duración de D horas. La resolución temporal es de dt horas. El parámetro normal indica si el hietograma resultante debe normalizarse para que su suma sea igual a I24.
#'
#' @param I24 Intensidad máxima en 24 horas
#' @param I1I24 Valor de I1/I24 
#' @param dt Incremento temporal
#' @param D duración en horas
#' @param normal Indica si el hietograma resultante debe normalizarse para que su suma sea igual a I24.
#'
#' @return Un hietograma
#'
#' @export
#'
IDF <- function(I24, I1I24, dt=1, D=24, normal=TRUE) {
   yeto0=rep(NA,D/dt)
   for (i in 1:length(yeto0)) {
       int = id(I24, I1I24, dt*i)                 # Intensidad para cada duración
       yeto0[i] = int*dt*i                        # Volumen acumulado 
   }
   yeto1 = (c(yeto0[1], diff(yeto0)))             # Diferencia de los vols. acum.
   impares=seq(1, length(yeto1), 2)               # Selecciono intervalos impares
   yeto = c(rev(yeto1[impares]),yeto1[-impares])  # Reordeno intervalos 
   if (normal) yeto=yeto*I24*24/sum(yeto)         # Normalizo el hietograma si hace falta
   return(yeto)
}

#' Calcula la intensidad máxima en mm/h para una duración de dt en un episodio con intensidad máxima en 24 horas de I24 y con un valor de I1/I24 igual a I1I24.
#'
#' @param I24 Intensidad máxima en 24 horas
#' @param I1I24 Valor de I1/I24 
#' @param dt Duración para la que se calcula la intensidad máxima
#'
#' @return An intensity value
#'
#' @export
#'
id <-function(I24, I1I24, dt=1) {
     exp = (28^0.1 - dt^0.1)/(28^0.1 - 1)            
     return(I24*I1I24^exp)
}

