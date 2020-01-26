
#' Convierte variables en criterios
#'
#' @param x Vector con la variable a transformar
#' @param var Vector con los valores de la variable en los puntos de inflexión del gráfico
#' @param cri Vector con los valores del criterio en los puntos de inflexión del gráfico
#' @param from Vector con los valores de la variable para reclasificar
#' @param to Vector con los valores del criterio tras reclasificar
#'
#' @return Vector con los valores de criterio
#' 
#' @export
#'
EMC_var2crit <- function(x, var, cri=NULL, from=NULL, to=NULL) {
   y=x
   if (!is.null(cri)) {
      for (i in 1:(length(var)-1)) {
         w <- which(x>=var[i] & x<var[i+1])
         m <- (cri[i+1] - cri[i]) / (var[i+1] - var[i])
         y[w] = cri[i] + m * (x[w]-var[i])
      }
   }
   if (!is.null(to) & !is.null(from)) {
      for (i in 1:length(from)) {
         w <- which(x>=from[i])
         y[w] <- to[i]
      }
   }
   return(y)
}



#' Proceso de Jerarquía Analítica
#'
#' @param matriz Matriz de preferencias
#'
#' @return Un vector con los pesos y la razón de consistencia.
#'
#' @export
#'
EMC_ahp <- function(matriz) {
 
   iam=c(0,0,0.58,0.9,1.12,1.24,1.37,1.41,1.45,1.49)
   
   matriz2=matriz
   for (i in 1:3) matriz2[,i] = matriz[,i]/sum(matriz[,i]) 
   w = apply(matriz2,1,sum)/3 

   lmax = (eigen(matriz)$values[1])
   n = nrow(matriz)

   rc=(as.numeric(lmax)-n)/(iam[n]*(n-1))

   return(list(w=w,lmax=lmax,rc=rc))
}

#' Calculo de pesos para diferentes criterios. Si solo se le pasan los criterios utilizaa jerarquía analítica, si se le pasan las importancia utiliza tasación simple y si se le pasa la matriz utiliza el proceso de jerarquía analítica.
#'
#' @param criterios Vector con los nombres de los criterios en orden de importancia
#' @param importancia Vector con los valores de importancia absoluta de cada criterio
#' @param matriz Matriz de comparación por pares
#'
#' @return Un vector con los pesos salvo cuando se utiliza AHP, en este caso se devuelve también la razón de consistencia.
#'
#' @export
#'
EMC_pesos_criterios <- function(criterios, importancia=NULL, matriz=NULL) {
   if (is.null(matriz) & is.null(importancia)) {
      total <- sum(1:length(criterios))
      pesos <- rev((1:length(criterios))/total)
      return(pesos)
   } 
   if (is.null(matriz) & !is.null(importancia)) {
      total <- sum(importancia)
      pesos <- importancia/total
      return(pesos)
   } 
   if (!is.null(matriz)) {
      return(EMC_ahp(matriz))
   }
}

#' Calculo de distancia al punto ideal, antiideal y método Topsis.
#'
#' @param matriz Matriz que contiene en filas las alternativas y en columnas los criterios
#' @param pi Vector con los valores del punto ideal. Por defecto se toma el máximo de cada criterio
#' @param pai Vector con los valores del punto antiideal. Por defecto se toma el mínimo de cada criterio
#' @param w Vector con los coeficientes de ponderación de los criterios. Por defecto ponderan todos igual.
#'
#' @return Un data.frame con la distancia a los puntos ideal y antiideal y el resultados de Topsis.
#'
#' @export
#'
EMC_topsis <- function(matriz, pi=NULL, pai=NULL, w=NULL) {
    if(is.null(pi)) pi <- apply(matriz,2, max)
    if(is.null(pai)) pai <- apply(matriz,2, min)
    if(is.null(w)) w <- rep(1/ncol(matriz),ncol(matriz))

    dpi <- dpai <- c()

    for (i in 1:nrow(matriz)){
      dpi[i] <- sqrt(sum(w*(pi-matriz[i,])^2))
      dpai[i] <- sqrt(sum(w*(pai-matriz[i,])^2))
    }
    tp <- dpai / (dpai+dpi)
    return(data.frame(dpi=dpi,dpai=dpai,topsis=tp))
}


#' Calculo de combinación lineal en EMC.
#'
#' @param matriz Matriz que contiene en filas las alternativas y en columnas los criterios
#' @param w Vector con los coeficientes de ponderación de los criterios. Por defecto ponderan todos igual.
#'
#' @return Un vector con la adecuación de cada alternativa.
#'
#' @export
#'
EMC_combinacion <- function(matriz, w=NULL) {
    if(is.null(w)) w <- rep(1/ncol(matriz),ncol(matriz))
    res <- c()
    for (i in 1:nrow(matriz)) res[i] <- sum(w*matriz[i,])
    return(res)
}



