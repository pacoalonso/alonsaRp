#' Función para validación cruzada uno a uno de Regression(lm)-krigging
#' @param variable Variable dependiente
#' @param predictores Vector con los predictores que vamos a utilizar 
#' @param data Data.frame con los datos
#' 
#' @importFrom gstat krige
#' @importFrom automap autofitVariogram
#'
#' @export 
#'
fun.CVRK <- function(variable,predictores,data){

  formcvk<-"residuales ~ 1"
  cvtablaval <- data
  cvtablaval$predlm<-NA
  cvtablaval$predko<-NA

  for (i in 1:length(data[[variable]])) {

    #datos
    cvlmobs <- data[i,]
    cvlmresto <- data[-i,]

    #lm
    cvlmint=lm(paste(variable,"~",paste(predictores,collapse="+")), data=cvlmresto)
    cvpredictlmint=predict(cvlmint,cvlmobs)

    #espacial residuos
    cvlmresto$residuales <- cvlmint$residuals
    cv.variogram=autofitVariogram(as.formula(formcvk),cvlmresto)
    cvlmobs2 <- krige(as.formula(formcvk), cvlmresto, cvlmobs, model=cv.variogram$var_model)
    cvtablaval$predlm[i]=cvpredictlmint[[1]]
    cvtablaval$predko[i]=cvlmobs2[["var1.pred"]]
    rm(cvpredictlmint,cvlmobs2,cvlmobs,cvlmresto,cvlmint)
  }	
  cvtablaval<-subset(cvtablaval,is.na(cvtablaval$predko) == F) #limpio nulos	
  return(data.frame(observed=cvtablaval[[variable]],
                    cvpred=cvtablaval$predlm + cvtablaval$predko,
                    residual=cvtablaval[[variable]] - (cvtablaval$predlm + cvtablaval$predko))
        )
}

#' Función para validación cruzada uno a uno de regresión lineal
#'
#' @param variable Variable dependiente
#' @param predictores Vector con los predictores que vamos a utilizar 
#' @param data Data.frame con los datos
#' @export 
#'
fun.CVLM <- function(variable,predictores,data){
  pred=c()
  for (i in 1:length(data[[variable]])) {
    mod <- lm(paste(variable,"~",paste(predictores,collapse="+")), data=data[-i,])
    pred[i] <- predict(mod,data[i,])
   }
  return(data.frame(observed=data[[variable]],
                    cvpred=pred,
                    residual=data[[variable]] - pred)
        )
}


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
         w <- which(x==from[i])
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
#' @param pid Vector con los valores del punto ideal. Por defecto se toma el máximo de cada criterio
#' @param pai Vector con los valores del punto antiideal. Por defecto se toma el mínimo de cada criterio
#' @param w Vector con los coeficientes de ponderación de los criterios. Por defecto ponderan todos igual.
#'
#' @return Un data.frame con la distancia a los puntos ideal y antiideal y el resultados de Topsis.
#'
#' @export
#'
EMC_topsis <- function(matriz, pid=NULL, pai=NULL, w=NULL) {
    if(is.null(w)) w <- rep(1/ncol(matriz),ncol(matriz))
    if(is.null(pid)) pid <- apply(matriz,2, max, na.rm=TRUE)
    if(is.null(pai)) pai <- apply(matriz,2, min, na.rm=TRUE)

    pid2 <- matrix(rep(pid, each=nrow(matriz)),nrow=nrow(matriz))
    pai2 <- matrix(rep(pai, each=nrow(matriz)),nrow=nrow(matriz))
    w2 <-   matrix(rep(w,   each=nrow(matriz)),nrow=nrow(matriz))

    m2a = (matriz-pid2)^2 * w2
    m2b = (matriz-pai2)^2 * w2

    dpid <- apply(m2a, 1, sum)
    dpai <- apply(m2b, 1, sum) 

    tp <- dpai / (dpai+dpid)
    return(data.frame(dpi=dpid,dpai=dpai,topsis=tp))
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
    res <- as.matrix(matriz) %*% as.matrix(w)
    return(res)
}

#' Calcula el factor del índice óptimo para una imagen de satélite en un objeto raster
#'
#' @param rast Objeto raster
#'
#' @importFrom raster cellStats layerStats nlayers 
#' @importFrom methods as
#'
#' @return Data.frame with band names and OIF
#'
#' @export
#'
RS_oif <-function(rast) {
   nl <- nlayers(rast)
   sds <- cellStats(rast, "sd", na.rm=TRUE)
   corr <- abs(layerStats(rast, "pearson", na.rm=TRUE)[[1]])
   resultados <- matrix(rep(NA,4 * choose(nl, 3)),ncol=4)

   i=0
   for (b1 in 1:(nl - 2)){
      for (b2 in (b1+1):(nl - 1)){
         for (b3 in (b2+1):nl){
            i=i+1
            num <- sum(sds[c(b1, b2, b3)])
            den <- corr[b1, b2] + corr[b2, b3] + corr[b1, b3]
            resultados[i,]=c(b1, b2, b3, num / den)
   }}}
   r <- order(resultados[,4])
   resultados <- as.data.frame(resultados[rev(r),])
   names(resultados)=c("Banda 1","Banda 2", "Banda 3", "OIF")
   return(resultados)
}


#' Rescale the layers of a raster object
#'
#' @param rast Objeto raster
#' @param to Vector with the range of the resulting layers
#'
#' @importFrom raster values extent dropLayer addLayer extent<-
#' @importFrom sp proj4string
#' @importFrom scales rescale
#'
#' @return A new raster object with the layers rescaled
#'
#' @export
#'
RS_rescale <-function(rast, to) {
   nl <- nlayers(rast)
   for (b in 1:nl){
       xx <- round(rescale(values(subset(rast,b)), to=c(0,255)))
       rr <- raster(t(matrix(xx, ncol=nrow(rast))))
       extent(rr)=extent(rast)
       proj4string(rr) = proj4string(rast)
       rast=dropLayer(addLayer(rast,rr),1)
   }
   return(rast)
}

#' Aplica k-means a un objeto raster y le añade una capa con la clasificación resultante
#'
#' @param rast Objeto raster
#' @param centers Número de clases
#' @param sample Número de celdillas a incluir en el muestreo
#' @param iter.max Número máximo de iteraciones
#'
#' @importFrom raster extent extent<- addLayer sampleRandom raster
#' @importFrom clue cl_predict
#' @importFrom sp proj4string
#' @importFrom methods as
#'
#' @return The input raster object with a new layer that includes the classification.
#'
#' @export
#'
RS_raster_kmeans <- function(rast, centers, sample=10000, iter.max=100) {
     sr <- sampleRandom(rast, sample)
     km <- kmeans(sr, centers=10, iter.max=iter.max)
     xx=cl_predict(km,as(rast, "SpatialGridDataFrame")@data)
     rr <- raster(t(matrix(xx, ncol=nrow(rast))))
     extent(rr)=extent(rast)
     proj4string(rr) = proj4string(rast)
     return(addLayer(rast,rr))    
}

