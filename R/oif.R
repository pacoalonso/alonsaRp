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
