#' Escribe una columna de un SpatialGridDataFrame como una capa raster
#'
#' @param SGDF El SpatialGridDataFrame
#' @param file Fichero que contendrá los datos
#' @param col Columna que contiene la capa que se guarda. Puede pasarse su nombre o su número de orden
#'
#' @export
#'
writeGDAL_selCol <-function(SGDF, file, col) {
   SGDF@data <- as.data.frame(SGDF@data[,col])
   writeGDAL(SGDF, file)
}     


