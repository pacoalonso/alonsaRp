#' Función para calcular intervalos de clase para una capa raster
#'
#' @param capa Layer's name
#' @param n Number of intervals
#' @param style One of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks" or "dpih"
#' @param grass If TRUE the layer is loaded from the GRASS location
#' @param column Column of the object that contains the data
#' @param fixed Fixed intervals in case style="fixed"
#'
#' @return Colour intervals 
#'
#' @export
#'
intervalos = function(capa, n, style="quantile", grass=FALSE, column=1, fixed=NULL){
    if (class(capa) == "character") {
   	   if (grass) datos <- rgrass7::readRAST(c(capa)) else datos=rgdal::readGDAL(capa)
       intervalos=classInt::classIntervals(datos@data[, column], n, style=style) 
    }
    if (class(capa) == "SpatialGridDataFrame") {
        intervalos=classInt::classIntervals(capa@data[, column],n,style=style	) 
    }    
    if (class(capa) == "numeric") {
        intervalos=classInt::classIntervals(capa, n, style=style	) 
    }
	return(intervalos)

   #,sample=F,sampleSize=100,
   #if(sample){
   #       z2=sample(z[!is.nan(z)],sampleSize)
   #       z2=c(z2,min(z),max(z))
   # }else{
   #       z2=z
   # }
   #
   # if(style=="fixed"){
   #      clases=classIntervals(z2,n,style=style,fixedBreaks=fixedBreaks)     # Calculo intervalos
   # }else{clases=classIntervals(z2,n,style=style)}

   # if(sample){
   #    clases$brks[1]=min(z,na.rm=T)
   #    clases$brks[n+1]=max(z,na.rm=T)
   #    clases$var=z
   # } 


}


