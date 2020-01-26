#' Crea paletas de color
#' 
#' @param color Tipo de paleta. Uno de "brewer", "rainbow", "heat", "terrain", "topo", "cm" o "ramp"
#' @param brewer Tipo de paleta Brewer
#' @param n Número de colores en la paleta
#' @param inifin Vector con colores
#' 
#' @return Paleta de color
#' 
#' @import grDevices
#' @import rgrass7
#' @import rgdal
#' @import RColorBrewer
#' @import classInt 
#'
#' @export
#'
creaPaleta=function(color="brewer",brewer="Blues", n=5, inifin=c("red","blue")){
			
    if(color=="brewer"){df=RColorBrewer::brewer.pal(n,brewer)}     
    if(color=="rainbow"){df=rainbow(n)}
    if(color=="heat"){df=heat.colors(n)}
    if(color=="terrain"){df=terrain.colors(n)}
    if(color=="topo"){df=topo.colors(n)}
    if(color=="cm"){df=cm.colors(n)}
    if(color=="ramp"){
	nk=1+(n-1)/(length(inifin)-1)

	df=c()
	for (i in 1:(length(inifin)-1)){
	      if(i==length(inifin)-1){
	         df=c(df,colorRampPalette(c(inifin[i],inifin[i+1]))(nk))
	      }else{
	         df=c(df,colorRampPalette(c(inifin[i],inifin[i+1]))(nk)[-nk])
	      }
	}
    }

    return(df)
}

#' Writes a GRASS colr file
#' 
#' @param capa GRASS layer
#' @param paleta Vector of colors
#' @param intervalos Color intervals
#' @param type Type of layer ("rast" or "vect"
#' @param columna Columna del vectorial que contendrá datos de color
#' 
#' @return Paleta de color
#'
#' @export
#'
col2GRASS <- function(capa,paleta, intervalos, type="rast", columna) {
   b <- intervalos$brks
   col <- col2rgb(paleta)
   if (type=="rast") lineas="echo \"" else lineas=""
   for (i in 1:(length(b)-1)) {
      color = paste0(col[,i],collapse=":")
      lineas = paste0(lineas,b[i], " ", color, " ", b[i+1], " ", color, "\n")
   }
   color = paste0(col[,length(b)-1],collapse=":")
   lineas = paste0(lineas,b[length(b)], " ", color)
   if (type=="rast") {
       lineas=paste0(lineas,"\"|r.colors ", capa, " rules=-")
       cat(lineas)
       system(lineas)
   }
   if (type=="vect") {
       tfil=tempfile()
       cat(lineas,file=tfil)
       orden=paste0("v.colors ", capa, " rules=",tfil, " rgb_column=GRASSRGB use=attr column=",columna)
       cat(orden)
       system(orden)
       system(paste("rm",tfil))
   }
}

#' Draw a palette
#'
#' @param col Paleta
#' @param border Color del borde
#' @param intervals Intevalos de la paleta
#' @param main Título
#' @param horiz TRUE/FALSE
#' 
#' @export
#'
pintaPal=function(col,border="black", intervals=NULL, main="", horiz=TRUE){
	n=length(col)
	plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="",main=main)
	if (horiz) {
         rect(0:(n-1)/n,0,(1:n)/n,0.9,col=col,border=border)
         if (!is.null(intervals)) {
             text(0:n/n,0.95,round(intervals))
         }
    } else {
         rect(0,0:(n-1)/n,0.9,(1:n)/n,col=col,border=border)
         if (!is.null(intervals)) {
             text(0.95,0:n/n,round(intervals))
         }  
    }
}


#' Coloured Histogram 
#'
#' @param z Data
#' @param colores Palette
#' @param intervals Object with intervals
#' @param breaks System to calculate histogram breaks
#' @param thres TRUE/FALSE
#' @param main Main text
#' @param xlab Xlab text
#'
#' @export
#'
HistPal=function(z, colores, intervals, main="",breaks="Sturges", thres=FALSE, xlab=""){
	kk=hist(z, main=main,breaks=breaks,xlab=xlab)
	for (i in 1:(length(kk$breaks)-1)){
         inc=(kk$breaks[i+1]-kk$breaks[i])/100
	     for (j in seq(kk$breaks[i],kk$breaks[i+1],by=inc)){
        	  h=findInterval(j,intervals$brks)
        	  cl=colores[h];if(h==0) cl="white"
        	  lines(c(j,j),c(0,kk$counts[i]),col=cl)
       	 }
	}
    if (thres){
         k=intervals$brks
         for (i in 1:length(k)){lines(c(k[i],k[i]),c(0,max(kk$counts)),lty=2)}
    }
	kk=hist(z,breaks=breaks, add=T,xlab="")
}

