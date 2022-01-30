dxdy2wh <- function(dx,dy,e) {
    i2cm <- 2.54
    cm2i <- 1/i2cm

    return( list(w=e*dx*100*cm2i, h=e*dy*100*cm2i) )
}


#' Funcion para modificar un bounding box
#'
#' @param  bb bounding box a modificar
#' @param l margen a anadir (en metros)
#'
#' @export
#'
largerBbox <- function(bb,l) {
    bb[1,1] <- bb[1,1] - l
    bb[1,2] <- bb[1,2] + l
    bb[2,1] <- bb[2,1] - l
    bb[2,2] <- bb[2,2] + l

    return(bb)
}

#' Funcion para hacer mapas
#'
#' @param  pdfFile Output pdf file
#' @param  other Other plot instructions
#' @param  bbox Alternative bbox
#' @param  scale Map scale
#' @param  paper c(ancho, alto) en cm o iso
#' @param  mai Fix margins
#' @param  cex.axis Axis annotation size
#' @param  cex.lab Axis label size
#' @param  cex.leg Legend labels size
#' @param  xlab X axis label
#' @param  ylab Y axis label
#' @param  type "base" or "SGDF"
#' @param  SGDF Name of the raser map
#' @param  raster raster
#' @param  paleta Colour palette
#' @param  n Number of intervals in color palette
#' @param  mx Separacion en X desde la esquina inferior izquierda (cm)
#' @param  my Separacion en Y desde la esquina inferior izquierda (cm)
#' @param  show If TRUE show the PDF
#' @param  what what is to plot
#' @param  legtext Legend labels 
#' @param  breaks palette breaks
#' @param  col Column to represent
#' @param  portrait Portrait option
#' @param inifin Initial and final colors for palette
#' @param grid Grid definition
#' @param legend Legend position (word or two coordinates vector)
#' @param bg.leg Legend backgroud color
#' @param style Class interval style
#'
#' @import classInt raster RColorBrewer
#'
#' @export
#'
mapica <- function(pdfFile, other=NULL, bbox=NULL, scale=NULL, paper=NULL,
                   mai=NULL,cex.axis=1, cex.lab=1, cex.leg=1, xlab="X", ylab="Y",
                   type="base", SGDF=NULL, raster=NULL, paleta=NULL,
                   n=5, mx=NULL, my=NULL, show=FALSE, what="both", 
                   legtext=NULL, breaks=NULL, col=1, portrait=FALSE, inifin=NULL,
                   grid=NULL, legend=NULL, bg.leg="white", style="kmeans") {

   # Paper size
   i2cm <- 2.54
   cm2i <- 1/i2cm
   isopaper <- cm2i * c(841, 1189,  594, 841,  420, 594,   297, 420,  210, 297,   148, 210, 
                        105, 148,   74, 105,   52, 74,     37, 52,    26, 37) /10

   if (is.null(paper)) paper <- "a4"
   if (length(paper) == 1) {
       iso <- as.numeric(substr(paper, 2,2))
       heightPaper <- isopaper[iso*2+1]; widthPaper <- isopaper[iso*2+2]
       if (portrait) {temp <- widthPaper; widthPaper <- heightPaper; heightPaper <- temp}
   } else {
       widthPaper <- cm2i * paper[1]; heightPaper <- cm2i * paper[2]
   }

   # Image size
   if (is.null(bbox)) {
       if (type=="SGDF") bbox <- SGDF@bbox
       if (type=="base") bbox <- SGDF@bbox
   }
   bbox = list(e=bbox[1,2], w=bbox[1,1], n=bbox[2,2], s=bbox[2,1])  
   dy <- bbox$n-bbox$s
   dx <- bbox$e-bbox$w
   yx <- dy/dx
   widthImage <- dxdy2wh(dx,dy,scale)$w
   heightImage <- dxdy2wh(dx,dy,scale)$h

   # Margenes
   if (is.null(mx)) mx <- i2cm * (widthPaper - widthImage)/2
   if (is.null(my)) my <- i2cm * (heightPaper - heightImage)/2
   mai <- c(my*cm2i, mx*cm2i, heightPaper - (my*cm2i + heightImage), widthPaper - (mx*cm2i + widthImage))

   cat("dx=",dx, " dy=",dy,"\n")
   cat("widthImage=",widthImage, "inches heightImage=", heightImage, "inches\n",
       "widthPaper=", widthPaper, "inches heightPaper=",heightPaper,"inches\n")
   cat("mx=",mx, "cm  my=",my, "cm\nMargenes: inferior=",round(mai[1],2) ,"inches  izquierdo=",round(mai[2],2),"inches    superior=",round(mai[3],2),"inches  derecho=",round(mai[4],2),  "inches \n\n")    

   # Mapa
   pdf(pdfFile, width=widthPaper, height=heightPaper)  
      prvpar <- par()
      par(mai=mai, xpd=TRUE) 
      if (type=="base") {  # Mapa con el plot basico
          plot(0,0, type="n", xlab=xlab, ylab=ylab,
                   xlim=c(bbox$w,bbox$e), ylim=c(bbox$s,bbox$n), xaxs = "i",  yaxs = "i", asp=1,
                   cex.axis=cex.axis, cex.lab=cex.lab)
          if (!is.null(other)) {
             for (l in 1:length(other)) eval(parse(text=other[[l]]))
          }
          plot(SGDF, add=TRUE)
      }   

      if (type=="SGDF") {  # Mapa de un SGDF hecho con plot

         # Paleta
         if (is.null(paleta)) paleta = creaPaleta(n=n)
        # if (paleta=="ramp") paleta = creaPaleta(color=paleta, inifin=inifin, n=n)
         if (length(paleta)==1) paleta = creaPaleta(color=paleta, inifin=inifin, n=n)
         if (is.null(breaks)) breaks = classInt::classIntervals(SGDF@data[,col], n=n, style=style)$brks

         # image
         image(SGDF, atts=1, xlab=xlab, ylab=ylab, axes=TRUE, what=what, 
              col=paleta, breaks=breaks, cex.axis=cex.axis, cex.lab=cex.lab,
              xlim=c(bbox$w,bbox$e), ylim=c(bbox$s,bbox$n), xaxs = "i",  yaxs = "i", asp=1)

         # ordenes adicionales
         if (!is.null(other)) {
             for (l in 1:length(other)) eval(parse(text=other[[l]]))
         }    

         # lineas marco
         lines(c(bbox$e,bbox$w),c(bbox$s,bbox$s))
         lines(c(bbox$e,bbox$w),c(bbox$n,bbox$n))
         lines(c(bbox$e,bbox$e),c(bbox$s,bbox$n))
         lines(c(bbox$w,bbox$w),c(bbox$s,bbox$n))
      } 

      # Cuadricula
      if(!is.null(grid)) {
         for (x in seq(grid[1], bbox$e, grid[3])) {
            if (x>bbox$w) lines( c(x,x), c(bbox$s,bbox$n))
         }
         for (y in seq(grid[2], bbox$n, grid[3])) {
            if (y>bbox$s) lines(c(bbox$e,bbox$w), c(y,y))
         }
      }   

      # leyenda
      if(!is.null(legend)) {
          leyenda <- paste0(breaks[-length(breaks)], "-", breaks[-1])
          legend(legend[1],legend[2], fill=paleta, legend=leyenda, bg=bg.leg, cex=cex.leg)
      }
      
  
   dev.off()
   if (show) system(paste0("xpdf ",pdfFile," &"))
   par(prvpar)
}

