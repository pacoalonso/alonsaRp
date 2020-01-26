#'
#' Gráfico de una matriz de confusión
#'
#' @param CMM Confusion matrix
#' @param col Colour palette
#' @param names Names of the classes
#' @param ylim Limits of y axis
#' @param pos Position of the legend
#' @param ncol Number of columns in the legend
#' @param plot "omission" or "commission"
#' @param angle Angle for the labels
#' @param cex.names Size of names
#' @param margen Vector for par()$mar
#'
#' @export
#'

plot_cm <- function(CMM,names=1:ncol(CMM),col=1:ncol(CMM), ylim=NULL, pos=NULL, ncol=2, plot="omission", angle=90, cex.names=1, margen=c(8,4,2,2)) {
   if (is.null(ylim)) ylim=c(0,1)
   if (plot=="commission") denSum=apply(CMM,1,sum)
   if (plot=="omission")   denSum=apply(CMM,2,sum)
   CMM3=CMM2=CMM
   diag(CMM2)=0
   if (plot=="omission")   for (i in 1:ncol(CMM)) CMM3[,i]=CMM2[,i]/denSum[i]
   if (plot=="commission") for (i in 1:ncol(CMM)) CMM3[,i]=CMM2[i,]/denSum[i]
  
   par(xpd=NA, mar=margen)
   barplot(CMM3, ylim=ylim, col=col, xlab="", ylab="Error", space=1) 
   box()
   if (!is.null(pos)) legend(pos,fill=col,legend=names,ncol=ncol)

   end_point = 2*nrow(CMM) - 0.5 
   xx=seq(1.5,end_point,by=2)
   text(xx, rep(-ylim[2]*0.05,length(xx)), srt = angle, adj= 1, xpd = NA, labels = names, cex=cex.names)
}

