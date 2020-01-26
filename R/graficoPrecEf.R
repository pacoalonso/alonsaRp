#' Crea un gráfico con precipitación, interceptación, infiltración y precipitación efectiva. 
#'
#' @param p Precipitación
#' @param interc Interceptación
#' @param infil Infiltración
#' @param esco Escorrentía
#' @param q No sé
#' @param legpos Posición de la leyenda
#' @param col Paleta
#' @param leg Leyenda
#' @param xlim Límite X
#' @param qq No sé
#'
#' export
#'
graficoPrecEf <- function(p,interc,infil,esco, q=NULL, legpos="topright", col=NULL, leg=NULL, xlim=NULL, qq=NULL) {   
    if (is.null(q)) lhu=length(p) else lhu=length(q)
    if (is.null(xlim)) xlim=c(0, lhu)
    if (is.null(col)) col=c("black","green","red","cyan", "blue")
    if (is.null(leg)) leg=c("P","It","If","E","C")

    plot(p,type="l",
         xlab="Tiempo (horas)", ylab="Caudal (mm/hora)", 
         xlim=xlim, ylim=c(0,max(p)), 
         col=col[1]) 
 	lines(infil,col=col[3])
	lines(interc,col=col[2])
	lines(esco,col=col[4])
	if (is.null(q)) {
         legend(legpos,fill=col[1:4],legend=leg[1:4]) 
    } else {
         if (is.null(qq)) qq=c(0,max(q)*0.25,max(q)*0.5,max(q)*0.75,max(q))
         pq=0.5*q*max(p)/max(q)
         lines(pq,col=col[5], lwd=2)
         legend(legpos, fill=col,legend=leg)
         axis(4, at=0.5*qq*max(p)/max(q), labels=qq)
    }
}

