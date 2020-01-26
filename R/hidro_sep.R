#' Separación de escorrentía base y escorrentía directa en un hidrograma
#'
#' @param Q Vector de caudales
#' @param t Vector de tiempos
#' @param t1 Tiempo 1
#' @param t2 Tiempo 2
#' @param metodo Método de separación
#' @param xlab Rótulo para eje X
#' @param ylab Rótulo para eje Y
#' @param plot TRUE/FALSE
#' @param q0 Caudal 0 para el método 5
#' @param t3 Tiempo 3 para el método 5
#' @param t4 Tiempo 4 para el método 5
#' @param q3 Caudal 3 para el método 5
#' @param q4 Caudal 4 para el método 5
#'
#' @export
#'
hidroSep <- function(Q,t=NULL,t1=NULL, t2=NULL, metodo=NULL, 
                      xlab="Tiempo", ylab="Q", plot=TRUE, 
                      q0=NULL, t3=NULL, t4=NULL, q3=NULL, q4=NULL) {

    if (plot) plot(Q, type="l", xlab=xlab, ylab=ylab, ylim=c(0,max(Q)))
    if (is.null(metodo)) return()
    tp=which.max(Q)
    if (is.null(t1)) t1=which.min(Q[1:tp])
    if (is.null(t2)) t2=tp+which.max(diff(diff(Q[tp:length(Q)])))+1


    if (metodo==1) {
         t2=which(Q<=Q[t1])[2]
         if (t2<length(Q)) t2 = t2+1 - (Q[t1]-Q[t2+1])/(Q[t2]-Q[t2+1])          
         Qb2= rep(Q[t1],t2-(t1+1))
         Qb=c(Q[1:t1],Qb2,Q[t2:length(Q)])
    }
    if (metodo==3) {
         b = Q[t1-1]-Q[t1]
         qp = Q[t1] - (tp-t1)*b 
         Qb2a = Q[t1] + (1:(tp-(t1))) * (Q[t1]-Q[1])/(t1-1)
         Qb2b = qp + (1:(t2-(tp+1))) * (qp-Q[t2])/(tp-t2)
         Qb = c(Q[1:t1],Qb2a, Qb2b,Q[t2:length(Q)])

    }
    if (metodo==2) {
         Qb=Q[1:t1]
         Qb2= Q[t1] + (1:(t2-(t1+1))) * (Q[t1]-Q[t2])/(t1-t2)
         Qb=c(Qb,Qb2,Q[t2:length(Q)])
    }
        
    if (metodo==4) {
         tt=c(t1,tp,t2); qq=c(Q[t1], q0, Q[t2])
         m=lm(qq~tt+I(tt^2))
         qq2=predict(m ,data.frame(tt=c((t1+1):(t2-1))))
         Qb=c(Q[1:t1],qq2,Q[t2:length(Q)])         
    }

    if (metodo==5) {
         tt=c(t1,t3,t4,t2); qq=c(Q[t1], q3, q4, Q[t2])
         m=lm(qq~tt+I(tt^2)+I(tt^3))
         qq2=predict(m ,data.frame(tt=c((t1+1):(t2-1))))
         Qb=c(Q[1:t1],qq2,Q[t2:length(Q)])         
    }

    if (plot) lines(1:length(Qb), Qb, col="blue")
    return(data.frame(Q=Q, Qb=Qb, Qd=Q-Qb))
}

