#' Genera series de precipitación diaria a partir de un modelo markoviano y una distribución exponencial
#'
#' @param m01 Vector de probabilidad de día de lluvia tras día seco mensual
#' @param m11 Vector de probabilidad de día lluvioso tras día lluvioso mensual
#' @param pexp Vector de párametro exponencial mensual
#' @param n Number of days
#'
#' export
#'
genera_lluvia=function(m01,m11,pexp, n=365){
    mes=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
    total=ndias=rep(0,12);
    p=rep(0,365)

    for (t in 2:n){
       if(p[t-1]>0){pp=m11[mes[t]]}else{pp=m01[mes[t]]}
       if(pp>runif(1)){p[t]=rexp(1,pexp[mes[t]])}
       total[mes[t]]=total[mes[t]]+p[t]
       if(p[t]>0){ndias[mes[t]]=ndias[mes[t]]+1}
    }
    k=list(p,total,ndias,sum(total))
   return(k)
}

#' Modelo de Témez
#'
#' @param P Precipitación
#' @param ETP ETP
#' @param Hmax Hmax
#' @param Imax Imax
#' @param C Parámetro C
#' @param Hprev Humedad previa
#'
#' export
#'
Temez <- function(P,ETP,Hmax,Imax,C,Hprev) {
    H=ETR=I=R=rep(NA,length(P))
    for (t in 1:length(P)){
         dH=Hmax-Hprev                                       
         P0=C*dH                                             
         if (P[t]<=P0) {T=0}                                 
         if (P[t]>0) {T=((P[t]-P0)^2)/(P[t]+dH+ETP[t]-2*P0)} 
         H[t]=max(0,Hprev+(P[t]-T)-ETP[t])                   
         ETR[t]=min(Hprev+P[t]-T,ETP[t])                     
         I[t]=Imax*T/(T+Imax)                                
         R[t]=T-I[t]                                         
         Hprev=H[t]                                          
    }
    return(data.frame(P=P, ETP=ETP, ETR=ETR, H=H, I=I, R=R))
}


