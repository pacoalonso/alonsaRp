#' Calibra el modelo de Muskingum
#'
#' @param i Vector with the inputs
#' @param o Vector with the outputs
#' @param dt Temporal step
#' @param s1 Parámetro
#'
calibra_muskingum <- function(i, o, dt, s1) {
    s=i;s[1]=s1
    for (t in 2:length(i)) s[t] = s[t-1] + dt * ( (i[t]+i[t-1]) - (o[t]+o[t-1]) ) / 2

    a = ( sum(o^2)*sum(s*i) - sum(o*i)*sum(s*o) ) / ( sum(i^2)*sum(o^2) - sum(o*i)^2 )
    b = ( sum(i^2)*sum(s*o) - sum(o*i)*sum(s*i) ) / ( sum(i^2)*sum(o^2) - sum(o*i)^2 )
    lista=c(a+b,a/(a+b)); names(lista) = c("k (seg.)","x")
    return(lista)
}


#' Calibra el modelo de Muskingum
#'
#' @param i Vector with the inputs
#' @param k Parameter k
#' @param x Parameter x
#' @param o1 Parámetro
#' @param dt Temporal step
#'
muskingum <- function(i,k,x,o1, dt) {
   dt2 = dt/2
   c0 = (-k*x + dt2) / (k*(1-x) + dt2)
   c1 = (k*x + dt2) / (k*(1-x) + dt2)
   c2 = (k*(1-x) - dt2) / (k*(1-x) + dt2)
    
   o=rep(NA,length(i));o[1]=o1

   for (t in 2:length(o)) o[t] = c0*i[t] + c1*i[t-1] + c2*o[t-1]

   return(o)
}

