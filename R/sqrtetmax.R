#' Evaluación de parámetros de la distribución sqrt-ET max
#'
#' @param m Media
#' @param cv Coeficiente de variación
#' 
#' @return sqrt-ET max distribution parameters
#'
#' @export
#'
SqrtETmaxPar <- function(m, cv) {
 
   a = c(1.318615,1.801513,-1765.86,   #a0
         -3.16463,2.473761,-7240.6,    #a1
         -1.59552,23.556200,-11785.6,  #a2
         -6.26911,49.957274,-9538.0,   #a3
         -11.3177,59.775636,-3834.3,   #a4
         -22.6976,35.696876,-612.68,   #a5
         -22.0663,8.505713,0.000)      #a6 
   b = c(2.307319,2.342697,-0.931508,   #b0
        -0.136674,-0.149784,2.156709,  #b1
        -0.075036,-0.099312,-0.779770, #b2
        -0.013464,0.003444,0.112962,   #b3
        0.003228,0.001014,-0.009340,   #b4
        0.000521,-0.000141,0.000412,   #b5
       -0.000141,0.000005,-0.000008)   #b6
   dim(a)=dim(b)=c(3,7)

   if (cv<=0.99 & cv>=0.7) col=1
   if (cv<=0.7 & cv>=0.3)  col=2
   if (cv<=0.3 & cv>=0.19) col=3

   lk = 0; for (i in 0:6) lk = lk + a[col,i+1]* log(cv)^i
   li = 0; for (i in 0:6) li = li + b[col,i+1]* lk^i
   alfa = exp(lk)*exp(li) / (1-exp(-exp(k)) *2*m)
   k = exp(lk)
   return(c(alfa=alfa,k=k))
}

#' Función de distribución sqrt-ET max
#'
#' @param x A vector of x values
#' @param k Parameter k
#' @param a Parameter a
#'
#' @return distribution function
#'
#' @export
#' 
pSqrtETmax <- function(x,k,a) exp(-k*(1+sqrt(a*x))*exp(-sqrt(a*x)))

#' Función de densidad sqrt-ET max
#'
#' @param x A vector of x values
#' @param k Parameter k
#' @param a Parameter a
#'
#' @return mass densitiy function
#'
#' @export
#'
dSqrtETmax <- function(x,k,a) {
    if (x==0) return(0)
    if (x>0) {
       x1 = max(0,x-0.5)
       x2 = x+0.5
       num = pSqrtETmax(x2, k, a) - pSqrtETmax(x1, k, a)
       return(num * (x2-x1) )
    }
}

#' Función de cuantiles de la distribución sqrt-ET max
#'
#' @param q Cuantile 
#' @param k Parameter k
#' @param a Parameter a
#'
#' @return x value corresponding to the q quantile
#'
#' @export
#'
qSqrtETmax <- function(q, k, a) {
   pp = seq(0,1000,by=0.1)
   prob = pSqrtETmax(pp, 20, 2)
   pp[which(prob>q)[1]]
}

#' Función de simulación de la distribución sqrt-ET max
#'
#' @param n Number of results 
#' @param k Parameter k
#' @param a Parameter a
#'
#' @return n numbers from an sqrt-ET max distribution
#'
#' @export
#'
rSqrtETmax <- function(n, k, a) {
    q=runif(n)
    res=c()
    for (i in 1:n) res[i] = qSqrtETmax(q[i], k, a)
    return(res)
}

