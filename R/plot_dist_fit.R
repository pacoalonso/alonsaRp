#' Gráfico de ajuste a una función de distribución
#'
#' @param x Datos
#' @param dist Función a probar ("pnorm","plnorm","ppearsonIII","pgumbel")
#'
#' @return Gráfico de ajuste de unos datos a una función de distribucion
#'
#' @importFrom moments skewness
#' @importFrom evd pgumbel
#' @importFrom PearsonDS ppearsonIII
#'
#' @export
#'
#'
plot_dist_fit <-function(x,dist) {  

     # Calculo los estadísticos
     if (dist=="pnorm") {m = mean(x); s = sd(x)}
     if (dist=="plnorm") {ml = mean(log(x)); sl = sd(log(x))}
     if (dist=="ppearsonIII") {
         g = skewness(x) 
         b = 4/(g^2)
         a = sqrt((sd(x)^2)/b)
         d = mean(x) -a*b
     }
     if (dist=="pgumbel") {a = 1.2825/sd(x); b = mean(x) - 0.45 *sd(x)}

     # Pinto los puntos
     plot(sort(x),(1:length(x))/(length(x)+1), ylab="F(x)")

     # Preparo secuencia de valores para dibujar la línea
     if (max(x)-min(x)<50) sx=seq(min(x),max(x),0.1) else sx=seq(min(x),max(x),1)

     # Pinto la línea y calculo Kolmogorov-Smirnoff (dependiendo de cual sea la distribución
     if (dist=="pnorm") {lines(sx,pnorm(sx,m,s)); ks = ks.test(x,pnorm,m,s)}
     if (dist=="plnorm") {lines(sx,plnorm(sx,ml,sl)); ks = ks.test(x,plnorm,ml,sl)}
     if (dist=="ppearsonIII") {lines(sx,ppearsonIII(sx,b,d,a)); ks = ks.test(x,ppearsonIII,b,d,a)}
     if (dist=="pgumbel") {lines(sx,pgumbel(sx,1/a,b)); ks = ks.test(x,pgumbel,1/a,b)}

     # Coloco el texto resumen de Kolmogorov-Smirnoff 
     tx = min(x) + 0.75 * (max(x)-min(x))
     text(tx,0.2,paste0("D = ",round(ks$statistic,3),"  p-value = ", round(ks$p.value,5)))
}


