#' Basic spatial statistics
#'
#' @param x Vector of X coordinates
#' @param y Vector of Y coordinates
#' 
#' @return x mean, y mean and sds
#'
#' @export
#'
PointAnal <- function(x,y){
   xm=mean(x);ym=mean(y)
   ds=sqrt(mean(((x-xm)^2 + (y-ym)^2)))
   return(xm=xm,ym=ym,ds=ds)
}

#' Funcion de vecino más próximo. 
#'
#' @param X una matriz con dos columnas (x e y)
#' @param A es el área. Si no se incluye se calcula como 	 A=(max(x)-min(x))*(max(y)-min(y))
#' @param d Distancia
#'
#' @return Una lista con la distancia al vecino más próximo de cada punto, d1m,d1a y R1. Si se le pasan unas distancias de  referencia, devuelve los valores de F(d)
#'
#' @export
#'
NearestNeig=function(X,A=NULL, d=NULL){
        x=X[,1];y=X[,2]
        d1=c()
	d1m=0
	if(is.null(A)) A=(max(x)-min(x))*(max(y)-min(y))
	for (i in 1:length(x)){
	    d1[i]=max(x)-min(x)
	    for (j in 1:length(x)){
	          if(i!=j) d1[i]=min(d1[i],distancia(x[i],x[j],y[i],y[j]),na.rm=T)
	    }
	    d1m=d1m+d1[i]
	}
	d1m=d1m/length(x)
	d1a=1/(2*sqrt(length(x)/A))
        if (!is.null(d)) {
            fd=c()
            for (i in 1:length(d)) fd[i]=length(which(d1<d[i]))/length(d1)
        }

        return(list(d1=d1,d1m=d1m,d1a=d1a,R1=d1m/d1a,distan=d,Fdistan=fd))
}

#' Calcula distancia de Minkowski
#'
#' @param x1 Coordenada X del primer punto
#' @param x2 Coordenada X del segundo punto
#' @param y1 Coordenada Y del primer punto
#' @param y2 Coordenada Y del segundo punto
#' @param p Exponente de la distancia de Minkowski
#'
#' @return Minkowski's distance
#' 
#' @export
#'
distancia=function(x1,x2,y1,y2,p=2){((x1-x2)^p + (y1-y2)^p)^(1/p)}


#' Analiza grafo
#'
#' @param l Tramos
#' @param n Nodos
#' 
#' @return Several graph indices
#'
#' @export
#'
grafo <- function(l,n) {
   g=l/(3*(n-2))  # Indice gamma
   c=l-n+1       # Número de circuitos
   cmax=2*n-5    # Número máximo de circuitos
   a=c/cmax      # alfa
   res=c(g,c,cmax,a); names(res)=c("g","c","cmax","a")
   return(res)
}

