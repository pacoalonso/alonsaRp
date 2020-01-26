#' Calcula distancia de Minkowski
#'
#' @param x1 Coordenada X del primer punto
#' @param x2 Coordenada X del segundo punto
#' @param y1 Coordenada Y del primer punto
#' @param y2 Coordenada Y del segundo punto
#' @param p Exponente de la distancia de Minkowski (2=Euclidiana, 1=Manhatan)
#'
distancia=function(x1,x2,y1,y2,p=2){((x1-x2)^p + (y1-y2)^p)^(1/p)}

