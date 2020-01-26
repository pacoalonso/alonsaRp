#' Distancia de Jeffries-Matusita ver tesis Fulggen
#'
#' @param mat1 Matriz con las reflectividades de la clase 1 con las bandas en columnas y los píxeles en filas
#' @param mat2 Matriz con las reflectividades de la clase 2 con las bandas en columnas y los píxeles en filas
#'
#' @return Distancia
#'
#'
#' @export
#'
jm <- function(mat1,mat2) {
        c1 = cov(mat1); c2=cov(mat2)
        m1 = apply(mat1,2,"mean"); m2=apply(mat2,2,"mean")
        # v1+v2 es la distancia de Bhattacharrya  
        v1 = t(m1-m2) %*% solve(c1+c2)/2 %*% (m1-m2)/8 
        v2 = log( (m1+m2) / (2*sqrt(m1)*sqrt(m2)) )   
        return(2*(1-exp(-(v1+v2))))
}

