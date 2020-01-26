#' Divergencia entre clases (Singh, 1984) ver Tso Mather (2009)
#'
#' @param mat1 Matriz con las reflectividades de la clase 1 con las bandas en columnas y los píxeles en filas
#' @param mat2 Matriz con las reflectividades de la clase 2 con las bandas en columnas y los píxeles en filas
#'
#' @return Divergencia
#'
#'
#' @export
#'
divergencia <- function(mat1,mat2) {
	c1=cov(mat1); c2=cov(mat2)
        m1=apply(mat1,2,"mean"); m2=apply(mat2,2,"mean")
        d1=sum(diag((c1-c2) %*% (solve(c2)-solve(c1))))/2
        d2= sum(diag((solve(c1)-solve(c2)) %*% (m1-m2) %*% t(m1-m2)))/2
        return(d1+d2)
}

