#' Interpolate the z value inside a triangle defined by 3 x,y,z points. If the point is not in the triangle returns NA
#'
#' @param x vector with 3 x values
#' @param y vector with 3 y values 
#' @param z vector with 3 z values 
#' @param x0 Coordinate of the point to interpolate 
#' @param y0 Coordinate of the point to interpolate 
#'
#' @import sp
#'
#' @return z0 for the point (x0,y0) or NA if the point is outside
#'
in_tin <- function(x, y, z, x0, y0) {
     if (point.in.polygon(x0, y0, x, y)>0) {
        xx=cbind(c(1,1,1),x,y)
        b=solve(t(xx) %*% xx) %*% (t(xx) %*% z)
        z=crossprod(b,c(1,x0,y0))
     } else {
        z=NA
     }
     return(z)
}

