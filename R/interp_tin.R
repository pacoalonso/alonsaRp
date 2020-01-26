#' Interpolate the z value inside a TIN defined by 3 x,y,z points. If the point is not in the triangle returns NA
#'
#' @param x vector with 3 x values
#' @param y vector with 3 y values 
#' @param z vector with 3 z values 
#' @param x0 Coordinate of the point to interpolate 
#' @param y0 Coordinate of the point to interpolate 
#'
#' @return z for the point (x,y) or NA if the point is outside
#'
#' @import tripack
#'
#'
#' @export
#'
interp_tin <- function(x,y, z, x0,y0) {
   tr=triangles(tri.mesh(x,y))

   z0=rep(NA,length(x0))
   for (p in 1:length(x0)) {
      for (t in 1:nrow(tr)) {
         xx=x[tr[t,1:3]];  yy=y[tr[t,1:3]]; zz=z[tr[t,1:3]]
         if (!is.na(in_tin(xx,yy,zz,x0[p],y0[p]))) z0[p]=in_tin(xx,yy,zz,x0[p],y0[p])
      }
   }
   return(z0)
}

