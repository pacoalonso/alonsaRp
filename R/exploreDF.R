#' Devuelve las propiedades fundamentales de un data.frame y sus columnas. Devuelve también los tamanos del elementos mas grande de las columnas
#'
#' @param df data.frame
#'
#'
#' @return Data.frame summary
#'
#' @export
#'
exploreDF <- function(df) {
   message(nrow(df), "filas y", ncol(df), "columnas\n")
   message("Variable          Tipo       Longitud    Ejemplo\n")
   w <- which.max(apply(df,1,function(x) return(sum(as.numeric(!is.na(x))))))  
   for (v in names(df)) {
       if (class(df[,v]) == "character") {
          clase = "character"
          lon = max(nchar(unique(df[,v]))) 
          format = "%-16s: %-10s %d chars  %s\n"
       } else if (class(df[,v]) == "list") {
          cat(sprintf("%-16s: list\n", v))
          next
       } else {
          if (all(df[, v]%%1==0, na.rm=TRUE)) {
            clase = "int"; lon = "4 bytes"
          } else {
            clase = "float";lon = "8 bytes"
          }          
          format = "%-16s: %-10s %-10s %d\n"
       }
       cat(sprintf(format, v, clase, lon, df[w,v]))       
   }
}

