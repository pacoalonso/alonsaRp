#' Lista los objetos en memoria junto a su tamaño por orden de tamaño
#'
#' @param format "b", "Kb", "Mb", "Gb"
#'
#' return Una lista de los objetos en memoria junto a su tamaño por orden de tamaño
#'
#' @import utils
#'
memoriaR <- function(format="b") {
    objs=ls(globalenv())
    tam=c()

    if (format=="b") conv=1
    if (format=="Kb") conv=1000
    if (format=="Mb") conv=1000000
    if (format=="Gb") conv=1000000000

    for (i in 1:length(objs)) {
       tam[i]=object.size(get(objs[i]))
    }

    o=order(tam)
    return(data.frame(objs=objs[o], tam=tam[o]/conv))
}

