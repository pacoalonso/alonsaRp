#' Cálculo de escala
#'
#' @param scale Scale's denominator
#' @param size Paper size (mm or ISO)
#' @param xlim X limit
#' @param ylim Y limit
#'
realScale <- function(scale,size="A4",xlim,ylim) {
	if (length(size)==1){
          if (size=="A0") size=c(841,1189)
          if (size=="A1") size=c(594,841)
          if (size=="A2") size=c(420,594)
          if (size=="A3") size=c(297,420)
          if (size=="A4") size=c(210,297)
          if (size=="A5") size=c(148,210)
          if (size=="A6") size=c(105,148)
          if (size=="A7") size=c(74,105)
          if (size=="A8") size=c(52,74)
    }
}

