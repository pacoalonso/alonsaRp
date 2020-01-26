#' Executes an Operating System Command
#'
#' @param command Command to be executed
#' @param intern If TRUE the outpus of the commasnd is passed into R.
#'
#' @return If intern=TRUE returns the oputput of the OS command, else returns the command.
#'
#' @export
#'
ejecuta <- function(command,intern=FALSE){
	if (Sys.info()[1]=="Linux"){
          kk=system(command, intern=intern, wait=TRUE)
    } else if (Sys.info()[1]=="Windows"){
          kk=shell(command, intern=intern, wait=TRUE)
    } else {
          cat("Sistema operativo desconocido\n")
    }
    if (intern) return(kk) else return(command)     
} 

