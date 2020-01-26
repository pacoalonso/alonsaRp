#' ET0  Thornthwaite mensuales
#'
#' @param temp Vector con 12 temperaturas medias mensuales 
#' @param hsm Vector con 12 horas de sol medias mensuales
#'
#' @return Vector con 12 valores de ET0 mensuales
#'
#'
#' @export
#'
et_Thornthwaite <- function(temp,hsm) {
    i = c()
    for (t in 1:12) i[t] = (temp[t]/5)^1.514          # Índice de calor mensual
    ii = sum(i)                                       # Índice de calor anual
    a = 675*(10^-9)*ii^3 - 771*(10^-7)*ii^2 +         # alfa
        1.972*(10^-5)*ii + 0.49239 
    ETP = 1.6*(10*temp/ii)^a                          # Estimación para 30 días
    ETP = ETP*c(31,28,31,30,31,30,31,31,30,31,30,31)  # Corrección número de dias

    ETP = ETP * hsm/12                                # Corrección horas de sol
    return(ETP)
}

