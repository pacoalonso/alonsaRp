#' Función para validación cruzada uno a uno de Regression(lm)-krigging
#' @param variable Variable dependiente
#' @param predictores Vector con los predictores que vamos a utilizar 
#' @param data Data.frame con los datos
#' 
#' @importFrom gstat krige
#' @importFrom automap autofitVariogram
#'
#' @export 
#'
fun.CVRK <- function(variable,predictores,data){

  formcvk<-"residuales ~ 1"
  cvtablaval <- data
  cvtablaval$predlm<-NA
  cvtablaval$predko<-NA

  for (i in 1:length(data[[variable]])) {

    #datos
    cvlmobs <- data[i,]
    cvlmresto <- data[-i,]

    #lm
    cvlmint=lm(paste(variable,"~",paste(predictores,collapse="+")), data=cvlmresto)
    cvpredictlmint=predict(cvlmint,cvlmobs)

    #espacial residuos
    cvlmresto$residuales <- cvlmint$residuals
    cv.variogram=autofitVariogram(as.formula(formcvk),cvlmresto)
    cvlmobs2 <- krige(as.formula(formcvk), cvlmresto, cvlmobs, model=cv.variogram$var_model)
    cvtablaval$predlm[i]=cvpredictlmint[[1]]
    cvtablaval$predko[i]=cvlmobs2[["var1.pred"]]
    rm(cvpredictlmint,cvlmobs2,cvlmobs,cvlmresto,cvlmint)
  }	
  cvtablaval<-subset(cvtablaval,is.na(cvtablaval$predko) == F) #limpio nulos	
  return(data.frame(observed=cvtablaval[[variable]],
                    cvpred=cvtablaval$predlm + cvtablaval$predko,
                    residual=cvtablaval[[variable]] - (cvtablaval$predlm + cvtablaval$predko))
        )
}

#' Función para validación cruzada uno a uno de regresión lineal
#'
#' @param variable Variable dependiente
#' @param predictores Vector con los predictores que vamos a utilizar 
#' @param data Data.frame con los datos
#' @export 
#'
fun.CVLM <- function(variable,predictores,data){
  pred=c()
  for (i in 1:length(data[[variable]])) {
    mod <- lm(paste(variable,"~",paste(predictores,collapse="+")), data=data[-i,])
    pred[i] <- predict(mod,data[i,])
   }
  return(data.frame(observed=data[[variable]],
                    cvpred=pred,
                    residual=data[[variable]] - pred)
        )
}


