#' Función para calcular CM, OA, K, EC y EO a partir de una matriz de confusión
#'
#' @param CM Confusion matrix
#' @param p Significance level
#' @param w Weighting matrix
#'
#' @return List of statistics kappa and overall accuracy (with confidence intervals), commission and omission errors.
#'
#' @import stats graphics
#'
#'
#' @export
#'
cm_kappa <- function(CM, p=0.05,w=NULL) {
    if (is.null(w)) w=matrix(rep(1,ncol(CM)*nrow(CM)),ncol=ncol(CM))
    CM=CM*w
    sf=apply(CM,1,"sum")
    sc=apply(CM,2,"sum")
    oa=sum(diag(CM))/sum(CM)
    ea=sum(apply(CM,1,"sum")*apply(CM,2,"sum"))/(sum(CM)^2)
    z3=sum((diag(CM)/sum(CM))*(sf+sc)/sum(CM))
    z4=0;for (i in 1:ncol(CM)) for (j in 1:ncol(CM)) z4=z4 + CM[i,j]* ( (sf[j]+sc[i])^2 )
    z4=z4/(sum(CM)^3)
    soa = sqrt(oa*(1-oa)/sum(CM))
    i1oa=oa - (qnorm(1-p/2)*soa+1/(2*sum(CM)))
    i2oa=oa + (qnorm(1-p/2)*soa+1/(2*sum(CM)))
    k=(oa-ea)/(1-ea)
    sk = sqrt((oa*(1-oa)/((1-ea)^2) + 2*(1-oa)*(2*oa*ea-z3)/((1-ea)^3) + (1-oa)^2 * (z4-4*ea^2)/((1-ea)^4)) / sum(CM))
    i1k = k - (qnorm(1-p/2)*sk + 1/(2*sum(CM)))
    i2k = k + (qnorm(1-p/2)*sk + 1/(2*sum(CM)))
    oe=1-diag(CM)/apply(CM,2,"sum")
    ce=1-diag(CM)/apply(CM,1,"sum")
    pa=1-oe
    ua=1-ce
    return(list(CM=CM,oa=c(i1oa,oa,i2oa),soa=soa,k=as.numeric(c(i1k,k,i2k)),sk=as.numeric(sk),oe=oe,ce=ce,pa=pa,ua=ua))
}

