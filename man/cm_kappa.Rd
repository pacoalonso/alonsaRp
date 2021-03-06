% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cm_kappa.R
\name{cm_kappa}
\alias{cm_kappa}
\title{Función para calcular CM, OA, K, EC y EO a partir de una matriz de confusión}
\usage{
cm_kappa(CM, p = 0.05, w = NULL)
}
\arguments{
\item{CM}{Confusion matrix}

\item{p}{Significance level}

\item{w}{Weighting matrix}
}
\value{
List of statistics kappa and overall accuracy (with confidence intervals), commission and omission errors.
}
\description{
Función para calcular CM, OA, K, EC y EO a partir de una matriz de confusión
}
