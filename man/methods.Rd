% Generated by roxygen2 (4.0.1): do not edit by hand
\name{coef.kmModel}
\alias{coef.kmModel}
\alias{deviance.kmModel}
\alias{estfun.kmModel}
\alias{logLik.kmModel}
\alias{model.matrix.kmModel}
\alias{plot.kmModel}
\alias{print.kmModel}
\alias{residuals.kmModel}
\alias{reweight.kmModel}
\title{Methods for \code{\link{kmModel}} Objects}
\usage{
\method{coef}{kmModel}(object, ...)

\method{deviance}{kmModel}(object, ...)

\method{estfun}{kmModel}(x, ...)

\method{logLik}{kmModel}(object, ...)

\method{model.matrix}{kmModel}(object, ...)

\method{plot}{kmModel}(x, ...)

\method{print}{kmModel}(x, ...)

\method{residuals}{kmModel}(object, ...)

\method{reweight}{kmModel}(object, weights, ...)
}
\arguments{
\item{object,x}{[\code{\linkS4class{kmModel}}] \cr
The fitted Kriging model.}

\item{weights}{[\code{numeric}] \cr
A vector of observation weights.}

\item{\dots}{Further arguments.}
}
\description{
Methods for \code{\link{kmModel}} objects.
}
\section{Methods (by generic)}{
\itemize{
\item \code{coef}: Coefficient values of a fitted \code{\linkS4class{km}} model, see \code{\link[DiceKriging]{coef,km-method}}.

\item \code{deviance}: The value of the deviance (log-likelihood times -2) extracted from \code{object}.

\item \code{estfun}: The empirical estimating (or score) function, i.e., the derivatives of the log-likelihood with respect
       to the parameters, evaluated at the training data.

\item \code{logLik}: Value of the concentrated log-likelihood at its optimum.

\item \code{model.matrix}: See \code{\link[stats]{model.matrix}}.

\item \code{plot}: Diagnostic plots, see \code{\link[DiceKriging]{plot,km-method}}.

\item \code{print}: The main features of the fitted Kriging model. See \code{\link[DiceKriging]{show,km-method}}.

\item \code{residuals}: The de-correlated residuals. Extractor function for slot \code{z} of \code{\linkS4class{km}}.

\item \code{reweight}: The re-weighted fitted \code{\linkS4class{kmModel}} object.
}}
\seealso{
\code{\link[DiceKriging]{coef,km-method}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, \code{\link[stats]{logLik}},
\code{\link[stats]{model.matrix}}, \code{\link[DiceKriging]{plot,km-method}}, \code{\link[DiceKriging]{show,km-method}},
\code{\link[stats]{residuals}}, \code{\link[party]{reweight}}, \code{\link[DiceKriging]{km}}.
}

