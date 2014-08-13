#' @title Recursive Partitioning of Kriging Models
#'
#' @description
#' This package provides an interface for model-based recursive partitioning (\code{\link[party]{mob}} from package \pkg{party}) of
#' Kriging models (\code{\link[DiceKriging]{km}} from package \pkg{DiceKriging}).
#' The main functions are \code{\link{mobKriging}} and \code{\link{predictMobKriging}}.
#' These allow to build segmented Kriging models by computing decision trees where each terminal node corresponds to a fitted Kriging model
#' and making predictions based on those models.
#' Partitioning of Kriging models can be particularly useful if the data contain numerical as well as categorical explanatory variables.
#' Since Kriging normally requires numerical explanatory variables the categorical variables cannot be taken into account directly but
#' can be used as partitioning variables in \code{\link{mobKriging}}.
#' Package \pkg{mobKriging} also provides different plots, among others diagnostic plots, for fitted segmented Kriging models. 
#'
# mob Kriging provides \code{\link{kmModel}} (an object of class \code{\linkS4class{StatModel}} implemented in 
# package \pkg{modeltools}) that offers an infra-structure for an unfitted \code{\link[DiceKriging]{km}} model.
#
# Moreover, methods for \code{\linkS4class{kmModel}} objects (that contain an object of class \code{\link[DiceKriging]{km-class}})
# for the generic functions
# \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}}, and
# \code{\link[stats]{predict}} are provided.
#
#
# @note
# Note that when combining kriging with model-based recursive partitioning we have to deal with two different \code{formula} objects.
#
# The \code{formula} argument of \code{\link[party]{mob}} is of the form \code{y ~ x1 + ... + xk | z1 + ... + zl} where the data
# corresponding to the variables before the \code{|} make up the design matrix for the kriging model
# (argument \code{design} of function
# \code{\link[DiceKriging]{km}}) and the variables after the \code{|} are used for partitioning.
#
# The linear trend of the kriging model (\code{formula} argument of \code{\link[DiceKriging]{km}}) can be specified using the argument
# \code{trend.formula} and is passed to \code{\link[DiceKriging]{km}} via the \dots argument of function
# \code{\link[party]{mob}} (see Examples).
# As in \code{\link[DiceKriging]{km}} the default is no trend, i.e., \code{trend.formula = ~ 1}.
#'
#'
#' @references
#' Zeileis, A., Hothorn, T. and Kornik, K. (2008), Model-based recursive partitioning.
#' \emph{Journal of Computational and Graphical Statistics}, \bold{17(2)} 492--514.
#'
#' Roustant, O., Ginsbourger, D. and Deville, Y. (2012), DiceKriging, DiceOptim: Two R packages for the analysis of computer
#' experiments by Kriging-based metamodeling and optimization.
#' \emph{Journal of Statistical Software}, \bold{51(1)}, \url{http://www.jstatsoft.org/}.
#'
#'
#' @name mobKriging-package
#' @docType package
NULL
