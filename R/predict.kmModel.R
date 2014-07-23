#' Predict a Kriging model.
#'
#'
#' @title Predict a \code{\link{kmModel}}
#'
#'
#' @param object An object of class \code{"kmModel"}.
#' @param newdata A \code{data.frame} of cases to be predicted.
#' @param pred.type Argument \code{type} of \code{\link[DiceKriging]{predict.km}}.
#'    A \code{character} string corresponding to the kriging family, to be chosen between simple kriging (\code{"SK"}), or universal kriging (\code{"UK"}).
#'    (Called \code{pred.type} to avoid confusion with argument \code{type} of \code{\link[party]{predict.mob}}.)
#' @param \dots Further arguments to \code{\link[DiceKriging]{predict.km}}.
#' @param mob Logical. If \code{TRUE} the object returned by \code{\link[DiceKriging]{predict.km}} is a \code{data.frame} instead
#'    of a \code{list}.
#'
#'
#' @return
#' See \code{\link[DiceKriging]{predict.km}}.
#' A \code{list} (or \code{data.frame}) containing the Kriging mean (including the trend) and further vectors and matrices
#' computed at \code{newdata}.
#'
#'
#' @seealso \code{\link[modeltools]{Predict}}, \code{\link[stats]{predict}}, \code{\link[DiceKriging]{predict.km}}.
#'
#'
#' @references 
#' Roustant, O., Ginsbourger, D. and Deville, Y. (2012), DiceKriging, DiceOptim: Two R packages for the analysis of computer
#' experiments by Kriging-based metamodeling and optimization.
#' \emph{Journal of Statistical Software}, \bold{51(1)}, \url{http://www.jstatsoft.org/}.
#'
#'
#' @rdname predict.kmModel
#'
#' @method predict kmModel
#' @export

predict.kmModel <- function(object, newdata = NULL, pred.type, ..., mob = TRUE) {
	pred <- object$predict_response(newdata = newdata, pred.type, ...)
	if (mob) {
		pred <- as.data.frame(pred)
		pred <- lapply(seq_len(nrow(pred)), function(i) pred[i,, drop = FALSE])
	}
	pred
}



#' @rdname predict.kmModel
#'
#'
#' @export

fitted.kmModel <- function(object, pred.type, ..., mob = TRUE) {
	pred <- object$predict_response(newdata = NULL, pred.type, ...)
	if (mob) {
		pred <- as.data.frame(pred)
		pred <- lapply(seq_len(nrow(pred)), function(i) pred[i,, drop = FALSE])
	}
	pred
}
