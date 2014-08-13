#' @title Predict a \code{\link{kmModel}}
#'
#' @description Predict a fitted Kriging model.
#'
#' @param object [\code{\link{kmModel}}] \cr
#'    The fitted Kriging model.
#' @param newdata [\code{data.frame}] \cr
#'    The cases to be predicted.
#' @param \dots Additional arguments to \code{\link[DiceKriging]{predict.km}}.
#'
#' @return
#' See \code{\link[DiceKriging]{predict.km}}.
#' A \code{list} containing at least the Kriging mean and trend
#' computed at \code{newdata}.
#'
#' @seealso \code{\link[modeltools]{Predict}}, \code{\link[stats]{predict}}, \code{\link[DiceKriging]{predict.km}}, \code{\link{kmModel}}.
#'
#' @references 
#' Roustant, O., Ginsbourger, D. and Deville, Y. (2012), DiceKriging, DiceOptim: Two R packages for the analysis of computer
#' experiments by Kriging-based metamodeling and optimization.
#' \emph{Journal of Statistical Software}, \bold{51(1)}, \url{http://www.jstatsoft.org/}.
#'
#' @examples
#' ## We use the first example in the documentation of function km
#' if (require(DiceKriging)) {
#'     d <- 2L
#'     x <- seq(0, 1, length = 4L)
#'     design <- expand.grid(x1 = x, x2 = x)
#'     y <- apply(design, 1, branin)
#'     df <- data.frame(y = y, design)
#'
#'     ## fitting the model
#'     mf <- dpp(kmModel, y ~ ., data = df)
#'     m <- fit(kmModel, mf, formula = ~ .)
#'
#'     ## fitted values and prediction
#'     fitted(m, type = "UK")
#'     predict(m, type = "UK", se.compute = FALSE, light.return = TRUE)
#' }
# predict(m, km.args = list(type = "UK"))
# predict(m, km.args = list(type = "UK", se.compute = FALSE, light.return = TRUE))
#'
#' @rdname predict.kmModel
#'
#' @method predict kmModel
#' @export

predict.kmModel <- function(object, newdata = NULL, ...) {
	m <- match.call(expand.dots = TRUE)
	km.args <- eval(m$km.args)
	if (is.null(km.args)) {
		pred <- object$predict_response(newdata = newdata, ...)
	} else {
		f <- object$predict_response
		pred <- do.call("f", c(list(newdata = newdata), km.args))
		if (!is.null(pred$c)) {
			pred$c <- t(pred$c)
			pred$Tinv.c <- t(pred$Tinv.c)
		}
		pred <- as.data.frame(pred)
		# pred <- Map(f = function(x) pred[x,, drop = FALSE], seq_len(nrow(pred)))
		pred <- lapply(seq_len(nrow(pred)), function(i) pred[i,, drop = FALSE])	
	}	
	pred
}



#' @rdname predict.kmModel
#'
#' @export

fitted.kmModel <- function(object, ...) {
	predict.kmModel(object, newdata = NULL, ...)
}
