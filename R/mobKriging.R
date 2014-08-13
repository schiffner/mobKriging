#' @title Recursive Partitioning of Kriging Models
#'
#' @description
#' Interface function for recursive partitioning of Kriging models.
#'
#' @details
#' \code{mobKriging} is an interface to function \code{\link[party]{mob}} with fixed argument \code{model = kmModel} that
#' avoids name clashes between arguments of \code{\link[party]{mob}} and \code{\link[DiceKriging]{km}}.
#' (For example, both, \code{\link[DiceKriging]{km}} and \code{\link[party]{mob}}, have arguments called \code{formula} and \code{control}.)
#' 
#' The \code{formula} argument of \code{\link[DiceKriging]{km}} which specifies the linear trend of the kriging model
#' must only contain the modeling features (\code{x1} to \code{xk}). 
#' It can be passed to \code{\link[DiceKriging]{km}} via the \code{km.args} argument, see the examples.
#'
#' @param obj [\code{formula} | \code{character(1)}]\cr
#'   Either the fitting formula or the name of the target variable.
#'   The formula looks like \code{y ~ x1 + ... + xk | z1 + .... zl}.
#'   \code{z1} to \code{zl} are the partitioning variables (probably categorical, but numerical
#'   ones can be in included, too), while \code{x1} to \code{xk} are the numerical variables
#'   for the kriging models in the terminal nodes of the tree.
#' @param part [\code{character}]\cr
#'   The names of the partitioning variables (\code{z1} to \code{zl}) for the tree structure.
#'   Ignored if \code{obj} is a \code{formula}.
#' @param feat [\code{character}]\cr
#'   The names of the numeric modeling features (\code{x1} to \code{xk}) passed to the kriging models in the terminal nodes.
#'   Ignored if \code{obj} is a \code{formula}.
#' @param data [\code{data.frame}]\cr
#'   Data to fit the model.
#' @param control [\code{list}]\cr
#'   Passed to \code{control} in \code{\link[party]{mob}} (see also \code{\link[party]{mob_control}}).
#' @param km.args [\code{list}]\cr
#'   Further arguments passed to \code{\link[DiceKriging]{km}}.
#'   Note that arguments \code{design} and \code{reponse} must not be included.
#'
#' @return 
#' An object of class \code{\linkS4class{mob}} inheriting from \code{\linkS4class{BinaryTree}}.
#' Every node of the tree is associated with a fitted Kriging model.
#'
#' @examples
# ## example from the documentation of function mob
# if(require("mlbench")) {
#
#     data("BostonHousing", package = "mlbench")
#     ## transform variables appropriately (for a linear regression)
#     BostonHousing$lstat <- log(BostonHousing$lstat)
#     BostonHousing$rm <- BostonHousing$rm^2
#     ## as well as partitioning variables (for fluctuation testing)
#     BostonHousing$chas <- factor(BostonHousing$chas, levels = 0:1, 
#         labels = c("no", "yes"))
#     BostonHousing$rad <- factor(BostonHousing$rad, ordered = TRUE)
#
#     ## partition the linear regression model medv ~ lstat + rm
#     ## with respect to all remaining variables:
#     fmBH <- mobKriging(medv ~ lstat + rm | zn + indus + chas + nox + age + 
#         dis + rad + tax + crim + b + ptratio,
#         control = mob_control(minsplit = 40L), data = BostonHousing[1:200,])
#
# km(response = BostonHousing$medv[1:200], design = BostonHousing[1:200,c("lstat","rm")])
#
# ## print the resulting tree
# fmBH
# ## or better visualize it
# plot(fmBH)
# }
#' if (require(DiceKriging)) {
#'     d <- 2L
#'     x <- seq(0, 1, length = 10L)
#'     design <- expand.grid(x1 = x, x2 = x)
#'     y <- apply(design, 1, branin)
#'     df <- data.frame(y = y, design)
#'
#'     ## no trend (formula = ~ 1)
#'     m <- mobKriging(y ~ x1 + x2 | x1 + x2, data = df, 
#'         control = mob_control(verbose = TRUE))
#'
#'     ## linear trend using all modeling features (formula = ~ .)
#'     m <- mobKriging(y ~ x1 + x2 | x1 + x2, data = df,
#'         km.args = list(formula = ~ ., control = list(trace = FALSE)),
#'         control = mob_control(minsplit = 30L, verbose = FALSE))
#'
#'     ## linear trend using all modeling features (formula = ~ .)
#'     m <- mobKriging(obj = "y", feat = c("x1", "x2"), part = c("x1", "x2"), data = df,
#'         km.args = list(formula = ~ ., control = list(trace = FALSE)),
#'         control = mob_control(verbose = FALSE, parm = 1:4))
#'
#'    coef(m)
#'    # logLik(m)
#'    # residuals(m)
#' }
#'
#' @export

mobKriging = function(obj, part, feat, data, control = mob_control(), km.args = list()) {
	assertDataFrame(data)
	assertList(control)
	assertList(km.args)
	UseMethod("mobKriging")
}



#' @export
mobKriging.formula = function(obj, part, feat, data, control = mob_control(), km.args = list()) {
	model = mob(obj, data = data, model = kmModel, control = control, km.args = km.args)
	return(model)
}



#' @export
mobKriging.character = function(obj, part, feat, data, control = mob_control(), km.args = list()) {
	cns = colnames(data)
	assertChoice(obj, cns)
	assertSubset(part, cns)
	assertSubset(feat, cns)
	f = as.formula(sprintf("%s ~ %s | %s", obj, collapse(feat, "+"), collapse(part, "+")))
	mobKriging(f, data = data, control = control, km.args = km.args)
}



#' @title Predict a Partitioned Kriging Model
#'
#' @description
#' Interface function for predicting a partitioned Kriging model.
#'
#' @details
#' \code{predictMobKriging} calls \code{\link[party]{predict.mob}}, but handles name clashes and
#' generates prettier output.
#'
#' @param object [\code{\linkS4class{mob}}] \cr
#'    A tree with fitted Kriging models in the terminal nodes.
#' @param newdata [\code{data.frame}] \cr
#'    The cases to be predicted.
#' @param type [\code{character(1)}] \cr
#'    Passed to \code{type} in \code{\link[party]{predict.mob}}: A character string specifying whether the response 
#'    or the ID of the associated terminal node should be predicted.
#' @param km.args [\code{list}] \cr
#'    Additional arguments to \code{\link[DiceKriging]{predict.km}}.
#'
#' @return
#' If \code{type = "node"} a vector of associated terminal node IDs.
#'
#' If \code{type = "response"} a \code{list} containing at least the Kriging mean and trend
#' computed at \code{newdata}, depending on \code{km.args}.
#'
#' @seealso \code{\link[party]{predict.mob}}, \code{\link[DiceKriging]{predict.km}}, \code{\link{mobKriging}}.
#
#' @examples
#' ## currently nonsense example
#' if (require(DiceKriging)) {
#'     d <- 2L
#'     x <- seq(0, 1, length = 10L)
#'     design <- expand.grid(x1 = x, x2 = x)
#'     y <- apply(design, 1, branin)
#'     df <- data.frame(y = y, design)
#'
#'     ## linear trend (formula = ~ x1 + x2)
#'     m <- mobKriging(y ~ x1 + x2 | x1 + x2, data = df,
#'         km.args = list(formula = ~ ., control = list(trace = FALSE)))
#'
#'     ## predict the response
#'     pred <- predictMobKriging(m, newdata = design, km.args = list(type = "UK",
#'         se.compute = TRUE, cov.compute = TRUE))
#'
#'     ## calculate fitted values, only trend and mean
#'     pred <- fittedMobKriging(m, km.args = list(type = "UK", se.compute = FALSE,
#'         light.return = TRUE))
#'
#'     ## predict nodes
#'     nodes <- predictMobKriging(m, newdata = design, type = "node")
#' }
#'
#' @rdname predictMobKriging
#'
#' @export

predictMobKriging <- function(object, newdata = NULL, type = c("response", "node"), km.args = list()) {
	assertList(km.args)
	type = match.arg(type)
	p = predict(object, newdata, type = type, km.args = km.args)
	if (type == "node")
		return(p)
	if (type == "response") {
		pred = list()
		pred$trend = sapply(p, function(x) x$trend)
		pred$mean = sapply(p, function(x) x$mean)
		if (!is.null(p[[1]]$sd)) {		## if se.compute = TRUE
			pred$sd = sapply(p, function(x) x$sd)
			pred$lower95 = sapply(p, function(x) x$lower95)
			pred$upper95 = sapply(p, function(x) x$upper95)
		}
		if (!is.null(p[[1]]$cov.1)) {	## if cov.compute = TRUE
			pred$cov = lapply(p, function(x) unlist(x[grep("cov.", names(x))]))
		}
		if (!is.null(p[[1]]$c.1)) {		## if light.return = FALSE
			pred$c = lapply(p, function(x) {ind = grep("c.", names(x), fixed = TRUE); unlist(x[ind[1:(length(ind)/2)]])})
			pred$Tinv.c = lapply(p, function(x) unlist(x[grep("Tinv.c.", names(x))]))
		}
		return(pred)
	}
}



#' @rdname predictMobKriging
#'
#' @export
fittedMobKriging <- function(object, type = c("response", "node"), km.args = list()) {
	predictMobKriging(object, newdata = NULL, type = type, km.args = km.args)
}
