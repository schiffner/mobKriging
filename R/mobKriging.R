#' @title Fit a mobKriging model
#'
#' @description
#' blaaaaaaaaaaaaaaa
#'
#' @param obj [\code{formula} | character(1)]\cr
#'   Either the fitting formula or the name of the target variable.
#'   The formula looks like \code{y ~ x1 + ... + xk | z1 + .... zl}.
#'   \code{z1} to \code{zl} are the splitting variables (probably categorical, but numerical
#'   ones could be in included, too), while \code{x1} to \code{xk} are the numerical variables
#'   for the kriging models in the terminal nodes of the tree.
#' @param part [\code{character}]\cr
#'   The partitioning variables for the tree structure.
#'   Ignored if you pass a formula.
#' @param feat [\code{data.frame}]\cr
#'   The modeling features passed to the kriging models in the terminal nodes.
#'   Data to fit the model.
#'   Ignored if you pass a formula.
#' @param data [\code{data.frame}]\cr
#'   Data to fit the model.
#' @param mob.control [\code{list}]\cr
#'   Further args passed to \code{control} in \code{\link[party]{mob}}.
#' @param km.args [\code{list}]\cr
#'   Further args passed to \code{\link[DiceKriging]{km}}.
#'   Note that args \code{design} and \code{reponse} must not be included.
#' @return [\code{\linkS4class{BinaryTree}}].
#'
#' @examples
#' ## currently nonsense example
#' if (require(DiceKriging)) {
#'     d <- 2L
#'     x <- seq(0, 1, length = 10L)
#'     design <- expand.grid(x1 = x, x2 = x)
#'     y <- apply(design, 1, branin)
#'     df <- data.frame(y = y, design)
#'
#'     ## no trend (trend.formula = ~ 1)
#'     m <- mobKriging(y ~ x1 + x2 | x1 + x2, data = df, 
#'         mob.control = mob_control(objfun = deviance, minsplit = 20L, verbose = TRUE))
#'
#'     ## linear trend (trend.formula = ~ x1 + x2)
#'     m <- mobKriging(y ~ x1 + x2 | x1 + x2, data = df, km.args = list(formula = ~ .),
#'         mob.control = mob_control(objfun = deviance, minsplit = 20L, verbose = TRUE))
#'
#'     ## plot the resulting tree
#'     ## (note that the trend.formula is currently not taken into account)
#'     # plot(m)
#'
#'     ## predict y
#'     pred <- predict(m, newdata = design, pred.type = "UK")
#'
#'     ## predict nodes
#'     nodes <- predict(m, newdata = design, type = "node")
#' }
#'
#' @export

mobKriging = function(obj, part, feat, data, mob.control = mob_control(), km.args = list()) {
	assertDataFrame(data)
	assertList(mob.control)
	assertList(km.args)
	UseMethod("mobKriging")
}



#' @export
mobKriging.formula = function(obj, part, feat, data, mob.control = mob_control(), km.args = list()) {
	model = mob(obj, data = data, model = kmModel, control = mob.control, km.args = km.args)
	return(model)
}



#' @export
mobKriging.character = function(obj, part, feat, data, mob.control = mob_control(), km.args = list()) {
	cns = colnames(data)
	assertChoice(obj, cns)
	assertSubset(part, cns)
	assertSubset(feat, cns)
	f = as.formula(sprintf("%s ~ %s | %s", obj, collapse(feat, "+"), collapse(part, "+")))
	mobKriging(f, data, mob.control, km.args)
}
