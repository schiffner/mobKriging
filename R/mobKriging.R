#' @title Fit a mobKriging model.
#'
#' @description
#' blaaaaaaaaaaaaaaa
#'
#' @param obj [\code{formula} | character(1)]\cr
#'   Either the fitting formula or the name of the target variable.
#'   The formula looks like \code{y ~ s1 + ... + sn | x1 + .... xn}.
#'   s1 to sn are the splitting variables (probably categorical, but numerical
#'   ones could be in included, too), while x1 to xn are the numerical variables
#'   the for the kriging models in the terminal nodes of the tree.
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
#' @param km.control [\code{list}]\cr
#'   Further args passed to \code{control} in \code{\link[DiceKriging]{km}}.
#' @return [\code{\linkS4class{BinaryTree}}].
#' @export

mobKriging= function(obj, part, feat, data, mob.control = list(), km.control = list()) {
  assertDataFrame(data)
  assertList(mob.control)
  assertList(km.control)

  UseMethod("mobKriging")
}

#' @export
mobKriging.formula = function(obj, part, feat, data, mob.control = list(), km.control= list()) {
  model = mob(obj, data = data, model = kmModel,
    control = mob.control)
  return(model)
}

#' @export
mobKriging.character = function(obj, part, feat, data, mob.control = list(), km.control = list()) {
  cns = colnames(data)
  assertChoice(obj, cns)
  assertSubset(part, cns)
  assertSubset(feat, cns)
  f = as.formula(sprintf("%s ~ %s | %s", obj, collapse(feat, "+"), collapse(part, "+")))
  mobKriging(f, data, mob.control, km.control)
}

