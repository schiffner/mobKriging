#' @title Fit a mobKriging model.
#'
#' @description
#' blaaaaaaaaaaaaaaa
#'
#' @param formula [\code{formula}]\cr
#'   The fitting formula
#' @param data [\code{data.frame}]\cr
#'   Data to fit the model.
#' @param mob.control [\code{list}]\cr
#'   Further args passed to \code{control} in \code{\link[party]{mob}}.
#' @param km.control [\code{list}]\cr
#'   Further args passed to \code{control} in \code{\link[DiceKriging]{km}}.
#' @return [\code{\link[party]{BinaryTree}}].
#' @export
mobKriging = function(formula, data, mob.control, km.control) {
  model = mob(y ~ x1 + x2 | x1 + x2, data = data, model = kmModel,
    control = mob.control)
  return(model)
}
