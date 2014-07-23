#' A class for an unfitted Kriging model.
#'
#'
#' \code{kmModel} is an object of class \code{\link[modeltools]{StatModel-class}} implemented in package \pkg{modeltools} that
#' provides an infra-structure for an unfitted \code{\link[DiceKriging]{km}} model.
#'
#' The argument \code{trend.formula} of function \code{fit} equals the \code{formula} argument of \code{\link[DiceKriging]{km}}:
#' An optional object of class \code{"formula"} specifying the linear trend of the kriging model (see lm).
#' This formula should concern only the input variables, and not the output (response).
#' If there is any, it is automatically dropped. In particular, no response transformation is available yet.
#' The default is \code{~1}, which defines a constant trend.
#' (Called \code{trend.formula} in order to avoid confusion with the \code{fromula} argument of \code{\link[party]{mob}}.)
#'
#'
#' @title \code{kmModel}
#'
#'
#' @seealso \code{\link[modeltools]{StatModel-class}}.
#'
#'
#' @references 
#' Roustant, O., Ginsbourger, D. and Deville, Y. (2012), DiceKriging, DiceOptim: Two R packages for the analysis of computer
#' experiments by Kriging-based metamodeling and optimization.
#' \emph{Journal of Statistical Software}, \bold{51(1)}, \url{http://www.jstatsoft.org/}.
#'
#'
#' @examples
#' if (require(DiceKriging)) {
#'     d <- 2
#'     x <- seq(0,1,length = 4)
#'     design <- expand.grid(x1 = x, x2 = x)
#'     y <- apply(design, 1, branin)
#'     df <- data.frame(y = y, design)
#'
#'     ## data pre-processing
#'     mf <- dpp(kmModel, y ~ ., data = df) 
#'
#'     ## no trend (trend.formula = ~ 1)
#'     m <- fit(kmModel, mf)
#'     ## equivalent
#'     m1 <- km(design = design, response = y)
#'
#'     ## linear trend (trend.formula = ~ x1 + x2)
#'     m <- fit(kmModel, mf, trend.formula = ~ .)
#'     ## equivalent
#'     m1 <- km(formula = ~ ., design = design, response = y)
#'
#'     ## predictions
#'     Predict(m, pred.type = "UK")
#'     ## equivalent
#'     predict(m1, newdata = design, type = "UK")
#' }
#'
#'
#' @rdname kmModel
#' 
#'
#' @import modeltools
#' @import DiceKriging
#' @export

kmModel <- new("StatModel",
	name = "kriging model",
	## data pre-processing
	dpp = function (formula, data = list(), subset = NULL, na.action = NULL, 
			frame = NULL, enclos = sys.frame(sys.nframe()), other = list(), 
    		designMatrix = TRUE, responseMatrix = TRUE, setHook = NULL, ...) {
    		mf <- match.call(expand.dots = FALSE)
    		m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
		    mf <- mf[c(1, m)]
    		mf[[1]] <- as.name("model.frame")
    		mf$na.action <- stats::na.pass
    		MEF <- new("ModelEnvFormula")
    		MEF@formula <- c(modeltools:::ParseFormula(formula, data = data)@formula, 
        		other)
    		MEF@hooks$set <- setHook
    		if (is.null(frame)) 
        		frame <- parent.frame()
    		mf$subset <- try(subset)
    		if (inherits(mf$subset, "try-error")) 
        		mf$subset <- NULL
    		MEF@get <- function(which, data = NULL, frame = parent.frame(), 
        		envir = MEF@env) {
        		if (is.null(data)) 
          	  		RET <- get(which, envir = envir, inherits = FALSE)
        		else {
            		oldData <- get(which, envir = envir, inherits = FALSE)
            		if (!use.subset) 
                		mf$subset <- NULL
            		mf$data <- data
            		mf$formula <- MEF@formula[[which]]
            		RET <- eval(mf, frame, enclos = enclos)
            		modeltools:::checkData(oldData, RET)
        		}
        		return(RET)
    		}
    		MEF@set <- function(which = NULL, data = NULL, frame = parent.frame(), 
        		envir = MEF@env) {
        		if (is.null(which)) 
            		which <- names(MEF@formula)
        		if (any(duplicated(which))) 
            		stop("Some model terms used more than once")
        		for (name in which) {
            		if (length(MEF@formula[[name]]) != 2) 
                		stop("Invalid formula for ", sQuote(name))
            		mf$data <- data
            		mf$formula <- MEF@formula[[name]]
            		if (!use.subset) 
                		mf$subset <- NULL
            		MF <- eval(mf, frame, enclos = enclos)
            		if (exists(name, envir = envir, inherits = FALSE)) 
                		modeltools:::checkData(get(name, envir = envir, inherits = FALSE), 
                  		MF)
            		assign(name, MF, envir = envir)
            		mt <- attr(MF, "terms")
            		if (name == "input" && designMatrix) {
						attr(mt, "intercept") <- 0	## remove intercept
                		assign("designMatrix", model.matrix(mt, data = MF, 
                  		...), envir = envir)
            		}
            		if (name == "response" && responseMatrix) {
							attr(mt, "intercept") <- 0
							assign("responseMatrix", model.matrix(mt, data = MF, 
							...), envir = envir)
            		}
        		}
        		MEapply(MEF, MEF@hooks$set, clone = FALSE)
    		}
    		use.subset <- TRUE
    		MEF@set(which = NULL, data = data, frame = frame)
    		use.subset <- FALSE
    		if (!is.null(na.action)) 
        		MEF <- na.action(MEF)
			MEF
		},
	fit = function (object, weights = NULL, trend.formula = ~ 1, noise.var = NULL, ...) {
    		if (is.null(weights)) {
       			m <- km(formula = trend.formula, design = object@get("designMatrix"), response = object@get("responseMatrix"), noise.var = noise.var, optim.method = "BFGS", ...)
    		} else {
        		m <- km(formula = trend.formula, design = object@get("designMatrix")[weights > 0, , drop = FALSE], response = object@get("responseMatrix")[weights > 0], noise.var = noise.var[weights > 0], optim.method = "BFGS", ...)
    		}
    		if (!m@param.estim)
    			stop("no parameters estimated")
 			z <- list(m = m)
    		class(z) <- c("kmModel")
    		z$weights <- weights
    		z$contrasts <- attr(object@get("designMatrix"), "contrasts")
    		z$terms <- attr(object@get("input"), "terms")
    		z$xlevels <- attr(object@get("designMatrix"), "xlevels")
    		z$predict_response <- function(newdata = NULL, pred.type, ...) {
        		if (!is.null(newdata)) {
            		penv <- new.env()
            		object@set("input", data = newdata, env = penv)
            		dm <- get("designMatrix", envir = penv, inherits = FALSE)
        		} else {
            		dm <- object@get("designMatrix")
        		}
				pred <- predict(object = z$m, newdata = dm, type = pred.type, ...)
				return(pred)
    		}
    		z$addargs <- list(trend.formula = trend.formula, ...)
    		z$ModelEnv <- object
    		z$statmodel <- kmModel
   		 	z
		},
	predict = function (object, newdata = NULL, ...) {
			object$predict_response(newdata = newdata, ...)
		},
	capabilities = new("StatModelCapabilities",
		weights = FALSE,
		subset = FALSE
	)
)
