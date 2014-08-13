#' An object of class \code{\linkS4class{StatModel}} that provides infra-structure for an unfitted Kriging model.
#'
#' @title \code{kmModel}
#'
#' @return Slot \code{fit} returns an object of class \code{kmModel}.
#'
#' @seealso \code{\linkS4class{StatModel}}, \code{\link[DiceKriging]{km}}, \code{\link[modeltools]{Predict}}.
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
#'     ## Fitting the model using kmModel:
#'     # data pre-processing
#'     mf <- dpp(kmModel, y ~ ., data = df)
#'     # no trend (formula = ~ 1)
#'     m1 <- fit(kmModel, mf)
#'     # linear trend (formula = ~ x1 + x2)
#'     m1 <- fit(kmModel, mf, formula = ~ .)
#'     # predictions on the training data
#'     # recommended: improved version of predict for models fitted with objects
#'     # of class StatModel
#'     Predict(m1, type = "UK")
#'     # also possible
#'     predict(m1, type = "UK")
#'
#'     ## This is equivalent to:
#'     # no trend (formula = ~ 1)
#'     m2 <- km(design = design, response = y)
#'     # linear trend (formula = ~ x1 + x2)
#'     m2 <- km(formula = ~ ., design = design, response = y)
#'     # predictions on the training data
#'     predict(m2, newdata = design, type = "UK")
#'
#'     ## extract information
#'     coef(m1)
#'     residuals(m1)
#'     logLik(m1)
#'
#'     ## diagnostic plots
#'     plot(m1)
#' }
#'
#' @rdname kmModel
#' @aliases kmModel-class
#'
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
	fit = function (object, weights = NULL, noise.var = NULL, km.args = NULL, ...) {
			if (is.null(km.args)) {
	    		if (is.null(weights)) {
    	   			m <- km(design = object@get("designMatrix"), response = object@get("responseMatrix"), noise.var = noise.var, ...)
    			} else {
        			m <- km(design = object@get("designMatrix")[weights > 0, , drop = FALSE], response = object@get("responseMatrix")[weights > 0], noise.var = noise.var[weights > 0], ...)
    			}
			} else {
	    		if (is.null(weights)) {
    	   			m <- do.call("km", c(list(design = object@get("designMatrix"), response = object@get("responseMatrix")), km.args))
    			} else {
    				if (!is.null(km.args$noise.var))
    					km.args$noise.var <- km.args$noise.var[weights > 0]
    	   			m <- do.call("km", c(list(design = object@get("designMatrix")[weights > 0, , drop = FALSE], response = object@get("responseMatrix")[weights > 0]), km.args))
    			}
			}
    		if (!m@param.estim)
    			stop("no parameters estimated")
 			z <- list(m = m)
    		class(z) <- c("kmModel")
    		z$weights <- weights
    		z$contrasts <- attr(object@get("designMatrix"), "contrasts")
    		z$terms <- attr(object@get("input"), "terms")
    		z$xlevels <- attr(object@get("designMatrix"), "xlevels")
    		z$predict_response <- function(newdata = NULL, ...) {
        		if (!is.null(newdata)) {
            		penv <- new.env()
            		object@set("input", data = newdata, env = penv)
            		dm <- get("designMatrix", envir = penv, inherits = FALSE)
        		} else {
            		dm <- object@get("designMatrix")
        		}
				pred <- predict(object = z$m, newdata = dm, ...)
				return(pred)
    		}
    		z$addargs <- list(noise.var = noise.var, km.args = km.args, ...)
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
