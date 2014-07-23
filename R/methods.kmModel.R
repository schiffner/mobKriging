#' Methods for \code{\link{kmModel}} objects.
#'
#'
#' @title Methods for \code{\link{kmModel}} Objects
#'
#'
#' @param object An object of class \code{"kmModel"}.
#' @param weights A vector of observation weights.
#' @param \dots Further arguments.
#'
#'
#' @return 
#' \code{reweight}: The re-weighted fitted \code{"kmModel"} object. 
#\cr
#'
#'
#' @seealso \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}},
#' \code{\link[DiceKriging]{km}}.
#'
#'
#' @rdname reweight.kmModel
#'
#' @importFrom party reweight
#' @export

reweight.kmModel <- function (object, weights, ...) {
	fit <- kmModel@fit
	try(do.call("fit", c(list(object = object$ModelEnv, weights = weights), object$addargs)))
}



# @param object An object of class \code{"kmModel"}.
# @param \dots Further arguments.
#'
#'
#' @return 
#' \code{deviance}: The value of the deviance extracted from \code{object}.
#'
#'
# @seealso \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}},
# \code{\link[DiceKriging]{km}}.
#'
#'
#' @rdname reweight.kmModel
#'
#' @importFrom stats deviance
#' @export

deviance.kmModel <- function (object, ...) {
	return(-object$m@logLik)
	## final value of the concentrated log-likelihood, factor -1 since objective function is minimized
}



#' @noRd
#'
#' @export
## needed if error in fitting

'deviance.try-error' <- function(object, ...)
	Inf



#' @param x An object of class \code{"kmModel"}.
# @param \dots Further arguments.
#'
#'
#' @return 
#' \code{estfun}: The empirical estimating (or score) function, i.e., the derivatives of the log-likelihood with respect
#'   to the parameters, evaluated at the training data.
#'
#'
# @seealso \code{\link[party]{reweight}}, \code{\link[stats]{deviance}}, \code{\link[sandwich]{estfun}},
# \code{\link[DiceKriging]{km}}.
#'
#'
#' @rdname reweight.kmModel
#'
#' @importFrom sandwich estfun
#' @export

## model@T: C = t(T) %*% T
## model@z: z = inv(t(T))*(y - F*beta), de-correlated residuals
## model@M: M = inv(t(t))*F, de-correlated experimental matrix
## xx = backsolve(model@T, model@z): solves Txx = z -> xx = inv(T)z = inv(T)inv(t(T))(y-F*beta) = inv(C)(y-F*beta)

estfun.kmModel <- function(x, ...) {
	model <- x$m
	
	if (identical(model@method, "PMLE")) {
		stop("penlized MLE currently not implemented")
	}

	## trend parameters
	if (model@known.param %in% c("None", "CovAndVar")) { ## possible values are "None", "All", "CovAndVar" and "Trend"
		trend.derivative <- model@M * model@z
		colnames(trend.derivative) <- colnames(model@F)
	} else
		trend.derivative <- NULL

	#### covariance parameters
	if (model@known.param %in% c("None", "Trend")) {
		if (identical(model@case, "LLconcentration_beta_sigma2")) {		## noise-free obs, no nugget effect
			## sigma^2
			sd2.derivative <- matrix(- 0.5/(model@covariance@sd2) * (1 - model@z^2))
			colnames(sd2.derivative) <- "sd2"
			## theta, p
			nparam <- model@covariance@param.n			## number of covariance parameters
			T <- model@T/sqrt(model@covariance@sd2)		## Cholesky dec. of R
			Tinv <- backsolve(T, diag(nrow(T)))			## inverse of T
			sd2 <- model@covariance@sd2
			model@covariance@sd2 <- 1
			aux <- covMatrix(model@covariance, model@X)
			R <- aux$C									## correlation matrix since model@covariance@sd2 = 1
			rangeShape.derivative <- matrix(0, model@n, nparam)
			colnames(rangeShape.derivative) <- c(paste(model@covariance@range.names, seq_along(model@covariance@range.val), sep = ""), paste(model@covariance@shape.names, seq_along(model@covariance@shape.val), sep = ""))
			for (k in 1:nparam) {
				gradR.k <- covMatrixDerivative(model@covariance, 
					X = model@X, C0 = R, k = k)
				eigenGradR.k <- eigen(gradR.k, symmetric = TRUE)
					## gradR.k = eigenGradR.k$vectors %*% diag(eigenGradR.k$values) %*% t(eigenGradR.k$vectors)
				tUinvT <- t(eigenGradR.k$vectors) %*% Tinv
				rangeShape.derivative[,k] <- 0.5 * eigenGradR.k$values * (tUinvT %*% model@z)^2 - 0.5 * colSums(eigenGradR.k$values * tUinvT^2)
			}
	    	cov.derivative <- cbind(sd2.derivative, rangeShape.derivative)
	    }
    	else if (identical(model@case, "LLconcentration_beta")) {		## noise, noise variances known
			aux <- covMatrix(model@covariance, model@X, noise.var = model@noise.var)
			## C = model@covariance@sd2 * R + diag(model@noise.var)
			## <-> R = (C - diag(model@noise.var))/model@covariance@sd2
			## aux$C = C
			## aux$vn = model@noise.var
			C <- aux$C											## C: covariance matrix including noise 
			R <- (aux$C - diag(aux$vn))/model@covariance@sd2	## R: correlation matrix without noise
			## checks: all(diag(R) == 1); all.equal(t(model@T) %*% model@T, C)
			## sigma^2
			T <- chol(R)
			xx <- backsolve(model@T, model@z)
			modelTinv <- backsolve(model@T, diag(nrow(model@T)))
			sd2.derivative <- 0.5 * matrix(-(t(T) %*% xx)^2 - 0.5 * colSums((modelTinv %*% T)^2))
			colnames(sd2.derivative) <- "sd2"			
	    	## theta, p
			nparam <- model@covariance@param.n					## number of covariance parameters
			rangeShape.derivative <- matrix(0, model@n, nparam)
			colnames(rangeShape.derivative) <- c(paste(model@covariance@range.names, seq_along(model@covariance@range.val), sep = ""), paste(model@covariance@shape.names, seq_along(model@covariance@shape.val), sep = ""))
	        for (k in 1:nparam) {
            	gradC.k <- covMatrixDerivative(model@covariance, 
                	X = model@X, C0 = aux$C - diag(aux$vn), k = k)
				eigenGradC.k <- eigen(gradC.k, symmetric = TRUE)
				tUinvT <- t(eigenGradC.k$vectors) %*% modelTinv
				rangeShape.derivative[,k] <- 0.5 * eigenGradC.k$values * (t(eigenGradC.k$vectors) %*% xx)^2 - 0.5 * colSums(eigenGradC.k$values * tUinvT^2)
			}
			cov.derivative <- cbind(sd2.derivative, rangeShape.derivative)
    	}
    	else if (identical(model@case, "LLconcentration_beta_v_alpha")) {	## nugget effect
			## v
			v <- model@covariance@sd2 + model@covariance@nugget
			alpha <- model@covariance@sd2/v
			v.derivative <- matrix(-1/(2*v) * (1 - model@z^2))
			colnames(v.derivative) <- "v"
			## alpha
			xx <- backsolve(model@T, model@z)
			## C = model@covariance@sd2 * R + diag(model@covariance@nugget)
			model@covariance@sd2 <- 1
			## -> aux$C = R + diag(model@covariance@nugget)
			model@covariance@nugget <- 0
			## -> aux$C = R
			aux <- covMatrix(model@covariance, model@X)
			R <- aux$C							## correlation matix without nugget effect
			T <- chol(R)						## Cholesky decomposition of R
			modelTinv <- backsolve(model@T, diag(nrow(model@T)))
			TmodelTinv <- T %*% modelTinv
			alpha.derivative <- -0.5 * matrix(- v * (T %*% xx)^2 + v * xx^2 + v * colSums(TmodelTinv^2) - v * colSums(modelTinv^2))
			colnames(alpha.derivative) <- "alpha"
			## theta, p
			nparam <- model@covariance@param.n				## number of covariance parameters
			rangeShape.derivative <- matrix(0, model@n, nparam)
			colnames(rangeShape.derivative) <- c(paste(model@covariance@range.names, seq_along(model@covariance@range.val), sep = ""), paste(model@covariance@shape.names, seq_along(model@covariance@shape.val), sep = ""))
			for (k in 1:nparam) {
				gradC.k <- covMatrixDerivative(model@covariance, 
					X = model@X, C0 = R, k = k)
				gradC.k <- alpha * gradC.k
				eigenGradC.k <- eigen(gradC.k, symmetric = TRUE)
				rangeShape.derivative[,k] <- 0.5 * v * eigenGradC.k$values * (t(eigenGradC.k$vectors) %*% xx)^2 - 0.5 * v * colSums(eigenGradC.k$values * (t(eigenGradC.k$vectors) %*% modelTinv)^2)
			} 
			cov.derivative <- cbind(v.derivative, rangeShape.derivative, alpha.derivative)
    	}
    }
    deriv <- cbind(trend.derivative, cov.derivative)
print(colSums(deriv))
    derivative <- matrix(0, length(x$weights), ncol(deriv))
    colnames(derivative) <- colnames(deriv)
    derivative[x$weights == 1,] <- deriv
print(head(derivative))
	return(derivative)
}



#' @noRd
#'
#' @export

print.kmModel <- function(x, ...) {
	show(x$m)
}



#' @noRd
#'
#' @export

model.matrix.kmModel <- function (object, ...) {
	object$ModelEnv@get("designMatrix")
}
