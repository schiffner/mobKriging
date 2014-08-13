#' @title Panel Generating Functions for Partitioned Kriging Models
#'
#' @description
#' Some panel generating functions of class \code{grapcon_generator} (package \pkg{vcd}) for terminal nodes associated with Kriging models.
#' For more details see \code{\link[party]{node_bivplot}}.
#'
#' @details
#' \code{node_diagnosticKriging} draws diagnostic plots (see also \code{\link[DiceKriging]{plot.km}}).
#' There are three different plots available, based on the results of \code{\link[DiceKriging]{leaveOneOut.km}},
#' and which ones are drawn can be controlled via the \code{which} argument:
#' \describe{
#'    \item{\code{"loo"}}{plot of fitted values against response values,}
#'    \item{\code{"res"}}{plot of standardized residuals,}
#'    \item{\code{"qq"}}{qqplot of standardized residuals.}
#' }
#' The default is to draw all three diagnostic plots.
#'
# \code{node_bivplotKriging}
# \code{node_trivplotKriging}
#'
#' @usage
#' node_diagnosticKriging(mobobj, id = TRUE, pop = TRUE,
#'	which = c("loo", "res", "qq"),
#'	pointcol = "black", pointcex = 0.5, linecol = "black", linetype = 1,
#'	ylines = 2, margins = rep(1.5, 4),
#'	kriging.type, trend.reestim = FALSE,
#'	...)
#'
#'
#' @param mobobj [\code{mob}] \cr
#'    An object of class mob.
#' @param id [\code{logical(1)}] \cr
#'    Should node IDs be plotted?
#' @param pop [\code{logical(1)}] \cr
#'    Should the panel viewports be popped?
#' @param which [\code{character}] \cr
#'    Which diagnostic plots should be drawn?
#'    The default is to draw all three available plots, see details.
#' @param pointcol [\code{numeric} | \code{character}] \cr
#'    Color for points.
#' @param pointcex [\code{numeric}] \cr
#'    Character extension of points in scatter plots.
#' @param linecol [\code{numeric} | \code{character}] \cr
#'    Color for lines.
#' @param linetype [\code{numeric} | \code{character}] \cr
#'    Type for lines.
#' @param ylines [\code{numeric(1)}] \cr
#'    Number of lines for spaces in y-direction.
#' @param margins [\code{numeric(4)}] \cr
#'    Margins of the viewports.
#' @param kriging.type [\code{character(1)}] \cr
#'    An optional character string corresponding to the kriging family, to be chosen between simple kriging (\code{"SK"}) or universal kriging (\code{"UK"}).
#' @param trend.reestim [\code{logical(1)}] \cr
#'    Should the trend be reestimated when removing an observation? Defaults to \code{FALSE}.
#' @param \dots Additional arguments passed to callies.
#'
#' @examples
#' if (require(DiceKriging)) {
#'     d <- 2L
#'     x <- seq(0, 1, length = 10L)
#'     design <- expand.grid(x1 = x, x2 = x)
#'     y <- apply(design, 1, branin)
#'     df <- data.frame(y = y, design)
#'
#'     ## no trend (formula = ~ 1)
#'     m <- mobKriging(y ~ x1 + x2 | x1 + x2, data = df,
#'         km.args = list(control = list(trace = FALSE)),
#'         control = mob_control(objfun = deviance, minsplit = 20L, verbose = FALSE))
#'
#'     ## default
#'     plot(m, terminal_panel = node_diagnosticKriging(m,
#'         linecol = "red", linetype = "dashed", kriging.type = "UK"))
#'     ## only leave-one-out plots
#'     plot(m, terminal_panel = node_diagnosticKriging(m, which = c("loo"),
#'         linecol = "red", linetype = "dashed", kriging.type = "UK"))
#'     ## only qq-plots
#'     plot(m, terminal_panel = node_diagnosticKriging(m, which = "qq",
#'         linecol = "grey", linetype = "dashed", kriging.type = "UK"))
#'     ## leave-one-out and residual plots
#'     plot(m, terminal_panel = node_diagnosticKriging(m, which = c("loo", "res"),
#'         linecol = "blue", linetype = "dashed", kriging.type = "UK"))
#' }
#'
#'
#' @rdname node_diagnosticKriging
#'
#' @export


# plot(m, terminal_panel = node_diagnosticKriging(m, which = "loo", linetype = "dashed", linecol = "red"), kriging.type = "UK")
# plot(m, terminal_panel = node_diagnosticKriging(m, which = "res"), kriging.type = "UK")
# plot(m, terminal_panel = node_diagnosticKriging(m, which = "qq", linetype = "dashed", linecol = "blue", kriging.type = "UK"))
# plot(m, terminal_panel = node_diagnosticKriging(m, which = c("loo", "qq"), linetype = "dashed", linecol = "blue", ylines = 2, kriging.type = "UK"))

node_diagnosticKriging <- function(mobobj, id = TRUE, pop = TRUE,
	which = c("loo", "res", "qq"),
	pointcol = "black", pointcex = 0.5, linecol = "black", linetype = 1,
	ylines = 2, margins = rep(1.5, 4),
	kriging.type, trend.reestim = FALSE,
	...) {
    
	## determine scalings
	y <- response(mobobj)
	y <- y[[1]]
	yscale <- range(y) + c(-0.1, 0.1) * diff(range(y))	
	
	pred <- predictMobKriging(mobobj, km.args = list(type = "UK", light.return = TRUE))
	yhat <- pred$mean
	yhatscale <- range(yhat) + c(-0.1, 0.1) * diff(range(yhat))

	sigma <- pred$sd
	resid <- (y - yhat)/(sigma + 0.01)
## FIXME: resids are way too small
	residscale <- c(-2,2) # range(resid) + c(-0.1, 0.1) * diff(range(resid))

	n <- length(y)
	indexscale <- c(0,n+1)
	
    ## number of panels needed
  	k <- length(which)

    #### set up appropriate panel functions
	## 1) leave-one-out
	loo_fun <- function(y, yhat, xmin, xmax, name, ...) {
		pushViewport(plotViewport(margins = margins, name = name,
			yscale = yhatscale, xscale = yscale))
		grid.points(x = y, y = yhat, gp = gpar(col = pointcol, cex = pointcex))
		grid.abline(intercept = 0, slope = 1, gp = gpar(col = linecol, lty = linetype))
		#grid.lines(x = c(xmin, xmax), y = c(xmin, xmax), default.units = "native", gp = gpar(col = linecol, lty = linetype))
		grid.xaxis(at = c(ceiling(yscale[1]*10), floor(yscale[2]*10))/10)
		grid.yaxis(at = c(ceiling(yhatscale[1]), floor(yhatscale[2])))
		grid.rect(gp = gpar(fill = "transparent"))
		grid.text("Fitted values", y = unit(0.5, "npc"), x = unit(-2.5, "lines"), rot = 90)
		grid.text("Exact values", x = unit(0.5, "npc"), y = unit(-2, "lines"))                
		if (pop) popViewport() else upViewport()			
		# plot(x = y[, 1], y = yhat, xlim = c(xmin, xmax), ylim = c(xmin, 
			# xmax), xlab = "Exact values", ylab = "Fitted values", 
			# main = "Leave-one-out", ...)
		# lines(x = c(xmin, xmax), y = c(xmin, xmax))
	}
		
	## 2) standardized residuals
	res_fun <- function(index, resid, name, ...) {
		pushViewport(plotViewport(margins = margins, name = name,
			yscale = residscale, xscale = indexscale))
		grid.points(x = index, y = resid, gp = gpar(col = pointcol, cex = pointcex))
		grid.abline(intercept = 0, slope = 0, gp = gpar(col = linecol, lty = linetype))
## FIXME: bessere achseneinteilung
		# grid.xaxis(at = seq(1,n,10))
		grid.xaxis(at = c(ceiling(indexscale[1]*10), floor(indexscale[2]*10))/10)
		grid.yaxis(at = c(ceiling(residscale[1]), floor(residscale[2])))
		grid.rect(gp = gpar(fill = "transparent"))
		grid.text("Stand. residuals", y = unit(0.5, "npc"), x = unit(-2.5, "lines"), rot = 90)
		grid.text("Index", x = unit(0.5, "npc"), y = unit(-2, "lines"))                
		if (pop) popViewport() else upViewport()			
		# plot(resid, xlab = "Index", ylab = "Standardized residuals", 
			# main = "Standardized residuals", ...)
	}
		
	## 3) qq-plot
	# modified version of qqline that returns intercept and slope
	myqqline <- function (y, datax = FALSE, distribution = qnorm, probs = c(0.25, 
		0.75), qtype = 7, ...) {
		stopifnot(length(probs) == 2, is.function(distribution))
		y <- quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
		x <- distribution(probs)
		if (datax) {
			slope <- diff(x)/diff(y)
			int <- x[1L] - slope * y[1L]
		} else {
			slope <- diff(y)/diff(x)
			int <- y[1L] - slope * x[1L]
		}
		return(list(int = int, slope = slope))
	}

	qq_fun <- function(resid, name, ...) {
## FIXME: xscale: klappt das immer?
		xscale = c(-4,4)
		pushViewport(plotViewport(margins = margins, name = name,
			yscale = residscale, xscale = xscale))
		p <- qqnorm(resid, plot.it = FALSE)
		grid.points(x = unit(p$x, "native"), y = unit(p$y, "native"), gp = gpar(col = pointcol, cex = pointcex))
		l <- myqqline(resid)
		grid.abline(intercept = l$int, slope = l$slope, gp = gpar(col = linecol, lty = linetype))
		grid.xaxis(at = c(ceiling(xscale[1]*10), floor(xscale[2]*10))/10)
		grid.yaxis(at = c(ceiling(residscale[1]), floor(residscale[2])))
		grid.rect(gp = gpar(fill = "transparent"))
		grid.text("Sample quantiles", y = unit(0.5, "npc"), x = unit(-2.5, "lines"), rot = 90)
		grid.text("Theor. quantiles", x = unit(0.5, "npc"), y = unit(-2, "lines"))                
		if (pop) popViewport() else upViewport()			
		# qqnorm(resid, main = "Normal QQ-plot of standardized residuals")
		# qqline(resid)
	} 

	## draw plots
    rval <- function(node) {
## FIXME: geht das auch global vorher?
		## calculate residuals
		model <- node$model$m
		pred <- leaveOneOut.km(model, type = kriging.type, trend.reestim = trend.reestim)
		y <- as.matrix(model@y)
		yhat <- pred$mean
		sigma <- pred$sd
		resid <- (y - yhat)/sigma
		xmin <- min(min(yhat), min(y))
		xmax <- max(max(yhat), max(y))
		index <- which(node$weights > 0)
		
		## set up top viewport
		top_vp <- viewport(layout = grid.layout(nrow = k, ncol = 2,
			widths = unit(c(ylines, 1), c("lines", "null")), heights = unit(k, "null")),
			width = unit(1, "npc"), height = unit(1, "npc") - unit(2, "lines"),
			name = paste("node_mob", node$nodeID, sep = ""))
		pushViewport(top_vp)
		grid.rect(gp = gpar(fill = "white", col = 0))

		## main title
		top <- viewport(layout.pos.col = 2, layout.pos.row = 1)
		pushViewport(top)
		mainlab <- paste(ifelse(id, paste("Node", node$nodeID, "(n = "), ""),
			sum(node$weights), ifelse(id, ")", ""), sep = "")
		grid.text(mainlab, y = unit(1, "npc") - unit(0.75, "lines"))
		popViewport()

		## leave-one-out
		if ("loo" %in% which) {
			# select panel
			plot_vpi <- viewport(layout.pos.col = 2, layout.pos.row = which("loo" == which))
			pushViewport(plot_vpi)
			# call panel function
			loo_fun(y = y[,1], yhat = yhat, xmin, xmax, paste("node_mob", node$nodeID, "-", "loo", sep = ""), ...)
			if (pop) popViewport() else upViewport()
			# plot(x = y[, 1], y = yhat, xlim = c(xmin, xmax), ylim = c(xmin, 
				# xmax), xlab = "Exact values", ylab = "Fitted values", 
				# main = "Leave-one-out", ...)
			# lines(x = c(xmin, xmax), y = c(xmin, xmax))
		}
		
		## standardized residuals
		if ("res" %in% which) {
			# select panel
			plot_vpi <- viewport(layout.pos.col = 2, layout.pos.row = which("res" == which))
			pushViewport(plot_vpi)
			# call panel function
			res_fun(index, resid, paste("node_mob", node$nodeID, "-", "res", sep = ""), ...)
			if (pop) popViewport() else upViewport()
			# plot(resid, xlab = "Index", ylab = "Standardized residuals", 
				# main = "Standardized residuals", ...)
		}

# print(residscale)
# print(range(resid))

		## qq-plot
		if ("qq" %in% which) {
			# select panel
			plot_vpi <- viewport(layout.pos.col = 2, layout.pos.row = which("qq" == which))
			pushViewport(plot_vpi)
			# call panel function
			qq_fun(resid, paste("node_mob", node$nodeID, "-", "qq", sep = ""), ...)
			if (pop) popViewport() else upViewport()
			# qqnorm(resid, main = "Normal QQ-plot of standardized residuals")
			# qqline(resid)
		}

		if (pop) popViewport() else upViewport()
	
	}
    
	return(rval)

}
class(node_diagnosticKriging) <- "grapcon_generator"



# plot.km
# function (x, kriging.type = "UK", trend.reestim = FALSE, ...) 
# {
    # model <- x
    # pred <- leaveOneOut.km(model, type = kriging.type, trend.reestim = trend.reestim)
    # y <- as.matrix(model@y)
    # yhat <- pred$mean
    # sigma <- pred$sd
    # resid <- (y - yhat)/sigma
    # xmin <- min(min(yhat), min(y))
    # xmax <- max(max(yhat), max(y))
    # par(mfrow = c(3, 1))
    # plot(x = y[, 1], y = yhat, xlim = c(xmin, xmax), ylim = c(xmin, 
        # xmax), xlab = "Exact values", ylab = "Fitted values", 
        # main = "Leave-one-out", ...)
    # lines(x = c(xmin, xmax), y = c(xmin, xmax))
    # plot(resid, xlab = "Index", ylab = "Standardized residuals", 
        # main = "Standardized residuals", ...)
    # qqnorm(resid, main = "Normal QQ-plot of standardized residuals")
    # qqline(resid)
    # par(mfrow = c(1, 1))
    # invisible(pred)
# }
# <environment: namespace:DiceKriging>
