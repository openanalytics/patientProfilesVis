#' Get useful transformation for the time variable
#' in patient profiles.
#' @param type String with transformation type, either:
#' \itemize{
#' \item{'asinh': }{hyperbolic arc-sine (\code{\link{asinh}}) transformation}
#' \item{'asinh-neg': }{hyperbolic arc-sine transformation only for the negative values,
#' otherwise linear scale}
#' }
#' @param scale Numeric vector of length 1 (1 by default)
#' with size of the linear region around 0, only used if in case 
#' \code{type} is: 'asinh'.\cr
#' If specified, the time variable is first scaled with: \code{x/scale},
#' then transformed.
#' @param n Integer of length 1 with number of breaks, 10 by default.
#' @param formatFct function formatting the time axis breaks,
#' (\code{\link{prettyNum}} by default),
#' see \code{format} parameter of the \code{\link[scales]{trans_new}}.
#' @return ggplot2 transformation (see \code{\link[scales]{trans_new}})
#' @author Pieter-Jan Stiers, Laure Cougnaud
#' @importFrom scales trans_new
#' @importFrom grDevices axisTicks
#' @importFrom utils head
#' @export
getTimeTrans <- function(
	type = c("asinh", "asinh-neg"),
	scale = 1,
	formatFct = prettyNum,
	n = 10){

	type <- match.arg(type)
	
	if(scale != 1 & type == "asinh-neg")
		warning("Scale is not available for asinh-neg transformation, scale is set to 1.")

	transf <- switch(type,
		'asinh' = {
			
			# extended version of scales::trans_breaks
			brkFct <- function(x){
				n <- 10
				r <- range(x, na.rm = TRUE)
				# create breaks in the asinh scale
				rTrans <- asinh(r)
				brTrans <- seq(from = ceiling(min(rTrans)), to = floor(max(rTrans)), length.out = n)
				# take close breaks in a log10 scale (prettier than asinh scale)
				brRaw <- sinh(brTrans)
				breaks <- unique(10^round(log10(abs(brRaw)))*sign(brRaw))
				if(diff(sign(r)) > 0)	breaks <- c(breaks, 0)
				return(breaks)
			}
			# pass scale argument programmatically:
			# !! should be modified if function definition has been changed
			body(brkFct)[[2]] <- bquote(n <- .(n))
			body(brkFct)[[4]] <- bquote(rTrans <- asinh(r/.(scale)))
			body(brkFct)[[6]] <- bquote(brRaw <- sinh(brTrans*.(scale)))
	
			# create transform and inverse function
			trFct <- function(x) x
			body(trFct) <- bquote(asinh(x/.(scale)))
			invFct <- function(x) x
			body(invFct) <- bquote(sinh(x)*.(scale))
			
			# create transf object
			trans_new(
				name = 'asinh', 
				transform = trFct, 
				inverse = invFct,
				breaks = brkFct,
				format = formatFct
			)
			
		},
		'asinh-neg' = {
			
			# extended version of scales::trans_breaks
			brkFct <- function(x){
				n <- 10
				min <- min(x, na.rm = TRUE)
				max <- max(x, na.rm = TRUE)
				nPos <- nNeg <- 0
				include0 <- FALSE
				if(min < 0 & max < 0){
					nNeg <- n
				}else	if(min > 0 & max > 0){
					nPos <- n
				}else{
					minTr <- asinh(min)
					n <- n - 1
					nNeg <- round(abs(minTr)/diff(c(minTr, max)) * n)
					nNeg <- min(n, max(nNeg, 1))
					nPos <- n-nNeg
					include0 <- TRUE
				}
				breaks <- c()
				if(nNeg > 0){
					# create breaks in the asinh scale
					brTrans <- seq(from = ceiling(asinh(min)), to = 0, length.out = nNeg+1)
					brTrans <- head(brTrans, -1)
					# take close breaks in a log10 scale (prettier than asinh scale)
					brRaw <- sinh(brTrans)
					breaksNeg <- unique(10^round(log10(abs(brRaw)))*sign(brRaw))
					breaks <- c(breaks, breaksNeg)
				}
				if(nPos > 0){
					breaksPos <- axisTicks(c(0, max), log = FALSE, nint = nPos+1)[-1]
					breaks <- c(breaks, breaksPos)
				}
				if(include0) breaks <- c(breaks, 0)
				breaks <- sort(breaks)
				return(breaks)
			}
			body(brkFct)[[2]] <- bquote(n <- .(n))
			
			trFct <- function(x)	ifelse(x < 0, asinh(x), x)	
			invFct <- function(x)	ifelse(x < 0, sinh(x), x)	
			scales::trans_new(
				name = "asinh_neg",
				transform = trFct,
				inverse = invFct,
				breaks = brkFct,
				format = formatFct
			)
		
	})

 	return(transf)

}