#' Get label(s) for a variable of the dataset
#' 
#' The label(s) are extracted either:
#' \itemize{
#' \item{if \code{data} is specified: }{from the 'label' attribute 
#' of the corresponding column in \code{data}
#' }
#' \item{if \code{labelVars} is specified: }{
#' from the specified vector of labels}
#' } 
#' @param var variable of data
#' @param data data.frame with data
#' @param labelVars named string with variable labels (names are the variable code)
#' @return string with label, \code{var} is no label is available
#' @author Laure Cougnaud
#' @export
getLabelVar <- function(var, data = NULL, labelVars = NULL){
	res <- if(!is.null(var)){
		if(is.null(data) & is.null(labelVars)){
			res <- var
			names(res) <- var
			res
		}else{
			#		stop("'data' or 'labelVars' should be specified for the label(s) extraction.")
			res <- sapply(var, function(x){
				attrX <- unname(if(!is.null(labelVars))
					labelVars[x]	else
					attr(data[, x], "label")
				)
				ifelse(is.null(attrX) || is.na(attrX), x, attrX)
			})
		}
	}
	return(res)
}

#' Get color palette of specified length,
#' either from a vector of names for the palette, or
#' from a specified length.
#' @param x vector with elements used as names for the palette.
#' If factor, the levels are used, otherwise the unique elements of the vector.
#' @param n number of elements in the palette
#' @param type character with type of palette used:
#' 'viridis' for color-blinded friendly palette or
#' 'GLPG' for Galapagos color palette
#' @return vector of viridis colors,
#' named with the elements in \code{x} if \code{x} is specified.
#' @author Laure Cougnaud
#' @examples 
#' getGLPGColorPalette(n = 10)
#' getGLPGColorPalette(x = paste('treatment', 1:4))
#' @importFrom viridisLite viridis
#' @importFrom glpgStyle glpgPaletteCharts
#' @export
getGLPGColorPalette <- function(n = NULL, x = NULL, type = c("GLPG", "viridis")){
	
	type <- match.arg(type)
	
	if(is.null(x) & is.null(n))
		stop("A vector ('x') or number of colors ('n') ",
				"should be specified.")
	
	x <- if(is.factor(x))	levels(x)	else unique(x)
	x[is.na(x)] <- 'NA'
	if(is.null(n)) n <- length(x)
	
	palette <- switch(type,
			'viridis' = viridis(n),
			'GLPG' = rep(glpgPaletteCharts(), length.out = n)
	)
	
	if(!is.null(x)) names(palette) <- x	else	palette <- unname(palette)
	
	return(palette)
	
}

#' Get shape palette.
#' 
#' Note that 20 unique symbols are available at maximum.
#' @inheritParams getGLPGColorPalette
#' @return vector of integer values with shape
#' @author Laure Cougnaud
#' @export
getGLPGShapePalette <- function(n = NULL, x = NULL){
	
	if(is.null(x) & is.null(n))
		stop("A vector ('x') or number of colors ('n') ",
				"should be specified.")
	
	x <- if(is.factor(x))	levels(x)	else unique(x)
	x[is.na(x)] <- 'NA'
	if(is.null(n)) n <- length(x)
	
	basePalette <- c(19, 15, 23:25, 1:14)
	
	palette <- rep(basePalette, length.out = n)
	
	if(!is.null(x)) names(palette) <- x	else	palette <- unname(palette)
	
	return(palette)
	
}

#' Get linetype palette.
#' 
#' Note that 7 unique symbols are available at maximum.
#' @inheritParams getGLPGColorPalette
#' @return character vector values with linetype
#' @author Laure Cougnaud
#' @export
getGLPGLinetypePalette <- function(n = NULL, x = NULL){
	
	if(is.null(x) & is.null(n))
		stop("A vector ('x') or number of colors ('n') ",
				"should be specified.")
	
	x <- if(is.factor(x))	levels(x)	else unique(x)
	x[is.na(x)] <- 'NA'
	if(is.null(n)) n <- length(x)
	
	basePalette <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash") 
	
	palette <- rep(basePalette, length.out = n)
	
	if(!is.null(x)) names(palette) <- x	else	palette <- unname(palette)
	
	return(palette)
	
}

#' capitalize the first letter of a word, from the help of the 'toupper' function
#' @param x string
#' @param onlyFirst logical, if TRUE (by default)
#' capitalize the first letter of the first forward only
#' @param rev logical, if TRUE (FALSE by default), set first letter to lower case (otherwise upper case)
#' @return string with first letter capitalized
#' @author author of the 'toupper' function?
#' @export
simpleCap <- function(x, onlyFirst = TRUE, rev = FALSE) {
	paste0c <- function(...) paste(..., sep = "", collapse = " ")
	fctToUse <- get(ifelse(rev, "tolower", "toupper"))
	simpleCap1 <- function(s) paste0c(fctToUse(substring(s, 1, 1)), substring(s, 2))
	sapply(x, function(x){	
		s <- strsplit(x, " ")[[1]]
		if(onlyFirst)	paste0c(c(simpleCap1(s[1]), s[-1]))	else	simpleCap1(s)
	})
}


