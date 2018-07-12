#' get path of the any \code{file} template in the \code{CSRFigures} package
#' @param file file name (with extension)
#' @author Laure Cougnaud
#' @export
getPathTemplate <- function(file){
	system.file(file.path("template", file), package = "patientProfilesVis")
}

#' Get approximately the number of lines in the y-axis of the plot.
#' Can be used to specify plot-specific height during the export.
#' @param gg \code{\link[ggplot2]{ggplot2}} object
#' @return vector with (approximated) number of lines
#' @author Laure Cougnaud
#' @importFrom ggplot2 ggplot_build
#' @export
getNLinesYGgplot <- function(gg){
	nLinesPlot <- length(unique(ggplot_build(gg)$data[[1]]$y))
	nLinesTitleAndXAxis <- sum(unlist(lapply(ggplot_build(gg)$plot$labels[c("title", "x")], function(label)
		if(label != "")	length(unlist(strsplit(label, split = "\n"))) * 3
	)))
	nLines <- nLinesPlot + nLinesTitleAndXAxis
	return(nLines)
}

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
		}else{
			#		stop("'data' or 'labelVars' should be specified for the label(s) extraction.")
			res <- sapply(var, function(x){
						attrX <- if(!is.null(labelVars))
									labelVars[x]	else
									attr(data[, x], "label")
						ifelse(is.null(attrX), x, attrX)
					})
		}
	}
	return(res)
}

#' Get viridis color palette of specified length,
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
#' getColorPalette(n = 10)
#' getColorPalette(x = paste('treatment', 1:4))
#' @importFrom viridis viridis
#' @importFrom glpgStyle glpgPaletteCharts
#' @export
getPatientColorPalette <- function(n = NULL, x = NULL, type = c("GLPG", "viridis")){
	
	type <- match.arg(type)
	
	if(is.null(x) & is.null(n))
		stop("A vector ('x') or number of colors ('n') ",
			"should be specified.")
	
	x <- if(is.factor(x))	levels(x)	else unique(x)
	if(is.null(n)) n <- length(x)
	
	palette <- switch(type,
		'viridis' = viridis(n),
		'GLPG' = rep(glpgPaletteCharts(), length.out = n)
	)
	
	if(!is.null(x)) names(palette) <- x	else	palette <- unname(palette)
	
	return(palette)
	
}

#' Get shape palette for \code{\link[plotly]{plotly}}
#' 
#' Note that 20 unique symbols are available at maximum.
#' @inheritParams getPatientColorPalette
#' @return vector of integer values with shape
#' @author Laure Cougnaud
#' @export
getPatientShapePalette <- function(n = NULL, x = NULL){
	
	if(is.null(x) & is.null(n))
		stop("A vector ('x') or number of colors ('n') ",
			"should be specified.")
	
	x <- if(is.factor(x))	levels(x)	else unique(x)
	
	if(is.null(n)) n <- length(x)
	
	basePalette <- c(19, 15, 23:25, 1:14)
	
	palette <- rep(basePalette, length.out = n)
	
	if(!is.null(x)) names(palette) <- x	else	palette <- unname(palette)
	
	return(palette)
	
}