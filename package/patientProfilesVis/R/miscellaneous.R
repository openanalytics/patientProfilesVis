#' get path of the any \code{file} template in the \code{CSRFigures} package
#' @param file file name (with extension)
#' @author Laure Cougnaud
#' @export
getPathTemplate <- function(file){
	system.file(file.path("template", file), package = "patientProfilesVis")
}

#' Get approximately the number of lines in the y-axis of the plot.
#' Can be used to specify plot-specific height during the export.
#' @author Laure Cougnaud
#' @inheritParams getNLinesLabel
#' @inherit getNLinesLabel return
#' @importFrom ggplot2 ggplot_build
#' @export
getNLinesYGgplot <- function(gg){
	
	dataPlot <- ggplot_build(gg)$data
	nLinesPlot <- if(inherits(gg, "subjectProfileLinePlot")){
		sum(length(unique(unlist(lapply(dataPlot, function(x)		unique(x$PANEL)))))) * 4
	}else{
		sum(length(unique(unlist(lapply(dataPlot, function(x)		unique(x$y))))))
	}

	nLinesTitleAndXAxis <- sum(c(
		getNLinesLabel(gg = gg, elName = "title"), 
		getNLinesLabel(gg = gg, elName = "x"),
		getNLinesLabel(gg = gg, elName = "caption")
	))
	nLines <- nLinesPlot + nLinesTitleAndXAxis
	return(nLines)
}

#' Get variable with page of the plot,
#' used for automatic paging of a plot
#' @param data data.frame with data
#' @param var string, variable of \code{data} with variable for the y-axis
#' @param typeVar string, type of the variable, either 'y' or 'panel'
#' @param formatReport list of output from the 
#' \code{\link{subjectProfileReportFormat}} function
#' @param title logical, has the plot a title?
#' @param xLab logical, has the plot a label for the x-axis?
#' @param caption logical, has the plot a caption?
#' @return input \code{data} with additional column 'pagePlot'
#' containing the page for the plot
#' @author Laure Cougnaud
getPageVar <- function(data, var, 
	typeVar = c("y", "panel"),
	formatReport = subjectProfileReportFormat(),
	title = TRUE, xLab = TRUE, caption = TRUE){
	
	typeVar <- match.arg(typeVar)
	
	# maximum number of lines for the plot
	maxNLines <- do.call(getMaxNLinesCombinePlot, formatReport) - 
		sum(c(title, xLab, caption)) # let some space for title/x/caption
	
	# compute number of elements per page
	nElPerPage <- floor(maxNLines / switch(typeVar, 'y' = 1, 'panel' = 4))
	
	# in case some levels are not present for some subjects
	data[, var] <- droplevels(data[, var])
	
	# get vector with cumulative number of lines across plots
	levelsRows <- seq_len(nlevels(data[, var]))

	# cut the variable by the maximum number of lines
	numVect <- .bincode(x  = levelsRows, 
		breaks = c(seq(from = 1, to = max(levelsRows), by = nElPerPage), Inf),
		right = FALSE
	)
	names(numVect) <- levels(data[, var])
	
	# create a variable with grouping
	data$pagePlot <- numVect[data[, var]]
	
	return(data)
	
}

#' Get number of lines for specific label
#' @param gg \code{\link[ggplot2]{ggplot2}} object
#' @param elName string with name of label to extract,
#' among 'x', 'y' and 'title'
#' @param elNLines (optional) integer with number of lines,
#' by default 2 for 'x'/'y' and 3 for 'title'
#' @return vector with (approximated) number of lines
#' @author Laure Cougnaud
#' @importFrom ggplot2 ggplot_build
getNLinesLabel <- function(gg, 
	elName = c("x", "y", "title", "caption"),  elNLines = NULL){

	elName <- match.arg(elName, several.ok = TRUE)
	
	if(is.null(elNLines))
		elNLines <- c("x" = 2, "y" = 2, "title" = 3, "caption" = 2)[elName]
	
	elValue <- ggplot_build(gg)$plot$labels[elName]
	sum(unlist(
		lapply(names(elValue), function(elNameI){
			x <- elValue[[elNameI]]
			if(!is.null(x) && !(is.character(x) && x == "")){
				if(is.expression(x))	x <- as.character(x)
				length(unlist(strsplit(x, split = "\n"))) * elNLines[elNameI]
			}
		})
	))
}


#' Get maximum number of lines of a 'combined plot'
#' for a specific document
#' @inheritParams subjectProfileReportFormat
#' @return numeric with maximum height for plot
#' @importFrom grid convertX
#' @author Laure Cougnaud
getMaxNLinesCombinePlot <- function(
	heightLineIn = subjectProfileReportFormat()$heightLineIn,
	margin = subjectProfileReportFormat()$margin,
	landscape = subjectProfileReportFormat()$landscape,
	aspectRatio = subjectProfileReportFormat()$aspectRatio){
	
	heightForPlot <- (
		# page dimension
		convertX(unit(ifelse(landscape, 21, 29.7), "cm"), "inches", valueOnly = TRUE) -
		# margin:
		margin * 2 -
		# section: 17 pt (only for first page) + subsection: 14 pt
		sum(convertX(unit(c(14, 17), "pt"), "inches", valueOnly = TRUE))
	) / aspectRatio

	nLinesPlot <- heightForPlot/heightLineIn

	return(nLinesPlot)
	
}

#' Get width for a plot for a certain page layout
#' @inheritParams subjectProfileReportFormat
#' @return width for the plot in inches
#' @importFrom grid convertX
#' @author Laure Cougnaud
getWidthPlot <- function(
	margin = subjectProfileReportFormat()$margin,
	landscape = subjectProfileReportFormat()$landscape,
	aspectRatio = subjectProfileReportFormat()$aspectRatio){
	
	widthPage <- (
		convertX(unit(ifelse(landscape, 29.7, 21), "cm"), "inches", valueOnly = TRUE) -
		margin*2
	)/ aspectRatio
	
	return(widthPage)

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
			res
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
#' getPatientColorPalette(n = 10)
#' getPatientColorPalette(x = paste('treatment', 1:4))
#' @importFrom viridisLite viridis
#' @importFrom glpgStyle glpgPaletteCharts
#' @export
getPatientColorPalette <- function(n = NULL, x = NULL, type = c("GLPG", "viridis")){
	
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
	x[is.na(x)] <- 'NA'
	if(is.null(n)) n <- length(x)
	
	basePalette <- c(19, 15, 23:25, 1:14)
	
	palette <- rep(basePalette, length.out = n)
	
	if(!is.null(x)) names(palette) <- x	else	palette <- unname(palette)
	
	return(palette)
	
}

#' Custom \code{\link{ggplot2}[theme]} for subject profile plot.
#' Currently classic dark-on-light ggplot2 theme with alternated 
#' grey color for vertical grid lines
#' @return \code{\link{ggplot2}[theme]} object
#' @author Laure Cougnaud
#' @export
subjectProfileTheme <- function(){
	customTheme <- theme_bw() +
		theme(panel.grid.major.y = element_line(colour = c("grey80", theme_bw()$panel.grid$colour)))
	return(customTheme)
}

#' Convert aesthetic variable to factor, converting the empty values ('') to NA
#' @param data data.frame with data
#' @param var variable of \code{data} with aesthetic
#' @return updated factor \code{var} variable
#' @author Laure Cougnaud
convertAesVar <- function(data, var){
	x <- data[, var]
	idxEmpty <- which(x == "")
	if(length(idxEmpty)){
		message("Empty records in the: '", var, "' variable are converted to NA.")
		x[idxEmpty] <- NA
	}
	res <- factor(x, exclude = NULL)
	return(res)
}

#' Get custom 'scale_[type]_manual' function
#' @param lab label for the scale (title of the legend)
#' @param palette named vector with color palette
#' @param type string with type of scale, e.g. 'color'
#' @return output of the 'scale_[type]_manual' function
#' @author Laure Cougnaud
#' @import ggplot2
getAesScaleManual <- function(lab, palette, type){
	
	paramsScale <- c(
		list(
			name = lab,
			values = palette, drop = FALSE,
			limits = names(palette)
		),
		if('NA' %in% names(palette))	
			list(na.value = palette['NA'])
	)
	scaleFct <- paste0("scale_", type, "_manual")
	res <- do.call(scaleFct, paramsScale)
	
	return(res)
	
}

#' Format a variable as factor, and optionally sorted according to the levels
#' of a grouping variable
#' @param data data.frame with data
#' @param paramVar string, variable of \code{data} with parameter
#' @param paramGroupVar (optional) character vector with variable(s) of \code{data} with grouping.
#' If specified, the parameters will be grouped by this(these) variable(s) in the y-axis.
#' @param revert logical, if TRUE revert the order of the levels of the variable
#' @return vector with re-formatted \code{paramVar}, NULL if empty
#' @author Laure Cougnaud
formatParamVar <- function(data, paramVar = NULL, paramGroupVar = NULL, revert = FALSE){
	
	res <- if(!is.null(paramVar)){
		
		paramVarVect <- if(!is.factor(data[, paramVar])){	
			factor(data[, paramVar])
		}else data[, paramVar]
	
		# if paramGroupVar is specified: change order levels of 'variable'
		if(!is.null(paramGroupVar)){
			if(is.null(paramVar)){
				warning("The variable used for grouping ('paramGroupVar') is not used",
					"because no variable for parameter ('paramVar') is not specified.")
			}else{
				groupVariable <- if(length(paramGroupVar) > 1){
					interaction(data[, paramGroupVar])
				}else{
					if(!is.factor(data[, paramGroupVar]))
						factor(data[, paramGroupVar])	else	data[, paramGroupVar]
				}	
				if(!all(tapply(groupVariable, paramVarVect, n_distinct) == 1)){
					warning(paste("The grouping variable:", groupVariable, "is not used, ",
						"because it is not unique for all parameters."))
				}else{
					paramVarVect <- reorder(paramVarVect, groupVariable, unique)
				}
			}
		}
		
		if(revert){
			factor(paramVarVect, levels = rev(levels(paramVarVect)))
		}else	paramVarVect
	
	}
	
	return(res)
	
}

#' Filter a dataset for records of interest
#' @param data data data.frame with data
#' @param subsetVar variable of \code{data} used for subsetting
#' @param subsetValue character vector with value(s) of interest to consider for 
#' \code{subsetVar}
#' @return possibly filtered dataset
#' @author Laure Cougnaud
#' @export
filterData <- function(data, 
	subsetVar = NULL, 
	subsetValue = NULL
){
	
	if(!is.null(subsetVar)){
		if(is.null(subsetValue)){
			warning("Subset variable: ", subsetVar, 
				"not used, because no value of interest is specified.")
		}else{
			data <- data[which(data[, subsetVar] %in% subsetValue), ]
		}
	}
	
	return(data)
	
}

#' Get list with format specification for subject profile report,
#' setting default for entire workflow
#' @param heightLineIn height of a line in inches
#' @param margin margin in inches
#' @param landscape logical, if TRUE the created report is in landscape format
#' @param aspectRatio ratio between size of image in inches 
#' (derived from specified margin, landscape and heightLineIn)
#' and real size for exported image
#' @return list with input parameters.
#' If not specified, default are used.
#' @author Laure Cougnaud
#' @export
subjectProfileReportFormat <- function(
	heightLineIn = 0.2,
	margin = 0.75,
	landscape = FALSE,
	aspectRatio = 0.5){

	paramFormat <- list(
		heightLineIn = heightLineIn, margin = margin, 
		landscape = landscape, aspectRatio = aspectRatio
	)
	
	return(paramFormat)

}