#' get path of the any \code{file} template in the \code{CSRFigures} package
#' @param file file name (with extension)
#' @author Laure Cougnaud
#' @export
getPathTemplate <- function(file){
	system.file(file.path("template", file), package = "patientProfilesVis")
}

#' Get approximately the number of 'lines' in the vertical direction
#' of a \code{\link[ggplot2]{ggplot2}} plot.
#' This is extracted from the presence of labels in the y-axis,
#' labels and title in the x-axis, general title and number of lines
#' in the legend.
#' Can be used to specify plot-specific height during the export.
#' @author Laure Cougnaud
#' @inheritParams getNLinesLabel
#' @inherit getNLinesLabel return
#' @importFrom ggplot2 ggplot_build
#' @export
getNLinesYGgplot <- function(gg){
	
	nLinesPlot <- if(inherits(gg, "subjectProfileLinePlot")){
				
		facetVar <- names(gg$facet$params$rows)
		facetLabels <- ggplot_build(gg)$layout$layout[, facetVar]
		# at minimum 4 lines for the line plot
		nLinesPlot <- sum(
			Vectorize(
				FUN = function(x){max(c(x, 4))}
			)(countNLines(facetLabels))
		)
	}else{
		
		# for text profile
		# as text content and y-axis labels can span multiple lines
		# extract the number of lines as the maximum of number of lines of the text content and corresponding axis label
		if(inherits(gg, "subjectProfileTextPlot")){
			dataPanel <- layer_data(gg, i = 1)
			nLinesLabelYAxis <- countNLines(layer_scales(gg, i = 1)$y$get_breaks()[dataPanel$y]) # lines of y-axis labels
			nLinesTextPanel <- countNLines(as.character(dataPanel$label)) # lines of text panel
			nLinesMax <- mapply(max, nLinesTextPanel, nLinesLabelYAxis) # max of n lines of panel and axis labels
			nLinesPlot <- sum(nLinesMax) + 0.8 * (length(nLinesMax) - 1)
		}else{
			nElLayout <- nrow(ggplot_build(gg)$layout$layout)
			yBreaks <- unique(unlist(lapply(seq_len(nElLayout), function(i)	layer_scales(gg, i = i)$y$get_breaks())))
			nLines <- countNLines(yBreaks)
			nLinesPlot <- sum(nLines) + 0.8 * (length(nLines) - 1)
		}
	}

	# in case long legend
	nLinesLegend <- getNLinesLegend(gg)
	nLinesPlot <- max(nLinesPlot, nLinesLegend)

	nLinesTitleAndXAxis <- sum(c(
		getNLinesLabel(gg = gg, elName = "title"), 
		getNLinesLabel(gg = gg, elName = "x"),
		getNLinesLabel(gg = gg, elName = "caption")
	))
	nLines <- nLinesPlot + nLinesTitleAndXAxis
	return(nLines)
}

#' Get number of lines in the legend,
#' either from directly the \code{\link[ggplot2]{ggplot2}} object,
#' or from the values of the legend (\code{legendValues})
#' and title (\code{legendTitle})
#' @param gg \code{\link[ggplot2]{ggplot2}} object
#' @param values Vector with unique legend values
#' @param title Vector with legend title
#' @return integer with (approximated) number of lines
#' @author Laure Cougnaud
#' @importFrom ggplot2 ggplot_gtable ggplot_build
getNLinesLegend <- function(gg, values, title){
	
	if(!missing(gg)){
	
		ggTable <- ggplot_gtable(ggplot_build(gg))
		
		# extract legend grobs
		idxLegend <- which(sapply(ggTable$grobs, function(x) x$name) == "guide-box")
		
		nLinesLegendTotal <- if(length(idxLegend) > 0){
			
			grobLegend <- ggTable$grobs[[idxLegend]]
			idxLegendGuides <- which(grobLegend$layout$name == "guides")
			
			if(length(idxLegendGuides) > 0){
			
				# extract number of lines in each legend
				nLinesLegend <- vapply(seq_along(idxLegendGuides), function(i){
					grobLegendI <- grobLegend$grobs[[i]]
					idxLegendILabels <- grep("^label", grobLegendI$layout$name)
					nLinesLegendILabels <- n_distinct(grobLegendI$layout[idxLegendILabels, "t"])
					nLinesLegendITitle <- sum(grobLegendI$layout$name == "title")
					nLinesLegendILabels + nLinesLegendITitle
				}, FUN.VALUE = numeric(1))
			
				# add extra line which separate legend guides
				sum(nLinesLegend) + length(idxLegendGuides) - 1
		
			}else 0
	
		}else 0

	}else	if(!missing(values)){
		
		nLinesLegendTotal <- sum(countNLines(values)) + if(!missing(title))	countNLines(title)
		
	}else	stop("Legend values via 'values' or a ggplot2 object via 'gg' should be specified.")
	
	return(nLinesLegendTotal)
	
}

#' Get number of lines for specific label either
#' from a \code{\link[ggplot2]{ggplot2}} object via \code{gg}
#' or from the label  via \code{value}
#' @param gg \code{\link[ggplot2]{ggplot2}} object
#' @param value String with label value.
#' @param elName string with name of label to extract,
#' among 'x', 'y' and 'title'
#' @param elNLines (optional) Named integer with number of lines,
#' by default 2 for 'x'/'y', 3 for 'title' and 1 for caption.
#' @return integer with (approximated) number of lines
#' @author Laure Cougnaud
#' @importFrom ggplot2 ggplot_build
getNLinesLabel <- function(
	gg,
	value,
	elName = c("x", "y", "title", "caption"),  elNLines = NULL){
	
	# element should be of length 1 if 'value' is specified
	elName <- match.arg(
		elName, 
		choices = c("x", "y", "title", "caption"), 
		several.ok = !missing(gg)
	)
	
	if(is.null(elNLines))
		elNLines <- c("x" = 2, "y" = 2, "title" = 3, "caption" = 1)[elName]
	
	if(!missing(gg)){
		elValue <- ggplot_build(gg)$plot$labels[elName]
		elValue <- elValue[!sapply(elValue, is.null)]
		nLinesList <- lapply(names(elValue), function(elNameI){
			getNLinesLabel(
				value = elValue[[elNameI]], 
				elName = elNameI,
				elNLines = elNLines
			)
#				if(!is.null(x) && !(is.character(x) && x == "")){
#					if(is.expression(x))	x <- as.character(x)
#					countNLines(x) * elNLines[elNameI]
#				}
		})
		sum(unlist(nLinesList))
		
	}else	if(!missing(value)){
		
		if(!is.null(value) && !(is.character(value) && value == "")){
			
			if(is.expression(value))	value <- as.character(value)
			countNLines(value) * elNLines[elName]
			
		}
		
	}else stop("Element value 'value' or a ggplot2 object via 'gg' should be specified.")
	
}

#' Count number of lines ('\\n' character) per character in a vector
#' @param x character vector
#' @return numeric vector with same length than \code{x}
#' containing number of lines in each element
#' @importFrom stringr str_count
#' @author Laure
countNLines <- function(x){
	nLineBreaks <- str_count(x, "\n")
	nLineBreaks[is.na(nLineBreaks)] <- 0
	nLineBreaks + 1
}

#' Get variable with page of the plot,
#' used for automatic paging of a plot
#' @param data data.frame with data
#' @param var string, variable of \code{data} with variable for the y-axis
#' @param typeVar string, type of the variable, either:
#' 'y': the variable is displayed in the x-axis or 
#' 'panel': the variable is displayed as separated facets.
#' This is used to compute height for each line of the plot.
#' @param formatReport list with parameters used to specify the format of the report,
#' e.g. output of the \code{\link{subjectProfileReportFormat}} function
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
	inputGetMNL <- formatReport[names(formatReport) != "yLabelWidth"]
	maxNLines <- do.call(getMaxNLinesCombinePlot, inputGetMNL) - 
		sum(c(title, xLab, caption)) # let some space for title/x/caption
	
	# compute number of elements per page
	nElPerPage <- floor(maxNLines / switch(typeVar, 'y' = 1, 'panel' = 4))
	
	# in case some levels are not present for some subjects
	# and convert to a factor
	data[, var] <- droplevels(data[, var], exclude = NULL)
	
	# get vector with cumulative number of lines across plots
	levelsRows <- seq_len(nlevels(data[, var]))

	# cut the variable by the maximum number of lines
	numVect <- .bincode(
		x  = levelsRows, 
		breaks = c(seq(from = 1, to = max(levelsRows), by = nElPerPage), Inf),
		right = FALSE
	)
	names(numVect) <- levels(data[, var])
	
	# create a variable with grouping
	data$pagePlot <- numVect[data[, var]]
	
	return(data)
	
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

#' Format a variable as factor, 
#' wrap it across multiple lines if needed
#' and optionally sorted according to the levels
#' of a grouping variable
#' @param data data.frame with data
#' @param paramVar string, variable of \code{data} with parameter
#' @param paramGroupVar (optional) character vector with variable(s) of \code{data} with grouping.
#' If specified, the parameters will be grouped by this(these) variable(s) in the y-axis.
#' @param revert logical, if TRUE revert the order of the levels of the variable
#' @param width max number of characters in the code{paramVar} parameter.
#' @return vector with re-formatted \code{paramVar}, NULL if empty
#' @importFrom dplyr n_distinct
#' @author Laure Cougnaud
formatParamVar <- function(data, 
	paramVar = NULL, paramGroupVar = NULL, 
	revert = FALSE,
	width = 20){
	
	res <- if(!is.null(paramVar)){
		
		paramVarVect <- if(!is.factor(data[, paramVar])){	
			factor(data[, paramVar])
		}else{
			data[, paramVar]
		}

		# cut too long labels
		paramVarLevels <- formatLongLabel(x = levels(paramVarVect), width = width)
		
		# convert paramVar
		# don't use directly ( labels to avoid error: duplicated factor levels
		paramVarVectNew <- paramVarLevels[as.character(paramVarVect)]		
		paramVarVect <- factor(paramVarVectNew,	levels = unique(paramVarLevels))
	
		# if paramGroupVar is specified: change order levels of 'variable'
		if(!is.null(paramGroupVar)){
			
			paramGroupVarInData <- paramGroupVar %in% names(data)
			
			if(any(!paramGroupVarInData))
				warning("The variable(s): ", toString(shQuote(paramGroupVar[!paramGroupVarInData])),
					" used for grouping is(are) not used because not in the data.")
		
			if(any(paramGroupVarInData)){
				
				paramGroupVar <- paramGroupVar[paramGroupVarInData]
				
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
					if(!all(tapply(groupVariable, paramVarVect, n_distinct) == 1, na.rm = TRUE)){
						warning(paste("The grouping variable:", groupVariable, "is not used, ",
							"because it is not unique for all parameters."))
					}else{
						paramVarVect <- reorder(paramVarVect, groupVariable, unique)
					}
				}
			}
		}
		
		if(revert){
			factor(paramVarVect, levels = rev(levels(paramVarVect)))
		}else	paramVarVect
	
	}
	
	return(res)
	
}

#' Format text variables for the \code{\link{subjectProfileTextPlot}} function, 
#' wrap it across multiple lines if needed,
#' and optionally sorted according to the levels
#' of a grouping variable
#' @param paramValueVar  string, variable of \code{data} containing the parameter value. 
#' @param widthValue max number of characters in the code{paramValueVar} parameter.
#' @inheritParams formatParamVar
#' @inheritParams getPageVar
#' @return \code{data} with reformatted \code{paramVar} and \code{paramValueVar} variables.
#' @author Laure Cougnaud
formatParamVarTextPlot <- function(data, 
	paramVar = NULL, 
	paramValueVar = NULL,
	paramGroupVar = NULL, 
	revert = FALSE,
	width = formatReport$yLabelWidth,
	widthValue = ifelse(
		formatReport$landscape,
		240,
		190
	),
	formatReport = subjectProfileReportFormat()){

	## parameter value variable
	data[, paramValueVar] <- formatParamVar(
		data = data, 
		paramVar = paramValueVar,
		revert = FALSE, paramGroupVar = NULL,
		width = widthValue
	)
	
	## parameter name variable
	data[, paramVar] <- formatParamVar(
		data = data, 
		paramVar = paramVar,
		revert = revert, paramGroupVar = paramGroupVar,
		width = width
	)
	
	return(data)
	
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
		if(!subsetVar %in% names(data)){
			warning("Subset variable not used because not in the data.")
		}else{
			if(is.null(subsetValue)){
				warning("Subset variable: ", subsetVar, 
					"not used, because no value of interest is specified.")
			}else{
				data <- data[which(data[, subsetVar] %in% subsetValue), ]
			}
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
#' @param yLabelWidth integer with approximate maximum width of parameters, 20 by default.
#' If parameter is longer, it will be splitted between words in separated lines.
#' @return list with input parameters.
#' If not specified, default are used.
#' @author Laure Cougnaud
#' @export
subjectProfileReportFormat <- function(
	heightLineIn = 0.2,
	margin = 0.75,
	landscape = FALSE,
	aspectRatio = 0.5,
	yLabelWidth = 30){

	paramFormat <- list(
		heightLineIn = heightLineIn, margin = margin, 
		landscape = landscape, aspectRatio = aspectRatio,
		yLabelWidth = yLabelWidth
	)
	
	return(paramFormat)

}

#' Custom formatting function for labels in plot.
#' @param x character vector with labels to format
#' @param width target maximum size. Note: a word
#' longer that this width won't be split (see \code{\link{strwrap}}).
#' @return vector with formatted labels
#' @author Laure Cougnaud
formatLongLabel <- function(x, width = 20){
	
	xRF <- vapply(x, function(x1)
		xRF <- paste(strwrap(x1, width = width), collapse = "\n"),
		FUN.VALUE = character(1)
	)
	if(length(x) != length(xRF))
		stop("Reformatting of labels failed.")
	
	return(xRF)
	
}