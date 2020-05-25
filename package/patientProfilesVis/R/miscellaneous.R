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
#' @param data Data.frame with data
#' @param var Character vector with variable in legend.
#' @param title Vector with legend title
#' @return integer with (approximated) number of lines
#' @author Laure Cougnaud
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#' @importFrom dplyr n_distinct
getNLinesLegend <- function(gg, values, data, var, title){
	
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
		
		nLinesLegendTotal <- sum(countNLines(values))
		
	}else	if(!missing(data) & !missing(var)){
		
		nLinesLegendList <- lapply(var, function(varI){
			x <- data[, varI]
			values <- if(is.factor(x))	levels(x)	else unique(x)
			getNLinesLegend(values = values)	
		})
		nLinesLegendTotal <- sum(unlist(nLinesLegendList))
		
	}else	stop("Legend values via 'values' or a ggplot2 object via 'gg' should be specified.")
	
	if(missing(gg) & !missing(title))
		nLinesLegendTotal <- nLinesLegendTotal + countNLines(title)
	
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
#' @param paging Logical, if TRUE (by default), the automatic 
#' paging is enabled, otherwise only one page is used.
#' @inheritParams formatParamVarTextPlot
#' @return input \code{data} with additional column 'pagePlot'
#' containing the page for the plot
#' @author Laure Cougnaud
getPageVar <- function(data, var, 
	typeVar = c("y", "panel"),
	formatReport = subjectProfileReportFormat(),
	title = TRUE, xLab = TRUE, caption = TRUE,
	paging = TRUE,
	table = FALSE){

	if(paging){
	
		typeVar <- match.arg(typeVar)
		
		# maximum number of lines for the plot
		inputGetMNL <- formatReport[names(formatReport) != "yLabelWidth"]
		maxNLines <- do.call(getMaxNLinesCombinePlot, inputGetMNL) - 
			sum(c(title, xLab, caption)) # let some space for title/x/caption
		
		if(table){
			
			nLines <- apply(data[, var], 1, function(x) max(countNLines(x)))
			
			# create a variable with grouping
			data$pagePlot <- floor(cumsum(nLines)/maxNLines) + 1
					
		}else{
	
			# in case some levels are not present for some subjects
			# and convert to a factor
			data[, var] <- droplevels(data[, var], exclude = NULL)
			
			# get vector with cumulative number of lines across plots
			levelsRows <- seq_len(nlevels(data[, var]))
			
			# compute number of elements per page
			nLines <- countNLines(levels(data[, var]))
			
			nElPerPage <- floor(maxNLines / 
				max(max(nLines), switch(typeVar, 'y' = 1, 'panel' = 4))
			)
			
			# cut the variable by the maximum number of lines
			numVect <- .bincode(
				x  = levelsRows, 
				breaks = c(seq(from = 1, to = max(levelsRows), by = nElPerPage), Inf),
				right = FALSE
			)
			names(numVect) <- levels(data[, var])

			# create a variable with grouping
			data$pagePlot <- numVect[data[, var]]
			
		}
		
	}else	data$pagePlot <- 1
	
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
#' @export
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



#' Format text variables for the \code{\link{subjectProfileTextPlot}} function, 
#' wrap it across multiple lines if needed,
#' and optionally sorted according to the levels
#' of a grouping variable
#' @param paramValueVar  string, variable of \code{data} containing the parameter value. 
#' @param paramValueLab Character vector with labels for \code{paramValueVar}.
#' @param table Logical, if TRUE the \code{paramValueVar} variables
#' are displayed as table (so are not concatenated).
#' @param colWidth Numeric vector of \code{length(paramValueVar)}
#' containing the approximate width of each parameter value column.
#' Only used if parameter values are displayed as a table (\code{table} is TRUE).
#' Note: columns can be slightly bigger if content larger than the specified width.
#' If not specified, optimized widths are determined.
#' @inheritParams glpgUtilityFct::formatVarForPlotLabel
#' @inheritParams getPageVar
#' @inheritParams getOptimalColWidth
#' @return \code{data} with reformatted \code{paramVar} and \code{paramValueVar} variables,
#' with additional attribute: \code{colWidth}.
#' @importFrom glpgUtilityFct formatVarForPlotLabel
#' @author Laure Cougnaud
formatParamVarTextPlot <- function(data, 
	paramVar = NULL, 
	paramValueVar = NULL, paramValueLab = NULL,
	paramGroupVar = NULL, 
	revert = FALSE,
	width = formatReport$yLabelWidth,
	widthValue = ifelse(
		formatReport$landscape,
		240,
		190
	),
	formatReport = subjectProfileReportFormat(),
	table = FALSE, colWidth = NULL){

	if(table){
		
		if(is.null(colWidth)){

			colWidth <- getOptimalColWidth(
				data = data[, paramValueVar, drop = FALSE], 
				labels = paramValueLab,
				widthValue = widthValue, 
				formatReport = formatReport
			)
			
		}else{
			widths <- rep(colWidth, length.out = length(paramValueVar))
			colWidth <- widthValue * widths/sum(widths)
		}
		
		data[, paramValueVar] <- lapply(seq_along(paramValueVar), function(i){
			paramVar <- paramValueVar[i]
			formatVarForPlotLabel(
				data = data, 
				paramVar = paramVar,
				revert = FALSE, 
				width = colWidth[i]
			)
		})
		if(!is.null(paramGroupVar)){
			groupVariable <- if(length(paramGroupVar) > 1){
				interaction(data[, paramGroupVar])
			}else{
				if(!is.factor(data[, paramGroupVar]))
					factor(data[, paramGroupVar])	else	data[, paramGroupVar]
			}
			data <- data[order(groupVariable), ]
		}
		
	}else{

		## parameter value variable
		data[, paramValueVar] <- formatVarForPlotLabel(
			data = data, 
			paramVar = paramValueVar,
			revert = FALSE, paramGroupVar = NULL,
			width = widthValue
		)
		
		## parameter name variable
		data[, paramVar] <- formatVarForPlotLabel(
			data = data, 
			paramVar = paramVar,
			revert = revert, paramGroupVar = paramGroupVar,
			width = width
		)
		
	}
	
	attr(data, "colWidth") <- colWidth
	
	return(data)
	
}

#' Get optimal column widths, based on the minimum word size
#' and median number of characters in each column.
#' @param data Data.frame with columns for which optimal width should be extracted.
#' @param labels (optional) Character vector with column labels for \code{data}.
#' @param widthValue max number of characters in the code{paramValueVar} parameter.
#' @inheritParams getPageVar
#' @return Numeric vector of \code{length(ncol(data))} with optimal widths.
#' @importFrom stats median
#' @author Laure Cougnaud
getOptimalColWidth <- function(data,
	widthValue = ifelse(
		formatReport$landscape,
		240,
		190
	),
	labels = NULL,
	formatReport = subjectProfileReportFormat()){
	
	if(!is.null(labels))	data <- rbind.data.frame(labels, data)

	# determine minimum column size
	nCharacWordMin <- apply(data, 2, function(x){
		sapply(strsplit(x, split = " "), function(y)
			if(length(y) > 0)	max(nchar(y))	else	2
		)
	})
	nCharacMinCol <- apply(nCharacWordMin, 2, max, na.rm = TRUE)
	nCharacMinCol[is.na(nCharacMinCol)] <- 0
	
	# determine median number of characters in a column
	nCharac <-  apply(data, 2, nchar)
	nCharacMedian <- apply(nCharac, 2, median, na.rm = TRUE)
	nCharacMedian[is.na(nCharacMedian)] <- 0
	 
	idxTooSmall <- which(nCharacMedian < nCharacMinCol)
	nCharacMedian[idxTooSmall] <- nCharacMinCol[idxTooSmall]
	nCharacExtra <- sum(nCharacMedian)-widthValue
	if(nCharacExtra > 0){
		idxTooBig <- which(nCharacMedian > nCharacExtra)
		nCharacMedian[idxTooBig] <- nCharacMedian[idxTooBig]-nCharacExtra/length(idxTooBig)
	}
	colWidth <- floor(nCharacMedian/sum(nCharacMedian)*widthValue)
	
	return(colWidth)
	
}

#' Filter a dataset for records of interest
#' @param data data data.frame with data
#' @param subsetData Data.frame from which subset based on \code{subjectVar}/\code{subsetValue}
#' should be extracted. If not specified, \code{data} is used.
#' Records from \code{data} are then mapped by the \code{subjectVar} variable.
#' @param subsetVar variable of \code{data} used for subsetting
#' @param subsetValue character vector with value(s) of interest to consider for 
#' \code{subsetVar}
#' @param subjectVar string, variable of \code{data} with subject ID
#' @param subjectSubset (optional) Character vector with subjects of interest 
#' (available in \code{subjectVar} of \code{data}), NULL by default.
#' @return possibly filtered dataset
#' @author Laure Cougnaud
#' @export
filterData <- function(data,
	subsetData = NULL,
	subsetVar = NULL, 
	subsetValue = NULL,
	subjectVar = "USUBJID",
	subjectSubset = NULL
){	
	
	# in case data is a tibble:
	if(!is.null(subsetData))
		subsetData <- as.data.frame(subsetData)
	
	# wrapper function to extract subset of interest
	getDataSubset <- function(subsetData){
		if(!is.null(subsetVar)){
			if(!subsetVar %in% names(subsetData)){
				warning("Subset variable not used because not in the data.")
			}else{
				if(is.null(subsetValue)){
					warning("Subset variable: ", subsetVar, 
						"not used, because no value of interest is specified.")
				}else{
					subsetData <- subsetData[which(subsetData[, subsetVar] %in% subsetValue), ]
				}
			}
		}
		return(subsetData)
	}
	
	
	if(!is.null(subsetData)){
		subsetDataUsed <- getDataSubset(subsetData = subsetData)
		subjectSubset <- subsetDataUsed[, subjectVar]
	}else	data <- getDataSubset(subsetData = data)
	
	if(!is.null(subjectSubset))
		data <- data[which(data[, subjectVar] %in% subjectSubset), ]
	
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

#' Check if the all profile(s) is/are 'time-variant',
#' so not a subject profile 'text' module or empty plot
#' @param gg object of class \code{subjectProfileX} 
#' (and \code{\link[ggplot2]{ggplot}}) or potentially
#' nested list of such objects.
#' @param empty Logical, should empty subject profile be
#' considered as time-variant?
#' @return Logical, is plot time variant?
#' @author Laure Cougnaud
isSubjectProfileTimeVariant <- function(gg, empty = TRUE){
	
	if(inherits(gg, "ggplot")){
		classesNonTV <- c(if(empty)	"subjectProfileEmptyPlot", "subjectProfileTextPlot")
		test <- !inherits(gg, classesNonTV)
	}else{
		test <- all(sapply(gg, isSubjectProfileTimeVariant))
		if(length(test) > 1)
			stop("Time variant and non time variant plots are both available.")
	}
	return(test)
}

#' Format specified \code{timeLim}.
#' 
#' In case one of the limits if missing,
#' the corresponding minimum/maximum across subjects is used.
#' @inheritParams subjectProfileIntervalPlot
#' @return Numeric vector of length 2 or list of such element
#' for each subject.
#' @importFrom plyr dlply
#' @author Laure Cougnaud
formatTimeLim <- function(data, subjectVar = "USUBJID", 
	timeStartVar, timeEndVar, timeLim = NULL){
	
	if(!is.null(timeLim)){
		
		if(any(is.na(timeLim))){
			
			idxTimeLimNA <- which(is.na(timeLim))
			timeLim <- dlply(data, subjectVar, function(dataSubject){
				timeLimData <- with(dataSubject, c(
					min(get(timeStartVar), na.rm = TRUE), 
					max(get(timeEndVar), na.rm = TRUE))
				)
				ifelse(is.na(timeLim), timeLimData, timeLim)
			})
	
		}
		
	}
	
	return(timeLim)
	
}
