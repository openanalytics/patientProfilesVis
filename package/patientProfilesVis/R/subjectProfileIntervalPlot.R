#' Create plot of subject profiles with segments for range of parameters 
#' 
#' Note that is no end date is available for a specific record,
#' the maximum of the time limits (\code{timeLim}) is used as end of the interval
#' and a triangle with right direction is added.
#' @param data data.frame with data
#' @param paramVar string, variable of \code{data} with parameter (used in the y-axis)
#' @param paramLab string, label for \code{paramVar}
#' @param subjectVar string, variable of \code{data} with subject ID
#' @param timeStartVar string, variable of \code{data} with start of range
#' @param timeEndVar string, variable of \code{data} with end of range
#' @param timeLim (optional) vector of length 2 with time limits (x-axis)
#' If not specified, extracted from the minimum \code{timeStartVar} and maximum \code{timeEndVar}
#' @param rangeSimilarStartEnd numeric, if a record has the same
#' \code{timeStartVar} and \code{timeEndVar}, what should be the range of the segment?
#' By default, a thousandth of the range of \code{timeLim}.
#' @param xLab string, label for the x-axis
#' @param yLab string, label for the y-axis
#' @param colorVar string, variable of \code{data} with color.
#' @param colorLab string, label for \code{colorVar}
#' @param colorPalette named vector with color for \code{colorVar}
#' @param title string, title for the plot
#' @param label string, label used in the plot.
#' This label is used to report the name of the panel 
#' in the text when the plots are combined (e.g. if the plot is empty).
#' @inheritParams getLabelVar
#' @inheritParams filterData
#' @inheritParams formatParamVar
#' @return list of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileTextPlot}
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom plyr dlply
#' @importFrom stats reorder
#' @export
subjectProfileIntervalPlot <- function(
	data,
	paramVar, paramLab = toString(getLabelVar(paramVar, labelVars = labelVars)),
	paramGroupVar = NULL,
	timeStartVar,
	timeEndVar,
	subjectVar = "USUBJID",
	subsetVar = NULL, subsetValue = NULL,
	timeLim =  with(data, c(min(get(timeStartVar), na.rm = TRUE), max(get(timeEndVar), na.rm = TRUE))),
	rangeSimilarStartEnd = diff(timeLim)/1000,
	xLab = paste(getLabelVar(c(timeStartVar, timeEndVar), labelVars = labelVars), collapse = "/"),
	yLab = "",
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	title = paramLab,
	label = title,
	labelVars = NULL
){
	
	# filter records without start date
	idxMissingStart <- is.na(data[, timeStartVar])
	if(any(idxMissingStart))
		data <- data[-which(idxMissingStart), ]	
	
	# if no end date: take last time in dataset
	idxMissingEnd <- is.na(data[, timeEndVar])
	data$missingEndPlot <- idxMissingEnd
	if(any(idxMissingEnd))
		data[idxMissingEnd, timeEndVar] <- timeLim[2]
	
	# if same start/end, data not included by geom_segment
	# so jitter the start/end in this case (proportion of the total x-range)
	idxSameStartEnd <- which(data[, timeStartVar] == data[, timeEndVar])
	if(length(idxSameStartEnd) > 0){
		data[idxSameStartEnd, c(timeStartVar, timeEndVar)] <-
			sweep(
				x = data[idxSameStartEnd, c(timeStartVar, timeEndVar)], 
				MARGIN = 2, 
				STATS = c(-1, 1) * rangeSimilarStartEnd/2,
				FUN = "+"
			)
	}
	
	# concatenate variable(s) if multiple are specified
	data$yVar <- if(length(paramVar) > 1)
		apply(data[, paramVar], 1, paste, collapse = " ")	else	data[, paramVar]

	# remove records without parameter variable
	data <- data[with(data, !is.na(yVar) & yVar != ""), ]
	
	# only keep records of interest
	data <- filterData(data, 
		subsetVar = subsetVar, 
		subsetValue = subsetValue
	)

	# if paramGroupVar is specified: change order levels of 'variable'
	data$yVar <- formatParamVar(
		data = data, paramVar = "yVar", paramGroupVar = paramGroupVar,
		revert = TRUE
	)
	
	# convert color variable to factor
	if(!is.null(colorVar)){
		data[, colorVar] <- convertAesVar(data, colorVar)
		if(is.null(colorPalette))	colorPalette <- getPatientColorPalette(x = data[, colorVar])
	}
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
						
		subject <- unique(dataSubject[, subjectVar])
			
		# build aesthetic
		aesArgs <- c(
			list(x = timeStartVar, xend = timeEndVar, y = "yVar", yend = "yVar"),
			if(!is.null(colorVar))	list(color = colorVar)
		)
		# and custom geom_segment
		geomSegmentCustom <- function(...)
			geom_segment(
				do.call(aes_string, aesArgs),
				size = 2, ...
			) 
		
		# if missing end, plot arrow, otherwise only a segment
		dataPlotByME <- dlply(dataSubject, "missingEndPlot")
				
		# create the plot
		gg <- ggplot()
		
		# plot segment for records with end date
		if("FALSE" %in% names(dataPlotByME))
			gg <- gg + geomSegmentCustom(data = dataPlotByME[["FALSE"]]) 
	
		# in case of missing end date
		# right-directed triangle not available in shape palette
		# option 1: use Unicode, but symbol not centered
		# option 2: draw segment
		if("TRUE" %in% names(dataPlotByME))
			gg <- gg + geomSegmentCustom(
				data = dataPlotByME[["TRUE"]],
				arrow = arrow(length = unit(1, "char")),
				show.legend = FALSE
			)
		
		# remove paramneters without data, set theme and labels
		gg <- gg +
			scale_y_discrete(drop = TRUE) +
			subjectProfileTheme() +
			labs(title = title, x = xLab, y = yLab)
	
		# color palette and name for color legend
		if(!is.null(colorVar))
			gg <- gg + getAesScaleManual(lab = colorLab, palette = colorPalette, type = "color")
		
		# set time limits for the x-axis
		if(!is.null(timeLim))
			gg <- gg + coord_cartesian(xlim = timeLim)
		
		attr(gg, 'metaData') <- list(subjectID = subject)
		
		class(gg) <- c("subjectProfileEventPlot", class(gg))
		
		gg

	})

	# metaData: stored plot label
	attr(listPlots, 'metaData') <- list(label = label)

	return(listPlots)
	
}