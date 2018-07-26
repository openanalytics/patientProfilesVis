#' Create plot of subject profiles with segments for range of parameters 
#' @param data data.frame with data
#' @param paramVar string, variable of \code{data} with parameter (used in the y-axis)
#' @param paramGroupVar (optional) character vector with variable(s) of \code{data} with grouping.
#' If specified, the parameters will be grouped by this(these) variable(s) in the y-axis.
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
	timeLim =  with(data, c(min(get(timeStartVar), na.rm = TRUE), max(get(timeStartVar), na.rm = TRUE))),
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
	# TODO per subject
	idxMissingEnd <- is.na(data[, timeEndVar])
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
	
	data[, "yVar"] <- if(length(paramVar) > 1)
		apply(data[, paramVar], 1, paste, collapse = " ")	else	data[, paramVar]
	
	# if paramGroupVar is specified: change order levels of 'variable'
	if(!is.null(paramGroupVar)){
		groupVariable <- if(length(paramGroupVar) > 0){
			interaction(data[, paramGroupVar])
		}else data[, paramGroupVar]
		data[, "yVar"] <- reorder(data[, "yVar"], groupVariable, unique)
	}
	
	# convert color variable to factor
	if(!is.null(colorVar)){
		data[, colorVar] <- convertAesVar(data, colorVar)
		if(is.null(colorPalette))	colorPalette <- getPatientColorPalette(x = data[, colorVar])
	}
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
		
		subject <- unique(dataSubject[, subjectVar])
				
		aesArgs <- c(
			list(x = timeStartVar, xend = timeEndVar, y = "yVar", yend = "yVar"),
			if(!is.null(colorVar))	list(color = colorVar)
		)
				
		# create the plot
		gg <- ggplot(data = dataSubject) +
			geom_segment(
				do.call(aes_string, aesArgs),
				size = 2
			) +
			scale_y_discrete(drop = TRUE) +
			subjectProfileTheme() +
			labs(title = title, x = xLab, y = yLab)
	
		# color palette and name for color legend
		if(!is.null(colorVar))
			gg <- gg + getAesScaleManual(lab = colorLab, palette = colorPalette, type = "color")
		
		if(!is.null(timeLim))
			gg <- gg + coord_cartesian(xlim = timeLim)
		
		attr(gg, 'metaData') <- list(subjectID = subject)
		
		class(gg) <- c("subjectProfileEventPlot", class(gg))
		
		gg

	})

	# metaData:
	# stored plot label
	attr(listPlots, 'metaData') <- list(label = label)

	return(listPlots)
	
}