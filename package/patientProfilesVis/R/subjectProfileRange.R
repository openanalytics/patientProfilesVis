#' Create plot of subject profiles with segments for range of parameters 
#' @param data data.frame with data
#' @param paramVar variable of \code{data} with parameter (used in the y-axis)
#' @param paramLab label for \code{paramVar}
#' @param subjectVar variable of \code{data} with subject ID
#' @param startVar variable of \code{data} with start of range
#' @param endVar variable of \code{data} with end of range
#' @param rangeLim vector of length 2 with limits for the range (x-axis).
#' If not specified, extracted from the minimum \code{startVar} and maximum \code{endVar}
#' @param xLab label for the x-axis
#' @param yLab label for the y-axis
#' @param colorVar variable of \code{data} with color
#' @param colorLab label for \code{colorVar}
#' @param title title for the plot
#' @inheritParams glpgUtility::getLabelVar
#' @return list of \code{\link[ggplot2]{ggplot2} objects}
#' @author Laure Cougnaud
#' @importFrom glpgUtility getLabelVar
#' @import ggplot2
#' @importFrom plyr dlply
#' @export
subjectProfileRangePlot <- function(
	data,
	paramVar, paramLab = getLabelVar(paramVar, labelVars = labelVars),
	subjectVar,
	startVar,
	endVar,
	rangeLim = with(data, c(min(get(startVar), na.rm = TRUE), max(get(startVar), na.rm = TRUE))),
	xLab = paste(getLabelVar(c(startVar, endVar), labelVars = labelVars), collapse = "/"),
	yLab = "",
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	title = paramLab,
	labelVars = NULL
){
	
	# if no end date: take last time in dataset
	idxMissingEnd <- is.na(data[, endVar])
	if(any(idxMissingEnd))
		data[idxMissingEnd, endVar] <- rangeLim[2]
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
		
		aesArgs <- c(
			list(x = startVar, xend = endVar, y = paramVar, yend = paramVar),
			if(!is.null(colorVar))	list(color = colorVar)
		)
				
		# create the plot
		gg <- ggplot(data = dataSubject) +
			geom_segment(
				do.call(aes_string, aesArgs),
				size = 3
			) +
			scale_y_discrete(drop = TRUE) +
			theme_bw() +
			labs(title = title, x = xLab, y = yLab)
	
		# change name for color scale
		if(!is.null(colorVar))
			gg <- gg + scale_colour_discrete(name = colorLab)

	})

	return(listPlots)
	
}