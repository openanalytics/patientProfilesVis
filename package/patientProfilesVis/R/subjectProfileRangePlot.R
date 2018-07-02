#' Create plot of subject profiles with segments for range of parameters 
#' @param data data.frame with data
#' @param paramVar string, variable of \code{data} with parameter (used in the y-axis)
#' @param paramLab string, label for \code{paramVar}
#' @param subjectVar string, variable of \code{data} with subject ID
#' @param startVar string, variable of \code{data} with start of range
#' @param endVar string, variable of \code{data} with end of range
#' @param timeLim (optional) vector of length 2 with time limits (x-axis)
#' If not specified, extracted from the minimum \code{startVar} and maximum \code{endVar}
#' @param rangeSimilarStartEnd numeric, if a record has the same
#' \code{startVar} and \code{endVar}, what should be the range of the segment?
#' By default, a thousandth of the range of \code{timeLim}.
#' @param xLab string, label for the x-axis
#' @param yLab string, label for the y-axis
#' @param colorVar string, variable of \code{data} with color
#' @param colorLab string, label for \code{colorVar}
#' @param title string, title for the plot
#' @inheritParams glpgUtility::getLabelVar
#' @return list of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileTextPlot}
#' @author Laure Cougnaud
#' @importFrom glpgUtility getLabelVar
#' @import ggplot2
#' @importFrom plyr dlply
#' @export
subjectProfileIntervalPlot <- function(
	data,
	paramVar, paramLab = getLabelVar(paramVar, labelVars = labelVars),
	startVar,
	endVar,
	subjectVar = "USUBJID",
	timeLim = with(data, c(min(get(startVar), na.rm = TRUE), max(get(startVar), na.rm = TRUE))),
	rangeSimilarStartEnd = diff(timeLim)/1000,
	xLab = paste(getLabelVar(c(startVar, endVar), labelVars = labelVars), collapse = "/"),
	yLab = "",
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	title = paramLab,
	labelVars = NULL
){
	
	# filter records without start date
	idxMissingStart <- is.na(data[, startVar])
	if(any(idxMissingStart))
		data <- data[-which(idxMissingStart), ]	
	
	# if no end date: take last time in dataset
	idxMissingEnd <- is.na(data[, endVar])
	if(any(idxMissingEnd))
		data[idxMissingEnd, endVar] <- timeLim[2]
	
	# if same start/end, data not included by geom_segment
	# so jitter the start/end in this case (proportion of the total x-range)
	idxSameStartEnd <- which(data[, startVar] == data[, endVar])
	if(length(idxSameStartEnd) > 0){
		data[idxSameStartEnd, c(startVar, endVar)] <-
			sweep(
				x = data[idxSameStartEnd, c(startVar, endVar)], 
				MARGIN = 2, 
				STATS = c(-1, 1) * rangeSimilarStartEnd/2,
				FUN = "+"
			)
	}
	
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
		if(!is.null(colorLab))
			gg <- gg + scale_colour_discrete(name = colorLab)
		
		gg <- gg + coord_cartesian(xlim = timeLim)
		
		class(gg) <- c("subjectProfileEventPlot", class(gg))
		
		gg

	})

	return(listPlots)
	
}