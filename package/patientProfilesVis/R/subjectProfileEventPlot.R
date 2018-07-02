#' Create plot of subject profiles for events
#' @param timeVar string, variable of \code{data} with time
#' @param shapeVar string, variable of \code{data} for shape of the points
#' @param shapeLab string, label for \code{shapeVar}
#' @inheritParams subjectProfileIntervalPlot
#' @return list of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileTextPlot}
#' @author Laure Cougnaud
#' @importFrom glpgUtility getLabelVar
#' @import ggplot2
#' @importFrom plyr dlply
#' @export
subjectProfileEventPlot <- function(
	data,
	paramVar, paramLab = getLabelVar(paramVar, labelVars = labelVars),
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	shapeVar = colorVar, shapeLab = getLabelVar(shapeVar, labelVars = labelVars),
	timeVar, 
	subjectVar = "USUBJID",
	xLab = getLabelVar(timeVar, labelVars = labelVars),
	yLab = "",
	timeLim = NULL,
	title = paramLab,
	labelVars = NULL
){
	
	data <- data[with(data, !is.na(get(paramVar)) & !is.na(get(timeVar))), ]
	
	if(!is.null(colorVar))	data <- data[!is.na(data[, colorVar]), ]
	if(!is.null(shapeVar))	data <- data[!is.na(data[, shapeVar]), ]
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
				
		aesArgs <- c(
			list(x = timeVar, y = paramVar),
			if(!is.null(colorVar))	list(color = colorVar),
			if(!is.null(shapeVar))	list(shape = shapeVar)
		)
			
		# create the plot
		gg <- ggplot(data = dataSubject) +
			geom_point(
				do.call(aes_string, aesArgs),
				size = 3
			) +
			scale_y_discrete(drop = TRUE) +
			theme_bw() +
			labs(title = title, x = xLab, y = yLab)
		
		# change name for color scale
		if(!is.null(colorLab))
			gg <- gg + scale_colour_discrete(name = colorLab)
		
		# change name for color scale
		if(!is.null(shapeLab))
			gg <- gg + scale_shape_discrete(name = shapeLab)
		
		if(!is.null(timeLim))
			gg <- gg + coord_cartesian(xlim = timeLim)
		
		class(gg) <- c("subjectProfileEventPlot", class(gg))
		
		gg
		
	})
	
	return(listPlots)
	
}