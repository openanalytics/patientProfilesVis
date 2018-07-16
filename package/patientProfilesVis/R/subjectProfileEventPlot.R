#' Create plot of subject profiles for events
#' @param timeVar string, variable of \code{data} with time
#' @param shapeVar string, variable of \code{data} for shape of the points
#' @param shapeLab string, label for \code{shapeVar}
#' @param shapePalette named vector with shape for \code{shapeVar}
#' @inheritParams subjectProfileIntervalPlot
#' @return list of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileTextPlot}
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom plyr dlply
#' @export
subjectProfileEventPlot <- function(
	data,
	paramVar, paramLab = getLabelVar(paramVar, labelVars = labelVars),
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = if(!is.null(colorVar))	getPatientColorPalette(x = data[, colorVar]),
	shapeVar = colorVar, shapeLab = getLabelVar(shapeVar, labelVars = labelVars),
	shapePalette = if(!is.null(shapeVar))	getPatientShapePalette(x = data[, colorVar]),
	timeVar, 
	subjectVar = "USUBJID",
	xLab = getLabelVar(timeVar, labelVars = labelVars),
	yLab = "",
	timeLim = NULL,
	title = paramLab,
	label = title,
	labelVars = NULL
){
	
	data <- data[with(data, !is.na(get(paramVar)) & !is.na(get(timeVar))), ]
	
	if(!is.null(colorVar))	data <- data[!is.na(data[, colorVar]), ]
	if(!is.null(shapeVar))	data <- data[!is.na(data[, shapeVar]), ]
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
				
		aesArgs <- c(
			list(x = timeVar, y = paramVar),
			if(!is.null(colorVar))	list(fill = colorVar, color = colorVar),
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
		
		# color palette and name for color legend
		if(!is.null(colorVar)){
			paramsScale <- list(
				name = colorLab, 
				values = colorPalette, drop = FALSE,
				limits = names(colorPalette)
			)
			gg <- gg + 
				do.call(scale_fill_manual, paramsScale) +
				do.call(scale_color_manual, paramsScale)
		}
		
		# change name for color scale
		if(!is.null(shapeVar))
			gg <- gg + scale_shape_manual(
				name = shapeLab, 
				values = shapePalette, drop = FALSE,
				limits = names(shapePalette)
			)
		
		if(!is.null(timeLim))
			gg <- gg + coord_cartesian(xlim = timeLim)
		
		class(gg) <- c("subjectProfileEventPlot", class(gg))
		
		gg
		
	})

	# metaData:
	# stored plot label
	attr(listPlots, 'metaData') <- list(label = label)
	
	return(listPlots)
	
}