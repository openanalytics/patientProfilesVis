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
#' @importFrom stats reorder
#' @export
subjectProfileEventPlot <- function(
	data,
	paramVar, paramLab = toString(getLabelVar(paramVar, labelVars = labelVars)),
	paramGroupVar = NULL,
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	shapeVar = colorVar, shapeLab = getLabelVar(shapeVar, labelVars = labelVars),
	shapePalette = NULL,
	timeVar, 
	subjectVar = "USUBJID",
	xLab = getLabelVar(timeVar, labelVars = labelVars),
	yLab = "",
	timeLim = NULL,
	title = paramLab,
	label = title,
	labelVars = NULL
){
	
	# concatenate variable(s) if multiple are specified
	data[, "yVar"] <- if(length(paramVar) > 1){
		apply(data[, paramVar], 1, paste, collapse = " ")
	}else{
		data[, paramVar]
	}
	
	# remove records without parameter or time variables
	data <- data[with(data, !is.na(yVar) & yVar != "" & !is.na(get(timeVar))), ]
	
	# convert aesthetic variables to factor
	if(!is.null(colorVar)){
		data[, colorVar] <- convertAesVar(data, colorVar)
		if(is.null(colorPalette))	colorPalette <- getPatientColorPalette(x = data[, colorVar])
	}
	if(!is.null(shapeVar)){
		data[, shapeVar] <- convertAesVar(data, var = shapeVar)
		if(is.null(shapePalette))	shapePalette <- getPatientShapePalette(x = data[, shapeVar])
	}
	
	# format variable
	data$yVar <- formatParamVar(
		data = data, paramVar = "yVar", paramGroupVar = paramGroupVar,
		revert = TRUE
	)
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
		
		subject <- unique(dataSubject[, subjectVar])
				
		aesArgs <- c(
			list(x = timeVar, y = "yVar"),
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
			subjectProfileTheme() +
			labs(title = title, x = xLab, y = yLab)
		
		# color palette and name for color legend
		if(!is.null(colorVar))
			gg <- gg + 
				getAesScaleManual(lab = colorLab, palette = colorPalette, type = "color") +
				getAesScaleManual(lab = colorLab, palette = colorPalette, type = "fill")
		
		# change name for color scale
		if(!is.null(shapeVar))
			gg <- gg + getAesScaleManual(lab = shapeLab, palette = shapePalette, type = "shape")
		
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