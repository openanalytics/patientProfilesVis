#' Create spaghetti plot for subject profiles
#' @param timeVar string, variable of \code{data} with time
#' @param paramValueVar string, variable of \code{data} with parameter value to represent
#' @param paramNameVar string, variable of \code{data} with parameter name
#' @inheritParams subjectProfileIntervalPlot
#' @return list of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileTextPlot}
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom plyr dlply
#' @export
subjectProfileLinePlot <- function(
	data,
	paramValueVar, paramLab = toString(getLabelVar(paramValueVar, labelVars = labelVars)),
	paramNameVar = NULL,
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	paramGroupVar = NULL,
	timeVar, 
	subjectVar = "USUBJID",
	xLab = getLabelVar(timeVar, labelVars = labelVars),
	yLab = "",
	timeLim = NULL,
	title = paramLab,
	label = title,
	labelVars = NULL
){
	
	data[, "yVar"] <- data[, paramValueVar]
	
	data <- data[with(data, !is.na(yVar) & yVar != "" & !is.na(get(timeVar))), ]
	
	# if paramGroupVar is specified: change order levels of 'variable'
	if(!is.null(paramGroupVar)){
		groupVariable <- if(length(paramGroupVar) > 0){
			interaction(data[, paramGroupVar])
		}else data[, paramGroupVar]
		data[, "yVar"] <- reorder(data[, "yVar"], groupVariable, unique)
	}
	
	# convert aesthetic variables to factor
	if(!is.null(colorVar)){
		data[, colorVar] <- convertAesVar(data, colorVar)
		if(is.null(colorPalette))	colorPalette <- getPatientColorPalette(x = data[, colorVar])
	}
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
		
		subject <- unique(dataSubject[, subjectVar])
				
		aesArgs <- c(
			list(x = timeVar, y = "yVar"),
			if(!is.null(colorVar))	list(color = colorVar)
		)
			
		# create the plot
		gg <- ggplot(data = dataSubject) +
			geom_point(do.call(aes_string, aesArgs)) +
			geom_line(do.call(aes_string, aesArgs)) +
			subjectProfileTheme() +
			labs(title = title, x = xLab, y = yLab) +
			theme(
				strip.text.y = element_text(size = 6),
				axis.text.y = element_text(size = 5)
			)
		
		if(!is.null(paramNameVar))
			gg <- gg + facet_grid(paste0(paramNameVar, "~."), 
				scales = "free_y", switch = "y",
				labeller = label_wrap_gen(width = 8)) +
				theme(strip.placement = "outside")
	
		# color palette and name for color legend
		if(!is.null(colorVar))
			gg <- gg + 
				getAesScaleManual(lab = colorLab, palette = colorPalette, type = "color")		
	
		if(!is.null(timeLim))
			gg <- gg + coord_cartesian(xlim = timeLim)
		
		attr(gg, 'metaData') <- list(subjectID = subject)
		
		class(gg) <- c("subjectProfileLinePlot", class(gg))
		
		gg
		
	})

	# metaData:
	# stored plot label
	attr(listPlots, 'metaData') <- list(label = label)
	
	return(listPlots)
	
}