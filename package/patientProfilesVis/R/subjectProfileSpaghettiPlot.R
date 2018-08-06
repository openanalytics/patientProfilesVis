#' Create spaghetti plot for subject profiles
#' @param timeVar string, variable of \code{data} with time
#' @param facetVar string, variable used for facetting
#' @inheritParams subjectProfileIntervalPlot
#' @return list of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileTextPlot}
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom plyr dlply
#' @export
subjectProfileSpaghettiPlot <- function(
	data,
	paramVar, paramLab = toString(getLabelVar(paramVar, labelVars = labelVars)),
	facetVar = NULL,
	timeVar, 
	subjectVar = "USUBJID",
	xLab = getLabelVar(timeVar, labelVars = labelVars),
	yLab = "",
	timeLim = NULL,
	title = paramLab,
	label = title,
	labelVars = NULL
){
	
	
	data[, "yVar"] <- data[, paramVar]
	
	data <- data[with(data, !is.na(yVar) & yVar != "" & !is.na(get(timeVar))), ]
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
		
		subject <- unique(dataSubject[, subjectVar])
				
		aesArgs <- c(
			list(x = timeVar, y = paramVar)
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
		
		if(!is.null(facetVar))
			gg <- gg + facet_grid(paste0(facetVar, "~."), 
				scales = "free_y", switch = "y",
				labeller = label_wrap_gen(width = 8)) +
				theme(strip.placement = "outside")
	
		if(!is.null(timeLim))
			gg <- gg + coord_cartesian(xlim = timeLim)
		
		attr(gg, 'metaData') <- list(subjectID = subject)
		
		class(gg) <- c("subjectProfileSpaghettiPlot", class(gg))
		
		gg
		
	})

	# metaData:
	# stored plot label
	attr(listPlots, 'metaData') <- list(label = label)
	
	return(listPlots)
	
}