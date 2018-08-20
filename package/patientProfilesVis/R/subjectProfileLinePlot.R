#' Create spaghetti plot for subject profiles
#' @param timeVar string, variable of \code{data} with time
#' @param paramValueVar string, variable of \code{data} with parameter value to represent
#' @param paramNameVar string, variable of \code{data} with parameter name
#' @param paramValueRangeVar character vector of length 2 containing variables of \code{data}
#' with minimum and maximum range for \code{paramValueVar},
#' e.g. to represent the reference range of the variable.
#' @inheritParams subjectProfileIntervalPlot
#' @return list of (across subjects) of list (across modules) of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileLinePlot}, with additional metaData attributes containing
#' 'label' and 'timeLim'.
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom glpgStyle glpgColor
#' @importFrom plyr dlply
#' @export
subjectProfileLinePlot <- function(
	data,
	paramValueVar, paramLab = toString(getLabelVar(paramValueVar, labelVars = labelVars)),
	paramNameVar = NULL,
	paramValueRangeVar = NULL,
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	paramGroupVar = NULL,
	timeVar, 
	subjectVar = "USUBJID",
	subsetVar = NULL, subsetValue = NULL,
	xLab = getLabelVar(timeVar, labelVars = labelVars),
	yLab = "",
	timeLim = NULL,
	title = paramLab,
	label = title,
	labelVars = NULL,
	formatReport = subjectProfileReportFormat()
){
	
	data[, "yVar"] <- data[, paramValueVar]
	
	# remove records without parameter or time variables
	data <- data[with(data, !is.na(yVar) & yVar != "" & !is.na(get(timeVar))), ]
	
	# only keep records of interest
	data <- filterData(data, 
		subsetVar = subsetVar, 
		subsetValue = subsetValue
	)
	
	# format variable
	data[, paramNameVar] <- formatParamVar(
		data = data, paramVar = paramNameVar, paramGroupVar = paramGroupVar
	)
		
	# convert aesthetic variables to factor
	if(!is.null(colorVar)){
		data[, colorVar] <- convertAesVar(data, colorVar)
		if(is.null(colorPalette))	colorPalette <- getPatientColorPalette(x = data[, colorVar])
	}
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
				
		# split plot into multiple page(s)
		dataSubject <- getPageVar(
			data = dataSubject, 
			var = paramNameVar, typeVar = "panel",
			formatReport = formatReport,
			title = !is.null(title),
			xLab = !is.null(xLab),
			caption = FALSE
		)
		
		subject <- unique(dataSubject[, subjectVar])
		
		listPlots <- dlply(dataSubject, "pagePlot", function(dataSubjectPage){
					
			aesArgs <- c(
				list(x = timeVar, y = "yVar"),
				if(!is.null(colorVar))	list(color = colorVar, group = colorVar)	else	list(group = 1)
			)
			
			# create the plot
			gg <- ggplot(data = dataSubjectPage, do.call(aes_string, aesArgs))
			
			# range of the variable
			if(!is.null(paramValueRangeVar)){
				if(length(paramValueRangeVar) != 2)
					stop(paste("The range of the parameter ('paramValueRangeVar' parameter)",
						"should be specified by two variables in the dataset."))
				# use geom_ribbon instead of geom_rect in case different intervals for different time points
				gg <- gg + 
					geom_ribbon(
						aes_string(x = timeVar, 
							ymin = paramValueRangeVar[1], ymax = paramValueRangeVar[2]
						),
						fill = unname(glpgColor("extra")["lightGreen"]), alpha = 0.1
					)
			}
			
			# base plot
			gg <- gg + geom_point() + geom_line() +
				subjectProfileTheme() +
				labs(title = title, x = xLab, y = yLab) +
				theme(axis.text.y = element_text(size = 7))
			
			if(!is.null(paramNameVar))
				gg <- gg + facet_grid(paste0(paramNameVar, "~."), 
					scales = "free_y", switch = "y",
					labeller = label_wrap_gen(width = Inf)) +
					theme(
						strip.placement = "outside", 
						strip.text.y = element_text(
							angle = 180, size = 8, hjust = 1
						),
						strip.background = element_rect(color = NA, fill = NA)
					)
		
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
		
	})

	# metaData:
	# stored plot label
	attr(listPlots, 'metaData') <- list(label = label, timeLim = timeLim)
	
	return(listPlots)
	
}