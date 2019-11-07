#' Create spaghetti plot for subject profiles
#' @param timeVar string, variable of \code{data} with time
#' @param paramValueVar string, variable of \code{data} with parameter value to represent
#' @param paramNameVar string, variable of \code{data} with parameter name
#' @param paramValueRangeVar character vector of length 2 containing variables of \code{data}
#' with minimum and maximum range for \code{paramValueVar},
#' e.g. to represent the reference range of the variable.
#' @param colorVar string, variable of \code{data} with color, used for the points only.
#' @param colorLab string, label for \code{colorVar}
#' @param colorPalette named vector with color for \code{colorVar}
#' @param shapeVar string, variable of \code{data} with shape, used for the points only.
#' @param shapeLab string, label for \code{shapeVar}
#' @param shapePalette named vector with shape for \code{shapeVar}
#' @inheritParams subjectProfileIntervalPlot
#' @return List of (across subjects) of list (across modules) 
#' of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileLinePlot}.
#' Each subject profile contains attributes: 'subjectID' and 'nLines' 
#' (estimated number of lines of space the plot will take).
#' The entire list also contains attributes: '\code{label}',
#' 'timeLim' and 'timeTrans' (if specified).
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom glpgStyle glpgColor
#' @importFrom plyr dlply
#' @importFrom glpgUtilityFct getLabelVar
#' @export
subjectProfileLinePlot <- function(
	data,
	paramValueVar, paramLab = toString(getLabelVar(paramValueVar, labelVars = labelVars)),
	paramNameVar = NULL, 
	paramValueRangeVar = NULL,
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	shapeVar = colorVar, shapeLab = getLabelVar(shapeVar, labelVars = labelVars),
	shapePalette = NULL,
	paramGroupVar = NULL,
	timeVar, timeTrans = NULL,
	subjectVar = "USUBJID", subjectSubset = NULL,
	subsetData = NULL, subsetVar = NULL, subsetValue = NULL, 
	xLab = getLabelVar(timeVar, labelVars = labelVars),
	yLab = "",
	timeLim = NULL,
	title = paramLab,
	label = title,
	labelVars = NULL,
	formatReport = subjectProfileReportFormat(),
	paging = TRUE
){
	
	
	# in case data is a tibble:
	data <- as.data.frame(data)
	
	data[, "yVar"] <- data[, paramValueVar]
	
	# remove records without parameter or time variables
	data <- data[with(data, !is.na(yVar) & yVar != "" & !is.na(get(timeVar))), ]
	
	# only keep records of interest
	data <- filterData(
		data = data, 
		subsetData = subsetData,
		subsetVar = subsetVar, 
		subsetValue = subsetValue,
		subjectVar = subjectVar, 
		subjectSubset = subjectSubset
	)
	
	# format variable
	data[, paramNameVar] <- formatParamVar(
		data = data, paramVar = paramNameVar, paramGroupVar = paramGroupVar,
		width = formatReport$yLabelWidth
	)
		
	# convert aesthetic variables to factor
	if(!is.null(colorVar)){
		data[, colorVar] <- convertAesVar(data, colorVar)
		if(is.null(colorPalette))	colorPalette <- getGLPGColorPalettePatientProfile(x = data[, colorVar])
	}else	colorPalette <- getGLPGColorPalettePatientProfile(n = 1)
	if(!is.null(shapeVar)){
		data[, shapeVar] <- convertAesVar(data, var = shapeVar)
		if(is.null(shapePalette))	shapePalette <- getGLPGShapePalettePatientProfile(x = data[, shapeVar])
	}
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
				
		# split plot into multiple page(s)
		dataSubject <- getPageVar(
			data = dataSubject, 
			var = paramNameVar, typeVar = "panel",
			formatReport = formatReport,
			title = !is.null(title),
			xLab = !is.null(xLab),
			caption = FALSE,
			paging = paging
		)
		
		subject <- unique(dataSubject[, subjectVar])
		
		listPlots <- dlply(dataSubject, "pagePlot", function(dataSubjectPage){
			
			# create the plot
			aesArgs <- list(x = timeVar, y = "yVar")
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
			
			# line
			dataLine <- if(!is.null(paramNameVar)){
				# remove rows with only one point (no need to connect points with the line)
				# to avoid warning: geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?
				# when 'facet_grid' is called
				nPointsPerParamName <- ddply(dataSubjectPage, paramNameVar, nrow)
				paramNameRetained <- subset(nPointsPerParamName, V1 > 1)[, paramNameVar]
				dataSubjectPage[which(dataSubjectPage[, paramNameVar] %in% paramNameRetained), ]
			}else	dataLine <- dataSubjectPage
			if(nrow(dataLine) > 0)
				gg <- gg + geom_line(data = dataLine)
			
			# point
			aesArgsPoint <- c(
				if(!is.null(colorVar))	list(color = colorVar, fill = colorVar),
				if(!is.null(shapeVar))	list(shape = shapeVar)
			)
			gg <- gg +
				if(length(aesArgsPoint) > 0){
					geom_point(do.call(aes_string, aesArgsPoint))
				}else geom_point()
			
			# general
			gg <- gg + 
				subjectProfileTheme() +
				labs(title = title, x = xLab, y = yLab) +
				theme(axis.text.y = element_text(size = 7))
			
			if(!is.null(paramNameVar)){
				
				gg <- gg + facet_grid(
					paste0(paramNameVar, "~."), 
					scales = "free_y", switch = "y"#,
#					labeller = label_wrap_gen(width = Inf)
					) +
					theme(
						strip.placement = "outside", 
						strip.text.y = element_text(
							angle = 180, size = 8, hjust = 1
						),
						strip.background = element_rect(color = NA, fill = NA)
					)
			
				# count number of lines each facet will take
				nLinesPlot <- countNLines(unique(dataSubjectPage[, paramNameVar]))
				nLinesPlot <- Vectorize(FUN = function(x){max(c(x, 4))})(nLinesPlot)
				
			}else	nLinesPlot <- 4
		
			# color palette and name for color legend
			if(!is.null(colorVar)){
				gg <- gg + 
					getAesScaleManual(lab = colorLab, palette = colorPalette, type = "color") +
					getAesScaleManual(lab = colorLab, palette = colorPalette, type = "fill")
			}else{
				gg <- gg + 
					scale_color_manual(values = colorPalette) +
					scale_fill_manual(values = colorPalette)
			}
		
			if(!is.null(shapeVar))
				gg <- gg + 
					getAesScaleManual(lab = shapeLab, palette = shapePalette, type = "shape")	
		
			if(!is.null(timeTrans))
				gg <- gg + scale_x_continuous(trans = timeTrans)
		
			# set time limits for the x-axis
			# default: FALSE in case time limits are changed afterwards
			if(!is.null(timeLim))
				gg <- gg + coord_cartesian(xlim = timeLim, default = TRUE)
		
			## extract number of lines
			
			# in legend
			nLinesLegend <- 0 +
				# for the color variable
				if(!is.null(colorVar))	getNLinesLegend(values = unique(dataSubjectPage[, colorVar]), title = colorLab) + 
				# for the shape variable
				if(!is.null(shapeVar))
					getNLinesLegend(values = unique(dataSubjectPage[, shapeVar]), title = shapeLab) + 
				# 1 line to separate the two legends if color and shape are specified and different
				# (ggplot will create separate legend if the title differ)
				if(!is.null(colorVar) & !is.null(shapeVar) && (colorVar != shapeVar || colorLab != shapeLab))	1

			nLinesPlot <- max(sum(nLinesPlot), nLinesLegend)
			
			# in title and axes
			nLinesTitleAndXAxis <- sum(c(
				getNLinesLabel(value = title, elName = "title"), 
				getNLinesLabel(value = xLab, elName = "x")
			))
			nLines <- nLinesPlot + nLinesTitleAndXAxis

			## set attributes
			
			attr(gg, 'metaData') <- list(subjectID = subject, nLines = nLines)
			class(gg) <- c("subjectProfileLinePlot", class(gg))
		
			gg
			
		})
		
	})

	# metaData:
	# stored plot label
	attr(listPlots, 'metaData') <- c(
		list(label = label, timeLim = timeLim),
		if(!is.null(timeTrans))	list(timeTrans = timeTrans)
	)
	
	return(listPlots)
	
}