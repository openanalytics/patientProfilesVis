#' Visualize events in subject profiles, so
#' event with a single time.
#' @param colorVar String, variable of \code{data} with color.
#' This variable is used
#' for the colors and the filling of the points.
#' @param title String with title, label of the parameter variable by default.
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams filterData
#' @inheritParams getPageVar
#' @return list of (across subjects) of list (across modules) of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileEventPlot}, with additional metaData attributes containing
#' '\code{label}', 'timeLim' and 'timeTrans' (if specified).
#' @author Laure Cougnaud
#' @family patient profiles plotting function
#' @import ggplot2
#' @importFrom plyr dlply
#' @importFrom stats reorder
#' @importFrom clinUtils formatVarForPlotLabel
#' @importFrom clinUtils getLabelVar
#' @export
subjectProfileEventPlot <- function(
	data,
	paramVar, paramLab = getLabelVar(paramVar, labelVars = labelVars),
	paramVarSep = " - ",
	paramGroupVar = NULL,
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	shapeVar = colorVar, 
	shapeLab = if(isTRUE(colorVar == shapeVar)){
		colorLab
	}else	getLabelVar(shapeVar, labelVars = labelVars),
	shapePalette = NULL,
	alpha = 1,
	timeVar, timeLab = getLabelVar(timeVar, labelVars = labelVars),
	timeTrans = NULL, timeExpand = NULL,
	subjectVar = "USUBJID", subjectSubset = NULL, 
	subjectSample = NULL, seed = 123,
	subsetData = NULL, subsetVar = NULL, subsetValue = NULL,
	xLab = timeLab,
	yLab = "",
	timeLim = NULL,
	title = toString(getLabelVar(paramVar, labelVars = labelVars, label = paramLab)),
	label = title,
	labelVars = NULL,
	formatReport = subjectProfileReportFormat(),
	paging = TRUE
){
	
	# in case data is a tibble:
	data <- as.data.frame(data)
	
	# check if specified variable(s) are available in the data
	checkVar(var = subjectVar, data = data)
	checkVar(var = paramVar, data = data)
	checkVar(var = paramGroupVar, data = data)
	checkVar(var = timeVar, data = data)
	checkVar(var = colorVar, data = data)
	checkVar(var = shapeVar, data = data)
	
	# concatenate variable(s) if multiple are specified
	data[, "yVar"] <- interactionWithMissing(data = data, vars = paramVar, varSep = paramVarSep)
	
	# remove records with missing time
	data <- filterMissingInVar(
		data = data, 
		var = timeVar, varLab = timeLab
	)
	
	# only keep records of interest
	data <- filterData(
		data = data, 
		subsetData = subsetData,
		subsetVar = subsetVar, 
		subsetValue = subsetValue,
		subjectVar = subjectVar, 
		subjectSubset = subjectSubset,
		subjectSample = subjectSample, 
		seed = seed
	)
	
	# convert aesthetic variables to factor
	if(!is.null(colorVar)){
		data[, colorVar] <- convertAesVar(data, colorVar)
		if(is.null(colorPalette))	colorPalette <- getColorPalettePatientProfile(x = data[, colorVar])
	}else	colorPalette <- getColorPalettePatientProfile(n = 1)
	if(!is.null(shapeVar)){
		data[, shapeVar] <- convertAesVar(data, var = shapeVar)
		if(is.null(shapePalette))	shapePalette <- getShapePalettePatientProfile(x = data[, shapeVar])
	}
	
	# format variable
	data$yVar <- formatVarForPlotLabel(
		data = data, 
		paramVar = "yVar", 
		paramGroupVar = paramGroupVar,
		revert = TRUE, 
		width = formatReport$yLabelWidth
	)
	
	timeLim <- formatTimeLim(
		data = data, subjectVar = subjectVar, 
		timeStartVar = timeVar, timeEndVar = timeVar, timeLim = timeLim
	)
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
				
		subject <- unique(dataSubject[, subjectVar])
		
		# split plot into multiple page(s)
		dataSubject <- getPageVar(
			data = dataSubject, 
			var = "yVar", typeVar = "y",
			formatReport = formatReport,
			title = !is.null(title),
			xLab = !is.null(xLab),
			caption = FALSE,
			paging = paging
		)	
		
		listPlots <- dlply(dataSubject, "pagePlot", function(dataSubjectPage){
					
			aesArgs <- c(
				list(x = timeVar, y = "yVar"),
				if(!is.null(colorVar))	list(fill = colorVar, color = colorVar),
				if(!is.null(shapeVar))	list(shape = shapeVar)
			)
				
			# create the plot
			gg <- ggplot(data = dataSubjectPage) +
				geom_point(
					do.call(aes_string, aesArgs),
					size = 3, alpha = alpha
				) +
				scale_y_discrete(drop = TRUE) +
				subjectProfileTheme()
		
			if(!is.null(title))
				gg <- gg + ggtitle(title)
			
			if(!is.null(xLab))
				gg <- gg + xlab(xLab)
			
			if(!is.null(yLab))
				gg <- gg + ylab(yLab)
				
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
			
			# change name for color scale
			if(!is.null(shapeVar))
				gg <- gg + getAesScaleManual(lab = shapeLab, palette = shapePalette, type = "shape")
			
			argsScaleX <- c(
				if(!is.null(timeExpand))	list(expand = timeExpand),
				if(!is.null(timeTrans))	list(trans = timeTrans)
			)
			if(length(argsScaleX) > 0)
				gg <- gg + do.call("scale_x_continuous", argsScaleX)
			
			# set time limits for the x-axis
			# default: TRUE in case time limits are changed afterwards
			if(!is.null(timeLim)){
				timeLimSubject <- if(is.list(timeLim))	timeLim[[subject]]	else	timeLim
				gg <- gg + coord_cartesian(xlim = timeLimSubject, default = TRUE)
			}
			
			## extract number of lines
			
			# labels y-axis:
			nLines <- countNLines(unique(dataSubjectPage[, "yVar"]))
			nLinesPlot <- sum(nLines) + 0.8 * (length(nLines) - 1)
			
			# legend:
			nLinesLegendColor <- if(!is.null(colorVar)){
				getNLinesLegend(values = colorPalette, title = colorLab)
			}else	0
			nLinesLegendShape <- if(!is.null(shapeVar)){
				getNLinesLegend(values = shapePalette, title = shapeLab)
			}else	0
			nLinesLegend <- 0 +
				if(!is.null(colorVar) & !is.null(shapeVar)){
					# one legend (ggplot will create separate legend if the title differ)
					if(colorVar == shapeVar && !is.null(colorLab) & !is.null(shapeLab) && shapeLab == colorLab){
						nLinesLegendColor 
					# two legends
					}else{
						# 1 line to separate the two legends if color and shape are specified and different
						nLinesLegendColor + nLinesLegendShape + 1
					}
				}else	if(!is.null(colorVar)){
					nLinesLegendColor
				}else	if(!is.null(shapeVar)){
					nLinesLegendShape
				}else	0
			
			nLinesPlot <- max(sum(nLinesPlot), nLinesLegend)	
			
			# in title and axes
			nLinesTitleAndXAxis <- sum(c(
				getNLinesLabel(value = title, elName = "title"), 
				getNLinesLabel(value = xLab, elName = "x")
			))
			nLines <- nLinesPlot + nLinesTitleAndXAxis

			## set attributes
			
			attr(gg, 'metaData') <- list(subjectID = subject, nLines = nLines)
			class(gg) <- c("subjectProfileEventPlot", class(gg))
			
			gg
			
		})
		
	})

	# metaData:
	# stored plot label
	attr(listPlots, 'metaData') <- c(
		list(label = label, timeLim = timeLim),
		if(!is.null(timeTrans))	list(timeTrans = timeTrans),
		if(!is.null(timeExpand))	list(timeExpand = timeExpand)
	)
	
	return(listPlots)
	
}