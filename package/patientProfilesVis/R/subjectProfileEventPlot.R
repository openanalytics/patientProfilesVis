#' Create plot of subject profiles for events
#' 
#' 
#' @param timeVar string, variable of \code{data} with time
#' @param timeLab String, label for \code{timeVar}.
#' @param shapeVar string, variable of \code{data} for shape of the points
#' @param shapeLab string, label for \code{shapeVar}
#' @param shapePalette named vector with shape for \code{shapeVar}
#' @inheritParams subjectProfileIntervalPlot
#' @return list of (across subjects) of list (across modules) of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileEventPlot}, with additional metaData attributes containing
#' '\code{label}', 'timeLim' and and 'timeTrans' (if specified).
#' @author Laure Cougnaud
#' @family patient profiles plotting function
#' @import ggplot2
#' @importFrom plyr dlply
#' @importFrom stats reorder
#' @importFrom glpgUtilityFct formatVarForPlotLabel
#' @importFrom glpgUtilityFct getLabelVar
#' @export
subjectProfileEventPlot <- function(
	data,
	paramVar, paramLab = toString(getLabelVar(paramVar, labelVars = labelVars)),
	paramVarSep = " - ",
	paramGroupVar = NULL,
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	shapeVar = colorVar, shapeLab = getLabelVar(shapeVar, labelVars = labelVars),
	shapePalette = NULL,
	alpha = 1,
	timeVar, timeLab = getLabelVar(timeVar, labelVars = labelVars),
	timeTrans = NULL, timeExpand = NULL,
	subjectVar = "USUBJID", subjectSubset = NULL, 
	subjectSample = NULL, seed = 123,
	subsetData = NULL, subsetVar = NULL, subsetValue = NULL,
	xLab = getLabelVar(timeVar, labelVars = labelVars),
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
	
	checkVar(var = subjectVar, data = data)
	
	# concatenate variable(s) if multiple are specified
	dataParam <- data[, paramVar, drop = FALSE]
	data[, "yVar"] <- do.call(
		interaction, 
		c(as.list(dataParam), list(sep = paramVarSep, drop = TRUE, lex.order = TRUE))
	)
	
	# remove records without parameter or time variables
	isYMissing <- is.na(data[, "yVar"]) | data[, "yVar"] == ""
	if(any(isYMissing))
		message(paste(sum(isYMissing), "record(s) with missing", 
			toString(paramLab), "are not considered.")
		)
	isTimeMissing <- is.na(data[, timeVar])
	if(any(isTimeMissing))
		message(paste(sum(isTimeMissing), "record(s) with missing", 
			toString(timeLab), "are not considered.")
		)
	data <- data[!isYMissing & !isTimeMissing, ]
	
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
		if(is.null(colorPalette))	colorPalette <- getGLPGColorPalettePatientProfile(x = data[, colorVar])
	}else	colorPalette <- getGLPGColorPalettePatientProfile(n = 1)
	if(!is.null(shapeVar)){
		data[, shapeVar] <- convertAesVar(data, var = shapeVar)
		if(is.null(shapePalette))	shapePalette <- getGLPGShapePalettePatientProfile(x = data[, shapeVar])
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
				subjectProfileTheme() +
				labs(title = title, x = xLab, y = yLab)
			
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
			nLinesLegend <- 0 +
				if(!is.null(colorVar))	getNLinesLegend(values = colorPalette, title = colorLab) +
				if(!is.null(shapeVar))	getNLinesLegend(values = shapePalette, title = shapeLab) +
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