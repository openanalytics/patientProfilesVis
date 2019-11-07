#' Create plot of subject profiles with segments for range of parameters 
#' If no start and/or end date is available, specific arrows are created.
#' @inherit formatTimeInterval details
#' @param paramVar string, variable of \code{data} with parameter (used in the y-axis)
#' @param paramLab string, label for \code{paramVar}
#' @param rangeSimilarStartEnd numeric, if a record has the same
#' \code{timeStartVar} and \code{timeEndVar}, what should be the range of the segment?
#' By default, a thousandth of the range of \code{timeLim}
#' (if not specified the time range available in the data).
#' @param xLab string, label for the x-axis
#' @param yLab string, label for the y-axis
#' @param colorVar string, variable of \code{data} with color.
#' @param colorLab string, label for \code{colorVar}
#' @param colorPalette named vector with color for \code{colorVar}
#' @param title string, title for the plot
#' @param label string, label used in the plot.
#' This label is used to report the name of the panel 
#' in the text when the plots are combined (e.g. if the plot is empty).
#' @param labelVars named string with variable labels (names are the variable code)
#' @param paramVarSep string with character(s) used to concatenate multiple 
#' \code{paramVar}, ' - ' by default.
#' @param timeLabel string with general time label, used
#' in the footnote for the explanation of the arrow, 'time' by default.
#' @param timeStartShapeVar,timeEndShapeVar (optional) string with
#' names of the variables in \code{data} used for symbol shape
#' for \code{timeStartVar}/\code{timeEndVar}.
#' @param shapePalette Named vector with shape for \code{timeStartShapeVar}\code{timeEndShapeVar}.
#' @param shapeLab String with label for \code{timeStartShapeVar}\code{timeEndShapeVar}
#' @param shapeSize Size for symbols (only used if \code{timeStartShapeVar}/\code{timeEndShapeVar} is specified).
#' @param timeAlign Logical, if TRUE (by default)
#' the different plots are horizontally aligned.
#' If set to FALSE, each plot has its own time-limits.
#' @param timeTrans ggplot2 transformation (see \code{\link[scales]{trans_new}}),
#' e.g. produced by the \code{\link{getTimeTrans}} function.
#' @inheritParams filterData
#' @inheritParams formatParamVar
#' @inheritParams formatTimeInterval
#' @inheritParams getPageVar
#' @return list of (across subjects) of list (across modules) of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileIntervalPlot}, with additional metaData attributes containing
#' '\code{label}' and 'timeLim' and 'timeTrans' (if specified).
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom plyr dlply
#' @importFrom stats reorder
#' @importFrom glpgUtilityFct getLabelVar
#' @export
subjectProfileIntervalPlot <- function(
	data,
	paramVar, paramVarSep = " - ",
	paramLab = toString(getLabelVar(paramVar, labelVars = labelVars)),
	paramGroupVar = NULL,
	timeStartVar,
	timeEndVar,
	timeLabel = "time",
	subjectVar = "USUBJID", subjectSubset = NULL,
	subsetData = NULL, subsetVar = NULL, subsetValue = NULL, 
	timeLim = NULL, timeLimData = NULL, timeLimStartVar = NULL, timeLimEndVar = NULL,
	timeTrans = NULL,
	timeAlign = TRUE,
	rangeSimilarStartEnd = NULL,
	xLab = paste(getLabelVar(c(timeStartVar, timeEndVar), labelVars = labelVars), collapse = "/"),
	yLab = "",
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	timeStartShapeVar = NULL, timeEndShapeVar = NULL,
	shapePalette = NULL, 
	shapeLab = toString(unique(getLabelVar(c(timeStartShapeVar, timeEndShapeVar), labelVars = labelVars))),
	shapeSize = rel(1),
	title = paramLab,
	label = title,
	labelVars = NULL,
	formatReport = subjectProfileReportFormat(),
	paging = TRUE
){
	
	# in case data is a tibble:
	data <- as.data.frame(data)
		
	# fill missing start/end time and extract time limits
	resMSED <- formatTimeInterval(
		data = data, 
		timeStartVar = timeStartVar, timeEndVar = timeEndVar, 
		subjectVar = subjectVar,
		timeLim = timeLim, timeLimData = timeLimData, 
		timeLimStartVar = timeLimStartVar, timeLimEndVar = timeLimEndVar
	)
	data <- resMSED$data
	timeLim <- resMSED$timeLim
	timeLimInit <- resMSED$timeLimSpecified
	
	# specify the time limits if not specified
	# otherwise if missing values for start/end for all records of a patient
	# 'segment' span entire plotting window
	
	# if same start/end, data not included by geom_segment
	# so jitter the start/end in this case (proportion of the total x-range)
	idxSameStartEnd <- which(data[, timeStartVar] == data[, timeEndVar])
	if(length(idxSameStartEnd) > 0){
		if(is.null(rangeSimilarStartEnd))
			rangeSimilarStartEnd <- diff(timeLim)/1000
		data[idxSameStartEnd, c(timeStartVar, timeEndVar)] <-
			sweep(
				x = data[idxSameStartEnd, c(timeStartVar, timeEndVar)], 
				MARGIN = 2, 
				STATS = c(-1, 1) * rangeSimilarStartEnd/2,
				FUN = "+"
			)
	}
	
	# concatenate variable(s) if multiple are specified
	data$yVar <- if(length(paramVar) > 1)
		apply(data[, paramVar], 1, paste, collapse = paramVarSep)	else	data[, paramVar]

	# remove records without parameter variable
	data <- data[with(data, !is.na(yVar) & yVar != ""), ]
	
	# only keep records of interest
	data <- filterData(
		data = data, 
		subsetVar = subsetVar, 
		subsetValue = subsetValue,
		subsetData = subsetData,
		subjectVar = subjectVar, 
		subjectSubset = subjectSubset
	)

	# if paramGroupVar is specified: change order levels of 'variable'
	data$yVar <- formatParamVar(
		data = data, paramVar = "yVar", paramGroupVar = paramGroupVar,
		revert = TRUE, width = formatReport$yLabelWidth
	)
	
	# convert color variable to factor
	if(!is.null(colorVar)){
		data[, colorVar] <- convertAesVar(data, colorVar)
		if(is.null(colorPalette))	colorPalette <- getGLPGColorPalettePatientProfile(x = data[, colorVar])
	}else{
		if(is.null(colorPalette))	colorPalette <- getGLPGColorPalettePatientProfile(n = 1)
	}
	
	hasShapeVar <- !is.null(timeStartShapeVar) | !is.null(timeEndShapeVar)
	if(hasShapeVar){
		if(!is.null(timeStartShapeVar))
			data[, timeStartShapeVar] <- convertAesVar(data, timeStartShapeVar)
		if(!is.null(timeEndShapeVar))
			data[, timeEndShapeVar] <- convertAesVar(data, timeEndShapeVar)
		if(is.null(shapePalette)){
			shapes <- unlist(lapply(data[, c(timeStartShapeVar, timeEndShapeVar)], levels))
			shapePalette <- getGLPGShapePalettePatientProfile(x = shapes)
		}
	}
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
						
		subject <- unique(dataSubject[, subjectVar])
		
		# split plot into multiple page(s)
		dataSubject <- getPageVar(
			data = dataSubject, 
			var = "yVar", typeVar = "y",
			formatReport = formatReport,
			title = !is.null(title),
			xLab = !is.null(xLab),
			caption = TRUE,
			paging = paging
		)
		
		listPlots <- dlply(dataSubject, "pagePlot", function(dataSubjectPage){
			
			aesArgs <- c(
				list(
					x = timeStartVar, xend = timeEndVar, 
					y = "yVar", yend = "yVar"#,
	#			linetype = "missingStartEndPlot"
				),
				if(!is.null(colorVar))	list(color = colorVar)
			)
			
			# build aesthetic
			# and custom geom_segment
			geomSegmentCustom <- function(..., show.legend = FALSE){
				geom_segment(
					do.call(aes_string, aesArgs),
					size = 2, show.legend = show.legend, ...
				) 
			}
					
			# create the plot
			gg <- ggplot()
		
			## plot segments
	
			# records with start/end date
			# and for records with missing start and/or date: plot segment to have color legend without segment
			gg <- gg + geomSegmentCustom(data = dataSubjectPage, show.legend = TRUE)	
			
			if(hasShapeVar){
				
				geomPointCustom <- function(gg, xVar, shapeVar){			
					aesPC <- c(
						list(x = xVar, y = "yVar", shape = shapeVar), 
						if(!is.null(colorVar))	list(color = colorVar)
					)
					gg + geom_point(
						data = dataSubjectPage, 
						mapping = do.call(aes_string, aesPC), 
						fill = "white",
						size = shapeSize,
						position = position_nudge(y = -0.01)
					)
				}
				
				if(!is.null(timeStartShapeVar))
					gg <- geomPointCustom(gg, xVar = timeStartVar, shapeVar = timeStartShapeVar)
				if(!is.null(timeEndShapeVar))
					gg <- geomPointCustom(gg, xVar = timeEndVar, shapeVar = timeEndShapeVar)
				
				if(!is.null(shapePalette))
					gg <- gg + getAesScaleManual(lab = shapeLab, palette = shapePalette, type = "shape")
				
				# lines are included in shape legend
				gg <- gg + guides(shape = guide_legend(override.aes = list(linetype = NA)))
				
				caption <- NULL
				
			}else{
				
				# separate data versus different arrows
				# Note: in case of missing end date
				# right-directed triangle not available in shape palette
				# option 1: use Unicode, but symbol not centered
				# option 2: draw segment
				dataPlotByME <- dlply(dataSubjectPage, c("missingStartPlot", "missingEndPlot"))
			
				# records with start date and missing end date
				if("FALSE.TRUE" %in% names(dataPlotByME))
					gg <- gg + geomSegmentCustom(
						data = dataPlotByME[["FALSE.TRUE"]],
						arrow = arrow(length = unit(1, "char"), ends = "last")
					)
				
				# records with missing start date but with end date
				if("TRUE.FALSE" %in% names(dataPlotByME)){
					gg <- gg + geomSegmentCustom(
						data = dataPlotByME[["TRUE.FALSE"]],
						arrow = arrow(length = unit(1, "char"), ends = "first")
					)
				}
				
				# records with missing start and end dates
				if("TRUE.TRUE" %in% names(dataPlotByME))
					gg <- gg + geomSegmentCustom(
						data = dataPlotByME[["TRUE.TRUE"]],
						arrow = arrow(length = unit(1, "char"), ends = "both")
					)
				
				
				# set labels for linetype in legend
				#		linetypeVals <- c(
				#			'TRUE.TRUE' = "dotted", 'FALSE.TRUE' = "dashed", 
				#			'TRUE.FALSE' = "longdash", 'FALSE.FALSE' = "solid"
				#		)
				#		linetypeLabels <- c(
				#			'TRUE.TRUE' = "Missing start/end", 
				#			'FALSE.TRUE' = "Missing end", 
				#			'TRUE.FALSE' = "Missing start", 
				#			'FALSE.FALSE' = "No missing start/end"
				#		)
				#		linetypeLims <- c('TRUE.TRUE', 'FALSE.TRUE', 'TRUE.FALSE', 'FALSE.FALSE')
				#		linetypeLims <- linetypeLims[linetypeLims %in% unique(dataSubject$missingStartEnd)]
				#		gg <- gg + 
				#			scale_linetype_manual(
				#				name = "Missing time", 
				#				values = linetypeVals[linetypeLims], 
				#				limits = linetypeLims,
				#				labels = linetypeLabels[linetypeLims]
				#			)
				
				# remove paramneters without data, set theme and labels
				caption <- paste0("Arrow represents missing start/end ", timeLabel, ".")
			
			}
			
			gg <- gg +
				scale_y_discrete(drop = TRUE) +
				subjectProfileTheme() +
				labs(title = title, 
					x = xLab, y = yLab,
					caption = caption
				) + theme(plot.caption = element_text(hjust = 0.5))
		
			# color palette and name for color legend
			if(!is.null(colorVar)){
				gg <- gg + getAesScaleManual(lab = colorLab, palette = colorPalette, type = "color") +
					guides(color = guide_legend(override.aes = list(shape = NA)))
			}else	gg <- gg + scale_color_manual(values = colorPalette)
			
			if(!is.null(timeTrans))
				gg <- gg + scale_x_continuous(trans = timeTrans)
					
			# set time limits for the x-axis
			# default: FALSE in case time limits are changed afterwards
			if(!is.null(timeLim) & timeAlign)
				gg <- gg + coord_cartesian(xlim = timeLim, default = TRUE)

			# to deal with custom shape (e.g. partial dates)
			# use geom_point
			
			## extract number of lines
			
			# plot content
			nLines <- countNLines(unique(dataSubjectPage[, "yVar"]))
			nLinesPlot <- sum(nLines) + 0.8 * (length(nLines) - 1)
			
			# legend:

			nLinesLegend <- 0
			# for the color variable
			if(!is.null(colorVar))
				nLinesLegend <- getNLinesLegend(
					values = unique(dataSubjectPage[, colorVar]), 
					title = colorLab
				)
			if(hasShapeVar){
				shapes <- unique(unlist(dataSubjectPage[, c(timeStartShapeVar, timeEndShapeVar)]))
				nLinesLegend <- nLinesLegend +
					getNLinesLegend(
						values = shapes, 
						title = shapeLab
				)
			}
			nLinesPlot <- max(nLinesPlot, nLinesLegend)
			
			# in title and axes
			nLinesTitleAndXAxis <- sum(c(
				getNLinesLabel(value = title, elName = "title"), 
				getNLinesLabel(value = xLab, elName = "x"),
				getNLinesLabel(value = caption, elName = "caption")
			))
			nLines <- nLinesPlot + nLinesTitleAndXAxis
			
			## set attributes
			attr(gg, 'metaData') <- list(subjectID = subject, nLines = nLines)
			class(gg) <- c("subjectProfileEventPlot", class(gg))
			
			gg
			
		})

	})

	# metaData: stored plot label
	attr(listPlots, 'metaData') <- c(
		list(label = label, timeLim = timeLimInit),
		if(!is.null(timeTrans))	list(timeTrans = timeTrans)
	)

	return(listPlots)
	
}

#' Set missing start/end time variable in the data.
#' @details 
#' In case of missing values for the time start or end variables, they are replaced by:
#' \itemize{
#' \item{in case \code{timeLim} is specified: }{specified time limits}
#' \item{otherwise, in case \code{timeLimData}, \code{timeLimStartVar} and \code{timeLimEndVar} are specified: }{
#' minimum/maximum values in the variable: \code{timeLimVar} in the data: \code{timeLimData}
#' for the specific subject (if available). If there are missing for a specific subject,
#' they are taken across subjects.
#' }
#' \item{otherwise (default): }{the missing values for start/end date are replaced by the 
#' minimum/maximum of the times available in the \code{timeStartVar} and 
#' \code{timeEndVar} of the visualized \code{data} PER SUBJECT (if available).
#' }
#' \item{if all time are missings, the range is set to 0 and Inf} 
#' }
#' The time limits are the same across subjects, and set to:
#' \itemize{
#' \item{\code{timeLim} if specified}
#' \item{maximum time range in \code{timeLimStartVar} and \code{timeLimEndVar} in \code{timeLimData} 
#' if specified}
#' \item{the maximum range on the data obtained after filling missing values}
#' }
#' @param data data.frame with data
#' @param timeStartVar string, variable of \code{data} with start of range
#' @param timeEndVar string, variable of \code{data} with end of range
#' @param timeLim (optional) vector of length 2 with time limits (x-axis)
#' If not specified, extracted from the minimum \code{timeStartVar} 
#' and maximum \code{timeEndVar} per subject.
#' @param timeLimData data.frame with data used to extract time limits per subject
#' @param timeLimStartVar string, variable of \code{timeLimData} with time start
#' @param timeLimEndVar string, variable of \code{timeLimData} with time end
#' @inheritParams filterData
#' @return list with:
#' \itemize{
#' \item{'data': }{data with filled missing start/end time variables.
#' The data also contains additional logical columns 'missingStartPlot' and 'missingEndPlot',
#' set to TRUE if the records had missing \code{timeStartVar} and \code{timeEndVar} variables.
#' }
#' \item{'timeLim': }{vector of length 2 with minimum/maximum time limits across subjects.
#' }
#' \item{'timeLimSpecified': }{vector of length 2 with time limits as specified by the user,
#' either extracted from \code{timeLim} or from \code{timeLimData}
#' }
#' }
#' @importFrom plyr ddply
#' @author Laure Cougnaud
formatTimeInterval <- function(data, 
	timeStartVar, timeEndVar, 
	subjectVar = "USUBJID",
	timeLim = NULL, timeLimData = NULL, 
	timeLimStartVar = NULL, timeLimEndVar = NULL
){

	timeLimInData <- with(data, c(min(get(timeStartVar), na.rm = TRUE), max(get(timeEndVar), na.rm = TRUE)))
	
	timeLimDataSpec <- !is.null(timeLimData) & 
		!is.null(timeLimStartVar) & !is.null(timeLimEndVar) && 
		all(c(timeLimStartVar, timeLimEndVar) %in% colnames(timeLimData))
	
	# in case data is a tibble:
	if(!is.null(timeLimData))
		timeLimData <- as.data.frame(timeLimData)

	data <- ddply(data, subjectVar, function(x){
				
		subject <- unique(x[, subjectVar])
		
		xTimeData <- if(timeLimDataSpec){
			xTimeData <- subset(timeLimData, get(subjectVar) == subject)[, c(timeLimStartVar, timeLimEndVar)]
			if(all(is.na(xTimeData))){
				timeLimData[, c(timeLimStartVar, timeLimEndVar)]
			}else xTimeData
		}
		
		if(is.null(xTimeData) || all(is.na(xTimeData)))
			xTimeData <- x[, c(timeStartVar, timeEndVar)]
		
		# extract time range
		timeRangeData <- if(all(is.na(xTimeData))){
			if(!is.null(timeLimInData) && all(!is.na(timeLimInData))){
				timeLimInData
			}else{
				c(0, Inf)
			}
		}else range(xTimeData, na.rm = TRUE)
		
		# check missing start date		
		x$missingStartPlot <- is.na(x[, timeStartVar])
		if(any(x$missingStartPlot)){
			startTime <- ifelse(!is.null(timeLim), timeLim[1], min(timeRangeData))
			x[x$missingStartPlot, timeStartVar] <- startTime
		}
		
		# check missing end date
		x$missingEndPlot <- is.na(x[, timeEndVar])
		if(any(x$missingEndPlot)){
			endTime <- ifelse(!is.null(timeLim), timeLim[2], max(timeRangeData))
			x[x$missingEndPlot, timeEndVar] <- endTime
		}
		
		# return the data
		x				
				
	})

	data$missingStartEndPlot <- with(data, interaction(missingStartPlot, missingEndPlot))
	
	timeLimSpecified <- if(!is.null(timeLim)){
		timeLim
	}else if(timeLimDataSpec){
		range(timeLimData[, c(timeLimStartVar, timeLimEndVar)], na.rm = TRUE)
	}
	
	if(is.null(timeLim)){
		timeLim <- if(timeLimDataSpec){
			range(timeLimData[, c(timeLimStartVar, timeLimEndVar)], na.rm = TRUE)
		}else{
			dataForTimeLim <- data[, c(timeStartVar, timeEndVar)]
			range(dataForTimeLim[!is.na(dataForTimeLim) && dataForTimeLim != "Inf"])
		}
	}

	res <- list(data = data, timeLim = timeLim, timeLimSpecified = timeLimSpecified)
	
	return(res)
	
}