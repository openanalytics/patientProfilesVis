#' Visualize time interval in subject profiles,
#' so event with a start and end time.
#' @inheritSection formatTimeInterval Time interval representation
#' @param colorVar String, variable of \code{data} with color,
#' used both for the point(s) and segment(s).
#' @param shapePalette Named vector with (combined) shape palette for 
#' \code{timeStartShapeVar}\code{timeEndShapeVar}.
#' @param shapeLab String with label for \code{timeStartShapeVar}\code{timeEndShapeVar}
#' @param shapeSize Size for symbols (only used if \code{timeStartShapeVar}/\code{timeEndShapeVar} is specified).
#' @param timeAlign Logical, if TRUE (by default)
#' the different plots are horizontally aligned.
#' If set to FALSE, each plot has its own time-limits.\cr
#' If set to FALSE, this is not compatible with 
#' the specification of \code{timeLim}.
#' @param title String with title, label of the parameter variable by default.
#' @param caption (optional) String with caption (NULL for no caption). 
#' By default the caption contains information on the imputation strategy 
#' for missing time. 
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams filterData
#' @inheritParams clinUtils::formatVarForPlotLabel
#' @inheritParams formatTimeInterval
#' @inheritParams getPageVar
#' @return list of (across subjects) of list (across pages) 
#' of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileIntervalPlot}.
#'  with additional 'metaData' attributes containing
#' '\code{label}', 'timeLim' \code{timeTrans} and \code{timeExpand} (if specified).
#' @author Laure Cougnaud
#' @family patient profiles plotting function
#' @import ggplot2
#' @importFrom plyr dlply
#' @importFrom stats reorder
#' @importFrom clinUtils getLabelVar formatVarForPlotLabel
#' @export
subjectProfileIntervalPlot <- function(
	data,
	paramVar, paramVarSep = " - ",
	paramLab = getLabelVar(paramVar, labelVars = labelVars),
	paramGroupVar = NULL,
	timeStartVar, timeStartLab = getLabelVar(timeStartVar, labelVars = labelVars),
	timeEndVar, timeEndLab = getLabelVar(timeEndVar, labelVars = labelVars),
	timeLab = toString(c(timeStartLab, timeEndLab)),
	subjectVar = "USUBJID", subjectSubset = NULL,
	subjectSample = NULL, seed = 123,
	subsetData = NULL, subsetVar = NULL, subsetValue = NULL, 
	timeImpType = c("minimal", "data-based", "none"),
	timeLim = NULL, timeLimData = NULL, 
	timeLimStartVar = NULL, timeLimStartLab = getLabelVar(timeLimStartVar, labelVars = labelVars),
	timeLimEndVar = NULL, timeLimEndLab = getLabelVar(timeLimEndVar, labelVars = labelVars),
	timeTrans = NULL, timeExpand = NULL,
	timeAlign = TRUE,
	xLab = timeLab,
	yLab = "",
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	alpha = 1,
	timeStartShapeVar = NULL, timeEndShapeVar = NULL,
	shapePalette = NULL, 
	shapeLab = toString(unique(getLabelVar(c(timeStartShapeVar, timeEndShapeVar), labelVars = labelVars))),
	shapeSize = rel(3),
	title = toString(getLabelVar(paramVar, labelVars = labelVars, label = paramLab)),
	label = title,
	caption,
	labelVars = NULL,
	formatReport = subjectProfileReportFormat(),
	paging = TRUE){

	timeImpType <- match.arg(timeImpType)
	
	# in case data is a tibble:
	data <- as.data.frame(data)
	
	# check if specified variable(s) are available in the data
	checkVar(var = subjectVar, data = data)
	checkVar(var = paramVar, data = data)
	checkVar(var = paramGroupVar, data = data)
	checkVar(var = c(timeStartVar, timeEndVar), data = data)
	checkVar(var = c(timeStartShapeVar, timeEndShapeVar), data = data)
	
	if(!is.null(timeLim) & !timeAlign)
		warning(paste(
			"Time limits are not set as the visualizations shouldn't not be aligned across subjects.",
			"You might want to set 'timeAlign' to TRUE."
		))
		
	# fill missing start/end time and extract time limits
	resMSED <- formatTimeInterval(
		data = data, 
		timeStartVar = timeStartVar, timeStartLab = timeStartLab,
		timeEndVar = timeEndVar, timeEndLab = timeEndLab,
		timeStartShapeVar = timeStartShapeVar, timeEndShapeVar = timeEndShapeVar,
		subjectVar = subjectVar,
		timeLim = timeLim, timeLimData = timeLimData, 
		timeImpType = timeImpType,
		timeLimStartVar = timeLimStartVar, timeLimStartLab = timeLimStartLab,
		timeLimEndVar = timeLimEndVar, timeLimEndLab = timeLimEndLab
	)
	data <- resMSED$data
	timeLim <- resMSED$timeLim
	timeLimInit <- resMSED$timeLimSpecified
	timeShapePalette <- resMSED$timeShapePalette
	caption <- if(!missing(caption)){caption}else{resMSED$caption}
	
	# specify the time limits if not specified
	# otherwise if missing values for start/end for all records of a patient
	# 'segment' span entire plotting window
	
	# concatenate variable(s) if multiple are specified
	data[, "yVar"] <- interactionWithMissing(data = data, vars = paramVar, varSep = paramVarSep)
	
	# only keep records of interest
	data <- filterData(
		data = data, 
		subsetVar = subsetVar, 
		subsetValue = subsetValue,
		subsetData = subsetData,
		subjectVar = subjectVar, 
		subjectSubset = subjectSubset,
		subjectSample = subjectSample, 
		seed = seed
	)

	# if paramGroupVar is specified: change order levels of 'variable'
	data$yVar <- formatVarForPlotLabel(
		data = data, paramVar = "yVar", paramGroupVar = paramGroupVar,
		revert = TRUE, width = formatReport$yLabelWidth
	)
	
	# convert color variable to factor
	if(!is.null(colorVar)){
		data[, colorVar] <- convertAesVar(data, colorVar)
		if(is.null(colorPalette))	colorPalette <- getColorPalettePatientProfile(x = data[, colorVar])
	}else{
		if(is.null(colorPalette))	colorPalette <- getColorPalettePatientProfile(n = 1)
	}
	
	# convert specified shape variable(s) & extract palettes
	if(!is.null(timeStartShapeVar) | !is.null(timeEndShapeVar)){
		if(!is.null(timeStartShapeVar))
			data[, timeStartShapeVar] <- convertAesVar(data, timeStartShapeVar)
		if(!is.null(timeEndShapeVar))
			data[, timeEndShapeVar] <- convertAesVar(data, timeEndShapeVar)
		if(is.null(shapePalette)){
			shapes <- unlist(lapply(data[, c(timeStartShapeVar, timeEndShapeVar)], levels))
			shapePalette <- getShapePalettePatientProfile(x = shapes)
		}
	}
	if(is.null(timeStartShapeVar) | is.null(timeEndShapeVar))
		shapePalette <- c(shapePalette, timeShapePalette)
	shapePalette <- shapePalette[!duplicated(names(shapePalette))]
	
	# if shape variables are not specified, used the default
	if(is.null(timeStartShapeVar))
		timeStartShapeVar <- "timeStartStatus"
	if(is.null(timeEndShapeVar))
		timeEndShapeVar <- "timeEndStatus"
	
	hasShapeVar <- !is.null(timeStartShapeVar) | !is.null(timeEndShapeVar)
	
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
					x = sym(timeStartVar), xend = sym(timeEndVar), 
					y = sym("yVar"), yend = sym("yVar")
				),
				if(!is.null(colorVar))	list(color = sym(colorVar))
			)
			
			# create the plot
			gg <- ggplot()
		
			## plot segments
	
			# records with start/end date
			# and for records with missing start and/or date: plot segment to have color legend without segment
			# important! entire data should be defined with the first geom
			# and segment defined first, otherwise
			# order in labels of y-axis can be different between geom_point and geom_segment
			argsSegment <- list(
			  mapping = do.call(aes, aesArgs), data = dataSubjectPage,
			  show.legend = TRUE, alpha = alpha
			 )
			aesLineSize <- ifelse(packageVersion("ggplot2") >= "3.4.0", "linewidth", "size")
			argsSegment[[aesLineSize]] <- 2
			gg <- gg + do.call(geom_segment, argsSegment)
				
			geomPointCustom <- function(gg, xVar, shapeVar){			
				aesPC <- c(
					list(x = sym(xVar), y = sym("yVar"), shape = sym(shapeVar)), 
					if(!is.null(colorVar))	list(color = sym(colorVar))
				)
				argsGeomPoint <- list(
					data = dataSubjectPage, 
					mapping = do.call(aes, aesPC), 
					fill = "white",
					size = shapeSize,
					position = position_nudge(y = -0.01),
					alpha = alpha
				)
				if(packageVersion("ggplot2") >= "3.5.0"){
				  argsGeomPoint[["show.legend"]] <- c(
				    shape = TRUE, 
				    if(!is.null(colorVar)) c(color = TRUE)
				  )
				}
		    gg + do.call(geom_point, argsGeomPoint)
			}
			
			gg <- geomPointCustom(gg, xVar = timeStartVar, shapeVar = timeStartShapeVar)
			gg <- geomPointCustom(gg, xVar = timeEndVar, shapeVar = timeEndShapeVar)
			
			if(!is.null(shapePalette))
				gg <- gg + getAesScaleManual(lab = shapeLab, palette = shapePalette, type = "shape")
			
			# lines are included in shape legend
			gg <- gg + guides(shape = guide_legend(override.aes = list(linetype = NA)))
			
			gg <- gg +
				scale_y_discrete(drop = TRUE) +
				subjectProfileTheme()
		
			if(!is.null(title))
				gg <- gg + ggtitle(title)
			
			if(!is.null(xLab))
				gg <- gg + xlab(xLab)
			
			if(!is.null(yLab))
				gg <- gg + ylab(yLab)
			
			if(!is.null(caption))
				gg <- gg + labs(caption = caption) + 
					theme(plot.caption = element_text(hjust = 0.5))
		
			# color palette and name for color legend
			if(!is.null(colorVar)){
				gg <- gg + getAesScaleManual(lab = colorLab, palette = colorPalette, type = "color") +
					guides(color = guide_legend(override.aes = list(shape = NA)))
			}else	gg <- gg + scale_color_manual(values = colorPalette)
			
			# set scale only in continuous, if all missing scale is not defined as cont,
			# so get error: Error: Discrete value supplied to continuous scale
			isXScaleCont <- !all(is.na(dataSubjectPage[, c(timeStartVar, timeEndVar)]))
			argsScaleX <- c(
				if(!is.null(timeExpand))	list(expand = timeExpand),
				if(!is.null(timeTrans))	list(trans = timeTrans)
			)
			if(isXScaleCont && length(argsScaleX) > 0)
				gg <- gg + do.call("scale_x_continuous", argsScaleX)
					
			# set time limits for the x-axis
			# default: FALSE in case time limits are changed afterwards
			if(!is.null(timeLim) & timeAlign){
				timeLimSubject <- if(is.list(timeLimInit))	timeLimInit[[subject]]	else	timeLimInit
				gg <- gg + coord_cartesian(xlim = timeLimSubject, default = TRUE)
			}

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
					values = colorPalette,
					title = colorLab
				)
			if(hasShapeVar){
				nLinesLegend <- nLinesLegend +
					getNLinesLegend(
						values = shapePalette,
						title = shapeLab
					)
			}
			# 1 line to separate the two legends if color and shape are specified and different
			# (ggplot will create separate legend if the title differ)
			if(!is.null(colorVar) & hasShapeVar && colorLab != shapeLab){
				nLinesLegend <- nLinesLegend + 1
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
		if(!is.null(timeTrans))	list(timeTrans = timeTrans),
		if(!is.null(timeExpand))	list(timeExpand = timeExpand)
	)

	return(listPlots)
	
}

#' Set missing start/end time variable in the data.
#' @section Time interval representation:
#' In case the start or the end of the time interval contain missing values:
#' \itemize{
#' \item{if a dataset (\code{timeLimData}), start (\code{timeLimStartVar})
#' and end (\code{timeLimEndVar}) variables are specified: 
#' \enumerate{
#' \item{for each subject: 
#' \itemize{
#' \item{the minimum and maximum time values across these specified time 
#' variables are extracted}
#' \item{missing start values are replaced by the minimum time}
#' \item{missing start values are replaced by the maximum time}
#' }}
#' \item{if all values are missing for this subject, they are taken across subjects}
#' }}
#' \item{otherwise, depending on the imputation type (\code{timeImpType}):
#' \itemize{
#' \item{'minimal' (by default): 
#' \itemize{
#' \item{if the start and the end of the interval are missing: 
#' no imputation is done, only the label is displayed}
#' \item{if the start time is missing and the end time is not missing: 
#' start time is imputed with end time, and status is set to 'Missing start'}
#' \item{if the end time is missing and the start time is not missing: 
#' end time is imputed with start time, and status is set to 'Missing end'}
#' }}
#' \item{'data-based' (default in version < 1.0.0): 
#' minimum/maximum values in the start/end time variables in the data are considered
#' for the specific subject (if available). If there are missing for a specific subject,
#' they are taken across subjects. If all time are missings, the range is set to 0 and Inf}
#' \item{'none': no imputation is done}
#' }}}
#' The symbols displayed at the start and end of the interval are:
#' \itemize{
#' \item{by default: 
#' \itemize{
#' \item{a filled square labelled 'Complete' if the time is not missing}
#' \item{a filled left-directed arrow in case of missing start time}
#' \item{a filled right-directed arrow in case of missing end time}
#' }}
#' \item{if the variable(s) used for the shape of the start or end 
#' of the interval are specified (via 
#' \code{timeStartShapeVar}/\code{timeEndShapeVar}): 
#' labels are based on these variables, and a standard shape palette is used}
#' }
#' The time limits are the same across subjects, and set to:
#' \itemize{
#' \item{\code{timeLim} if specified}
#' \item{maximum time range in \code{timeLimStartVar} and 
#' \code{timeLimEndVar} in \code{timeLimData} 
#' if specified}
#' \item{the maximum range on the data obtained after imputation of missing 
#' values}
#' }
#' @param timeStartVar String, variable of \code{data} 
#' with start of time interval.
#' @param timeEndVar String, variable of \code{data} 
#' with end of time interval.
#' @param timeStartShapeVar (optional) String, variable of \code{data} 
#' used for the shape of the symbol displayed 
#' at the start of the time interval.\cr
#' If not specified, default shape palette is used,
#' see section 'Time interval representation'.
#' @param timeEndShapeVar String, variable of \code{data} 
#' used for the shape of the symbol 
#' displayed at the end of the time interval.
#' If not specified, default shape palette is used,
#' see section 'Time interval representation'.
#' @param timeStartLab String, label for \code{timeStartVar},
#' displayed in a message and in the plot caption.
#' @param timeEndLab String, label for \code{timeEndVar},
#' displayed in a message and in the plot caption.
#' @param timeLimData Data.frame with data used to impute time
#' in case some time records are missing in \code{data}, 
#' see section: 'Time interval representation'.
#' @param timeLimStartVar String, variable of \code{timeLimData} with 
#' start of the time interval.
#' @param timeLimStartLab String, label for \code{timeLimeStartVar},
#' displayed in a message and in the plot caption.
#' @param timeLimEndVar String, variable of \code{timeLimData} with 
#' end of the time interval.
#' @param timeLimEndLab String, label for \code{timeLimEndVar},
#' displayed in a message and in the plot caption.
#' @param timeImpType String with imputation type: 'minimal' (default),
#' 'data-based' or 'none', see section: 'Time interval representation'.\cr
#' This imputation type is not used if a dataset used to impute time is 
#' specified.
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams getPageVar
#' @return list with:
#' \itemize{
#' \item{'data': Data with:
#' \itemize{
#' \item{imputed \code{timeStartVar} and \code{timeEndVar}}
#' \item{new column 'timeStartStatus': 
#' character vector containing status of \code{timeStartVar} variable:
#'  'Complete' or 'Missing start' or NA}
#' \item{new column 'timeEndStatus': 
#' character vector containing status of \code{timeEndVar} variable:
#'  'Complete' or 'Missing end' or NA}
#' }}
#' \item{'timeLim': vector of length 2 with minimum/maximum time limits 
#' across subjects.}
#' \item{'timeLimSpecified': vector of length 2 with time limits as specified 
#' by the user, either extracted from \code{timeLim} or from \code{timeLimData}.
#' If missing value within \code{timeLim}, the corresponding minimum/maximum
#' value in the (updated) data is used.}
#' \item{'timeShapePalette': Named character vector with symbols for the 
#' different time status}
#' \item{'caption': String with extra explanation concerning imputation that 
#' could be included in plot caption.}
#' }
#' @importFrom plyr ddply
#' @importFrom clinUtils getLabelVar
#' @author Laure Cougnaud
formatTimeInterval <- function(data, 
	timeStartVar, timeStartLab = getLabelVar(timeStartVar, labelVars = labelVars),
	timeEndVar, timeEndLab = getLabelVar(timeEndVar, labelVars = labelVars),
	timeStartShapeVar = NULL, timeEndShapeVar = NULL,
	subjectVar = "USUBJID",
	timeLim = NULL, 
	timeLimData = NULL, 
	timeLimStartVar = NULL, timeLimStartLab = getLabelVar(timeLimStartVar, labelVars = labelVars),
	timeLimEndVar = NULL, timeLimEndLab = getLabelVar(timeLimEndVar, labelVars = labelVars),
	timeImpType = c("minimal", "data-based", "none"),
	labelVars = NULL
){
	
	timeImpType <- match.arg(timeImpType)
	
	timeLimDataSpec <- !is.null(timeLimData) & 
		!is.null(timeLimStartVar) & !is.null(timeLimEndVar) && 
		all(c(timeLimStartVar, timeLimEndVar) %in% colnames(timeLimData))
	if(!is.null(timeLimData) & !timeLimDataSpec){
		warning(paste(
			"Dataset to extract time limits ('timeLimData') is specified",
			"but start/end variable(s) are not specified",
			"or not available in this data. So 'timeLimData' is not considered."
		))
		timeLimData <- NULL
	}
	
	# in case data is a tibble:
	if(!is.null(timeLimData))
		timeLimData <- as.data.frame(timeLimData)
	
	# which records have missing values?
	data$missingStartPlot <- is.na(data[, timeStartVar])
	data$missingEndPlot <- is.na(data[, timeEndVar])
	
	# message for the user:
	if(any(data$missingStartPlot | data$missingEndPlot)){
		msg <- paste(c(
			if(any(data$missingStartPlot))	paste(sum(data$missingStartPlot), "record(s) with missing", timeStartLab),
			if(any(data$missingEndPlot))	paste(sum(data$missingEndPlot), "record(s) with missing", timeEndLab)
		), collapse = " and ")
		msg <- switch(timeImpType,
			'none' = paste(msg, "are not considered."),
			paste0(msg, " are imputed with ", 
				if(timeLimDataSpec)	paste(paste(c(timeLimStartLab, timeLimEndLab), collapse = "/"), " or "),
				timeImpType, " imputation."
			)
		)
		message(msg)
	}
	
	# variables with status used for the shapes
	data$timeStartStatus <- ifelse(data$missingStartPlot, "Missing start", "Complete")
	data$timeEndStatus <- ifelse(data$missingEndPlot, "Missing end", "Complete")
	
	# create caption
	caption <- NULL
	if(!is.null(timeLimData))
		caption <- paste0("Records with missing ", timeStartLab, "/", 
			timeEndLab, " are displayed at the ", timeLimStartLab, "/", 
			timeLimEndLab, " if available.")
	switch(timeImpType,
		`data-based` = {
			caption <- paste(c(caption, 
				paste0(
					"Records with missing ", timeStartLab, "/", timeEndLab, " are displayed ", 
					"at the respective minimum/maximum value across parameters by subject if available, ",
					"across subjects otherwise."
				)
			), collapse = "\n")
		},
		`minimal`= {
			caption <- paste(c(caption, 
				paste0(
					 "Records with missing ", timeStartLab, " (or ", timeEndLab,
					") are displayed at their respective ", timeEndLab, 
					" (or ", timeStartLab, ").\n",
					"Only the label is displayed for records with missing ", 
					timeStartLab, " and ", timeEndLab, "."
				)
			), collapse = "\n")
		}
	)
	if(timeImpType == "none" && is.null(timeLimData))
		caption <- NULL
	
	# variables used for the symbols	
	data <- ddply(data, subjectVar, function(x){
				
		subject <- unique(as.character(x[, subjectVar]))
		
		# 1) extract limits from specified 'timeLimeData'
		if(!is.null(timeLimData)){
			xTimeData <- timeLimData[which(timeLimData[, subjectVar] == subject), ]
			xTimeData <- xTimeData[, c(timeLimStartVar, timeLimEndVar)]
			if(all(is.na(xTimeData))){
				xTimeData <- timeLimData[, c(timeLimStartVar, timeLimEndVar)]
			}
			xTimeDataRange <- range(xTimeData, na.rm = TRUE)
		}
		
		# impute values from specified timeLimData
		if(!is.null(timeLimData) && !any(is.na(xTimeDataRange))){
			
			x[which(x$missingStartPlot), timeStartVar] <- min(xTimeDataRange)
			x[which(x$missingEndPlot), timeEndVar] <- max(xTimeDataRange)
			
		# or rules based on 'timeImpType' parameter:
		}else{
		
			switch(timeImpType,
							
				`data-based` = {
					
					xTimeData <- x[, c(timeStartVar, timeEndVar)]
					
					suppressWarnings(timeLimInData <- with(data, 
						c(min(get(timeStartVar), na.rm = TRUE), max(get(timeEndVar), na.rm = TRUE))
					))
					
					# extract time range
					timeRangeData <- if(all(is.na(xTimeData))){
						if(!is.null(timeLimInData) && all(!is.na(timeLimInData)) && all(is.finite(timeLimInData))){
							timeLimInData
						}else{
							c(0, Inf)
						}
					}else range(xTimeData, na.rm = TRUE)
					
					x[which(x$missingStartPlot), timeStartVar] <- min(timeRangeData)
					x[which(x$missingEndPlot), timeEndVar] <- max(timeRangeData)
					
				},
				
				`minimal` = {
					
					# missing start/end: NA (only label is displayed in the y-axis)
					idxMissingEndAndStart <- which(x$missingStartPlot & x$missingEndPlot)
					x[idxMissingEndAndStart, "timeStartStatus"] <- 
						x[idxMissingEndAndStart, "timeEndStatus"] <- NA_character_
					
					# missing start: impute with end date
					idxMissingStartNotEnd <- which(x$missingStartPlot & !x$missingEndPlot)
					x[idxMissingStartNotEnd, timeStartVar] <- x[idxMissingStartNotEnd, timeEndVar]
					x[idxMissingStartNotEnd, "timeEndStatus"] <- NA_character_
					
					# missing end: impute with start date
					idxMissingEndNotStart <- which(x$missingEndPlot & !x$missingStartPlot)
					x[idxMissingEndNotStart, timeEndVar] <- x[idxMissingEndNotStart, timeStartVar]
					x[idxMissingEndNotStart, "timeStartStatus"] <- NA_character_
					
				}
				
			)
			
		}
		
		# return the data
		x				
				
	})
	
	shapePalette <- c(
		if(is.null(timeStartShapeVar) | is.null(timeEndShapeVar))	c(Complete = '\u25A0'), 
		if(is.null(timeStartShapeVar))	c(`Missing start` = "\u25C4"), 
		if(is.null(timeEndShapeVar))	c(`Missing end` = "\u25BA")
	)
	
	timeLimSpecified <- if(!is.null(timeLim)){
				
		formatTimeLim(
			data = data, subjectVar = subjectVar, 
			timeStartVar = timeStartVar, timeEndVar = timeEndVar, 
			timeLim = timeLim
		)

	}else if(!is.null(timeLimData)){
		range(timeLimData[, c(timeLimStartVar, timeLimEndVar)], na.rm = TRUE)
	}
	
	# time limits for the plot
	if(is.null(timeLim)){
		timeLim <- if(!is.null(timeLimData)){
			range(timeLimData[, c(timeLimStartVar, timeLimEndVar)], na.rm = TRUE)
		}else{
			dataForTimeLim <- data[, c(timeStartVar, timeEndVar)]
			range(dataForTimeLim[!is.na(dataForTimeLim) & !dataForTimeLim %in% c(-Inf, Inf)])
		}
	}

	res <- list(
		data = data, 
		timeLim = timeLim, 
		timeLimSpecified = timeLimSpecified,
		timeShapePalette = shapePalette,
		caption = caption
	)
	
	return(res)
	
}