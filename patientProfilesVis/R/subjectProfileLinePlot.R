#' Visualize subject profiles of the evolution of 
#' continuous parameters versus time (spaghetti plot).
#' @param paramValueVar String, variable of \code{data} 
#' with parameter value to represent.\cr
#' Records with missing values are discarded.
#' @param paramNameVar Character vector with variable(s) of \code{data} with parameter name.
#' If multiple, they are concatenated with \code{paramVarSep}.
#' @param paramLab Named character vector, 
#' with label for the parameter variable(s) (\code{paramNameVar}).\cr
#' This is used to set the default title.
#' @param paramVarSep string with character(s) used to concatenate multiple 
#' \code{paramNameVar}, ' - ' by default.
#' @param paramValueRangeVar Character vector of length 2 containing 
#' variables of \code{data} with minimum and maximum value 
#' for \code{paramValueVar}, typically reference range indicators.\cr
#' Range can differ per parameter and even per time point.
#' This range is represented as a ribbon in the plot background.
#' e.g. to represent the reference range of the variable.
#' @param colorValueRange String with color for the filling of 
#' the ribbon represented by \code{paramValueRangeVar}.
#' @param colorVar String, variable of \code{data} with color.
#' This variable is used
#' for the colors and the filling of the points.
#' @param yLimFrom String with specification on the limits of the y-axis, either:
#' \itemize{
#' \item{'all' (by default): for each parameter (\code{paramNameVar}), 
#' the y-axis range contains the minimum/maximum value of 
#' the reference range (\code{paramValueRangeVar}) or data}
#' \item{'value': for each parameter (\code{paramNameVar}), 
#' the y-axis minimum/maximum value is restricted to the data range only.
#' Please note that the ribbon visualizing the reference range is also restricted
#' to the data range if wider.}
#' }
#' @param shapeSize Size for the symbols, any integer or 
#' object supported by \code{size} in \code{\link[ggplot2]{geom_point}}.
#' @param title String with title, label of the parameter value variable by default.
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams filterData
#' @inheritParams getPageVar
#' @return List of (across subjects) of list (across modules) 
#' of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileLinePlot}.
#' Each subject profile contains attributes: 'subjectID' and 'nLines' 
#' (estimated number of lines of space the plot will take).
#' The entire list also contains attributes: '\code{label}',
#' 'timeLim' and 'timeTrans' (if specified).
#' @author Laure Cougnaud
#' @family patient profiles plotting function
#' @import ggplot2
#' @importFrom plyr dlply
#' @importFrom clinUtils getLabelVar formatVarForPlotLabel
#' @export
subjectProfileLinePlot <- function(
	data,
	paramValueVar, paramLab = getLabelVar(paramValueVar, labelVars = labelVars),
	paramNameVar = NULL, paramVarSep = " - ",
	paramValueRangeVar = NULL, colorValueRange = "lightgreen",
	yLimFrom = c("all", "value"),
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	shapeVar = colorVar, 
	shapeLab = if(isTRUE(colorVar == shapeVar)){
		colorLab
	}else	getLabelVar(shapeVar, labelVars = labelVars),
	shapePalette = NULL,
	paramGroupVar = NULL,
	timeVar, 
	timeLab = getLabelVar(timeVar, labelVars = labelVars),
	timeTrans = NULL, timeExpand = NULL,
	subjectVar = "USUBJID", subjectSubset = NULL,
	subjectSample = NULL, seed = 123,
	subsetData = NULL, subsetVar = NULL, subsetValue = NULL, 
	xLab = timeLab,
	yLab = "",
	timeLim = NULL,
	title = toString(getLabelVar(paramValueVar, labelVars = labelVars, label = paramLab)),
	label = title,
	labelVars = NULL,
	formatReport = subjectProfileReportFormat(),
	paging = TRUE,
	alpha = 1, shapeSize = rel(1)
){
	
	yLimFrom <- match.arg(yLimFrom)
	
	if(!is.null(paramValueRangeVar) && length(paramValueRangeVar) != 2)
		stop("'paramValueRangeVar' should be of length 2.")
		
	# in case data is a tibble:
	data <- as.data.frame(data)
	
	# check if specified variable(s) are available in the data
	checkVar(var = subjectVar, data = data)
	checkVar(var = paramNameVar, data = data)
	checkVar(var = paramValueVar, data = data)
	checkVar(var = paramValueRangeVar, data = data)
	checkVar(var = paramGroupVar, data = data)
	checkVar(var = timeVar, data = data)
	checkVar(var = colorVar, data = data)
	checkVar(var = shapeVar, data = data)
	
	# concatenate variable(s) if multiple are specified
	data[, "paramFacetVar"] <- interactionWithMissing(data = data, vars = paramNameVar, varSep = paramVarSep)
	
	data[, "yVar"] <- data[, paramValueVar]
	
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
	
	# format variable
	data[, "paramFacetVar"] <- formatVarForPlotLabel(
		data = data, paramVar = "paramFacetVar", paramGroupVar = paramGroupVar,
		width = formatReport$yLabelWidth
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
	
	timeLim <- formatTimeLim(
		data = data, subjectVar = subjectVar, 
		timeStartVar = timeVar, timeEndVar = timeVar, timeLim = timeLim
	)
	
	# if axes limits shouldn't span the reference range (only if specified)
	if(!is.null(paramValueRangeVar) & yLimFrom == "value"){
		
		data <- ddply(data, c(subjectVar, "paramFacetVar"), function(x){
					
			# extract data range
			xValue <- x[, paramValueVar]
			if(all(is.na(xValue))){
				valueRange <- c(NA_real_, NA_real_)	
			}else{
				valueRange <- range(xValue, na.rm = TRUE)
			}
			
			# replace reference range by value range
			xMinRV <- x[, paramValueRangeVar[1]]
			xMaxRV <- x[, paramValueRangeVar[2]]
			
			x[, paramValueRangeVar[1]] <- ifelse(
				# all data out of reference range
				(xMinRV > valueRange[2]) | (xMaxRV < valueRange[1]), NA, 
				# otherwise if: min ref less than min data, take data min
				ifelse(xMinRV < valueRange[1], valueRange[1], xMinRV)
			)
			
			x[, paramValueRangeVar[2]] <- ifelse(
				# all data out of reference range
				(xMinRV > valueRange[2]) | (xMaxRV < valueRange[1]), NA, 
				# otherwise if: max ref range more max data, take data max
				ifelse(xMaxRV > valueRange[2], valueRange[2], xMaxRV)
			)
			x
		})
		
	}
	
	listPlots <- dlply(data, subjectVar, function(dataSubject){	
				
		# split plot into multiple page(s)
		dataSubject <- getPageVar(
			data = dataSubject, 
			var = "paramFacetVar", typeVar = "panel",
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
				dataRibbon <- dataSubjectPage[!(
					is.na(dataSubjectPage[, paramValueRangeVar[1]]) &
					is.na(dataSubjectPage[, paramValueRangeVar[2]])
				), ]
				if(length(dataRibbon) > 0){
					# use geom_ribbon instead of geom_rect in case different intervals for different time points
					gg <- gg + 
						geom_ribbon(
							mapping = aes_string(
								x = timeVar, 
								ymin = paramValueRangeVar[1], 
								ymax = paramValueRangeVar[2]
							),
							data = dataRibbon,
							fill = colorValueRange, 
							alpha = 0.1
						)
				}
			}
			
			# line
			if(!is.null(paramNameVar)){
				# remove rows with only one point (no need to connect points with the line)
				# to avoid warning: geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?
				# when 'facet_grid' is called
				nPointsPerParamName <- ddply(dataSubjectPage, "paramFacetVar", nrow)
				paramNameRetained <- nPointsPerParamName[which(nPointsPerParamName$V1 > 1), "paramFacetVar"]
				dataLine <- dataSubjectPage[which(dataSubjectPage[, "paramFacetVar"] %in% paramNameRetained), ]
			}else	dataLine <- dataSubjectPage
			if(nrow(dataLine) > 0)
				gg <- gg + geom_line(data = dataLine, alpha = alpha)
			
			# point
			aesArgsPoint <- c(
				if(!is.null(colorVar))	list(color = colorVar, fill = colorVar),
				if(!is.null(shapeVar))	list(shape = shapeVar)
			)
			
			if(length(aesArgsPoint) > 0){
				gg <- gg + geom_point(do.call(aes_string, aesArgsPoint), 
					alpha = alpha, size = shapeSize)
			}else{
				gg <- gg + geom_point(alpha = alpha, size = shapeSize)
			}
			
			if(!is.null(title))
				gg <- gg + ggtitle(title)
			
			if(!is.null(xLab))
				gg <- gg + xlab(xLab)
			
			if(!is.null(yLab))
				gg <- gg + ylab(yLab)
			
			# general
			gg <- gg + 
				subjectProfileTheme() +
				theme(axis.text.y = element_text(size = 7))
			
			if(!is.null(paramNameVar)){
				
				gg <- gg + facet_grid(
					paste0("paramFacetVar", "~."), 
					scales = "free_y", switch = "y"#,
#					labeller = label_wrap_gen(width = Inf)
				)
				
				argsTheme <- list(
					strip.placement = "outside", 
					strip.background = element_rect(color = NA, fill = NA)
				)
				argsTheme <- c(argsTheme, 
					list(strip.text.y.left = element_text(angle = 0, size = 8, hjust = 1))
				)
				gg <- gg + do.call(theme, argsTheme)
			
				# count number of lines each facet will take
				nLinesPlot <- countNLines(unique(dataSubjectPage[, "paramFacetVar"]))
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
		
			argsScaleX <- c(
				if(!is.null(timeExpand))	list(expand = timeExpand),
				if(!is.null(timeTrans))	list(trans = timeTrans)
			)
			if(length(argsScaleX) > 0)
				gg <- gg + do.call("scale_x_continuous", argsScaleX)
		
			# set time limits for the x-axis
			# default: FALSE in case time limits are changed afterwards
			if(!is.null(timeLim)){
				timeLimSubject <- if(is.list(timeLim))	timeLim[[subject]]	else	timeLim
				gg <- gg + coord_cartesian(xlim = timeLimSubject, default = TRUE)
			}
		
			## extract number of lines
			
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
			class(gg) <- c("subjectProfileLinePlot", class(gg))
		
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