#' Add reference lines to a profile plot
#' @param gg \code{\link[ggplot2]{ggplot2}} object
#' @param refLines (optional) nested list with details for reference line(s).
#' Each sublist contains:
#' \itemize{
#' \item{(required) 'label': }{string with label for the reference line}
#' \item{(required) 'time': }{time(x) coordinate for the reference line,
#' 'dotted' by default}
#' \item{(optional) 'color': }{color for the reference line,
#' 'black' by default}
#' \item{(optional) 'linetype': }{linetype for the reference line}
#' }
#' @param refLinesData data.frame with data from which the reference line(s) should be extracted
#' @param refLinesTimeVar string, variable of \code{refLinesData} with time for reference line(s)
#' @param refLinesLabelVar string, variable of \code{refLinesData} with label for reference line(s)
#' @param refLinesColor vector of length 1 with default color for reference line(s)
#' @param refLinesLinetype vector of length 1 with default linetype for reference line(s)
#' @param addLabel logical, if TRUE (FALSE by default) add the label of the reference line(s) at the bottom of the plot
#' @param timeLim vector of length 2 with time limits
#' @inheritParams subjectProfileIntervalPlot
#' @return if \code{addLabel} is TRUE, list with:
#' 'gg': \code{\link[ggplot2]{ggplot2}} and 'ggRefLines': \code{\link[ggplot2]{ggplot2}} with labels,
#' \code{\link[ggplot2]{ggplot2}} otherwise
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom stats setNames
#' @importFrom cowplot plot_grid
addReferenceLinesProfilePlot <- function(
	gg, 
	subjectVar = "USUBJID",
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL,
	refLinesColor = "black",
	refLinesLinetype = "dotted",
	timeLim = NULL,
	addLabel = FALSE){
	
	refLinesVect <- !is.null(refLines)
	refLinesFromData <- !is.null(refLinesData) & !is.null(refLinesTimeVar) & !is.null(refLinesLabelVar) 
	
	if(refLinesVect){
		
		refLinesLabels <- sapply(refLines, function(x) x$label)
		refLinesTime <- sapply(refLines, function(x) x$time)
		refLinesColor <- sapply(refLines, function(x) 
			ifelse("color" %in% names(x), x$color, refLinesColor)
		)
		refLinesLinetype <- sapply(refLines, function(x) 
			ifelse("linetype" %in% names(x), x$linetype, refLinesLinetype)
		)
		
	}else if(refLinesFromData){
		
		subjectIDPlot <- attr(gg, "metaData")$subjectID
		if(is.null(subjectIDPlot))
			warning("No reference lines are added to the plot with subject ID, because no 'subjectID' available.")
		
		refLinesInfo <- subset(refLinesData, 
			get(subjectVar) == subjectIDPlot &
				!is.na(get(refLinesLabelVar)) &
				!is.na(get(refLinesTimeVar))
		)
		refLinesLabels <- refLinesInfo[, refLinesLabelVar]
		refLinesTime <- refLinesInfo[, refLinesTimeVar]
		
	}
	
	res <- if(refLinesVect | refLinesFromData && length(refLinesTime) > 0){
				
		if(length(refLinesColor) == 1)
			refLinesColor <- rep(refLinesColor, length(refLinesLabels))
		if(length(refLinesLinetype) == 1)
			refLinesLinetype <- rep(refLinesLinetype, length(refLinesLabels))
		
		# add vertical lines
		for(i in seq_along(refLinesTime)){
			gg <- gg + geom_vline(
				xintercept = refLinesTime[i], 
				color = refLinesColor[i],
				linetype = refLinesLinetype[i], 
				alpha = 0.5,
				size = 1
			)
		}
		
		# add label at the bottom of the plot
		res <- if(addLabel){
					
			# extract number of lines for label
			nLinesRefLines <- max(nchar(refLinesLabels))/2
			
			# add label(s)
			colors <- setNames(refLinesColor, refLinesLabels)
			dataText <- data.frame(x = refLinesTime, label = refLinesLabels)
			ggRefLines <- ggplot(data = dataText) +
				geom_text(aes(x = x, label = label, colour = label, y = 0), 
					angle = 90, hjust = 0.5, show.legend = FALSE
				) + theme_void() +
				scale_color_manual(values = colors, limits = names(colors))
			if(!is.null(timeLim))	ggRefLines <- ggRefLines + coord_cartesian(xlim = timeLim)
			
			attributes(ggRefLines)$metaData$nLinesLabelRefLines  <- nLinesRefLines

			list(gg = gg, ggRefLines = ggRefLines)
					
		}else gg
		
	}else gg
	
	return(res)
	
}
