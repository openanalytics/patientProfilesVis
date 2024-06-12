#' Add reference lines to a profile plot
#' @param gg \code{\link[ggplot2]{ggplot2}} with
#' a subject profile plot for a specific subject (and page)
#' (subset of the output of the \code{subjectProfile[X]Plot})
#' @param refLines (optional) nested list with details for reference line(s).
#' Each sublist contains:
#' \itemize{
#' \item{(required) 'label': string with label for the reference line}
#' \item{(required) 'time': unique time (x) coordinate for the reference line}
#' \item{(optional) 'color': color for the reference line,
#' 'black' by default}
#' \item{(optional) 'linetype': linetype for the reference line,
#' 'dotted' by default}
#' }
#' @param refLinesData data.frame with data from which the reference line(s) should be extracted
#' @param refLinesTimeVar string, variable of \code{refLinesData} with time for reference line(s)
#' @param refLinesLabelVar string, variable of \code{refLinesData} with label for reference line(s)
#' @param refLinesColor vector of length 1 with default color for reference line(s)
#' @param refLinesLinetype vector of length 1 with default linetype for reference line(s)
#' @param addLabel logical, if TRUE (FALSE by default) add the label of the reference line(s) at the bottom of the plot
#' @param timeLim vector of length 2 with time limits.
#' This is used to set the limits to the plot
#' containing the reference lines labels (if requested).
#' @inheritParams patientProfilesVis-common-args
#' @return 
#' If \code{addLabel} is:
#' \itemize{ 
#' \item{\code{TRUE}: list with:
#' \itemize{
#' \item{'gg': \code{\link[ggplot2]{ggplot2}} plot with reference lines}
#' \item{'ggRefLines': \code{\link[ggplot2]{ggplot2}} plot containing only 
#' the labels at the specified position}
#' }}
#' \item{\code{FALSE}: \code{\link[ggplot2]{ggplot2}} plot with reference lines}
#' }
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom stats setNames
#' @importFrom utils packageVersion
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
		
		refLinesLabels <- sapply(refLines, function(x){
			if(!"label" %in% names(x))
				stop("Label should be specified for the reference line(s).")
			x$label
		})
		refLinesTime <- sapply(refLines, function(x){
			if(!"time" %in% names(x))
				stop("Time position should be specified for the reference line(s).")
			x$time
		})
		refLinesColor <- sapply(refLines, function(x) 
			ifelse("color" %in% names(x), x$color, refLinesColor)
		)
		refLinesLinetype <- sapply(refLines, function(x) 
			ifelse("linetype" %in% names(x), x$linetype, refLinesLinetype)
		)
		
	}else if(refLinesFromData){
		
		subjectIDPlot <- attr(gg, "metaData")$subjectID
		if(is.null(subjectIDPlot)){
			warning("No reference lines are added to the plot with subject ID, because no 'subjectID' available.")
			return(gg)
		}
		subjectIDPlot <- as.character(subjectIDPlot)
			
		idxSel <- which(
			refLinesData[, subjectVar] == subjectIDPlot &
			!is.na(refLinesData[, refLinesLabelVar]) &
			!is.na(refLinesData[, refLinesTimeVar])
		)
		refLinesInfo <- refLinesData[idxSel, ]
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
			argsGeomVLine <- list(
				xintercept = refLinesTime[i], 
				color = refLinesColor[i],
				linetype = refLinesLinetype[i], 
				alpha = 0.5
			)
			aesLineSize <- ifelse(packageVersion("ggplot2") >= "3.4.0", "linewidth", "size")
			argsGeomVLine[[aesLineSize]] <- 1
			gg <- gg + do.call(geom_vline, argsGeomVLine)
		}
		
		# add label at the bottom of the plot
		res <- if(addLabel){
					
			refLinesLabels <- as.character(refLinesLabels)		
					
			# extract number of lines for label
			nLinesRefLines <- max(nchar(refLinesLabels))/2
			
			# add label(s)
			colors <- setNames(refLinesColor, refLinesLabels)
			dataText <- data.frame(x = refLinesTime, label = refLinesLabels)
			aesRefLines <- list(x = sym("x"), label = sym("label"), 
			   colour = sym("label"), y = 0)
			ggRefLines <- ggplot(data = dataText) +
				geom_text(
				  mapping = do.call(aes, aesRefLines), 
					angle = 90, hjust = 0.5, show.legend = FALSE
				) + theme_void() +
				scale_color_manual(values = colors, limits = names(colors))
			if(!is.null(timeLim))	ggRefLines <- ggRefLines + coord_cartesian(xlim = timeLim, default = TRUE)
			
			attributes(ggRefLines)$metaData$nLinesLabelRefLines  <- nLinesRefLines

			list(gg = gg, ggRefLines = ggRefLines)
					
		}else gg
		
	}else gg
	
	return(res)
	
}
