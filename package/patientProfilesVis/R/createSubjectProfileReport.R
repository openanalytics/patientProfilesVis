#' Create subject profile report
#' @param listPlots list of plots, as returned by the \code{\link{subjectProfileTextPlot}},
#' \code{\link{subjectProfileEventPlot}} or \code{\link{subjectProfileIntervalPlot}}
#' @param landscape logical, if TRUE the created report is in landscape format
#' @param outputFile string, path to the output report
#' @param exportFigures logical, if TRUE (FALSE by default) the figures are also exported
#' in png format in a 'figures' folder
#' @inheritParams subjectProfileCombine
#' @return no returned value, the report is created at the location
#' specified by \code{outputFile}
#' @author Laure Cougnaud
#' @importFrom tools texi2pdf
#' @export
createSubjectProfileReport <- function(listPlots, 
	timeLim = getXLimSubjectProfilePlots(listPlots),
	refLines = NULL,
	landscape = FALSE,
	outputFile = "subjectProfile.pdf",
	exportFigures = FALSE){
	
	# combine
	listPlotsPerSubject <- subjectProfileCombine(listPlots, timeLim = timeLim, refLines = refLines)
	
	pathTemplate <- getPathTemplate("subjectProfile.Rnw")
	
	## input parameters for the child document:
	# listPlotsPerSubject
	outputDir <- normalizePath(dirname(outputFile), winslash = "/")
	landscape <- landscape # knitr search for input parameter in the parent envir
	
	## convert Rnw -> tex
	outputFileKnitr <- knitr::knit(input = pathTemplate, quiet = TRUE)
	
	## convert tex -> pdf
	
	# texi2pdf cannot deal with space in name and file should be in current directory
	oldwd <- getwd()
	setwd(outputDir)
	
	# convert to pdf
	texi2pdf(file = outputFileKnitr, clean = TRUE)
	
	# rename output file
	outputTexi2pdf <- paste0(file_path_sans_ext(outputFileKnitr), ".pdf")
	file.rename(from = outputTexi2pdf, to = outputFile)
	
	# clean output directory
	tmp <- file.remove(outputFileKnitr) # remove tex file
	if(!exportFigures)
		unlink(file.path(dirname(outputFile), "figures/"), recursive = TRUE)
	
	setwd(oldwd)
	
}

#' Combine subject profile plots.
#' @param listPlots list of \code{\link[ggplot2]{ggplot2}} objects
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
#' @return a list of \code{subjectProfilePlot} object, containing the combined
#' profile plots for each subject.
#' This is in essence only a \code{\link[ggplot2]{ggplot2}} objects,
#' (with the additional attribute 'nLinesPlot')
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @importFrom ggplot2 coord_cartesian ggplot theme_bw ggtitle
#' @inheritParams subjectProfileIntervalPlot
#' @author Laure Cougnaud
#' @export
subjectProfileCombine <- function(listPlots, 
	timeLim = getXLimSubjectProfilePlots(listPlots),
	refLines = NULL){
	
	# extract all subjects for which at least one plot is available
	subjects <- sort(unique(unlist(lapply(listPlots, names))))
	
	# create empty element in the list if the plot is not available for a certain subject
	listPlotsAll <- lapply(listPlots, function(x){
		list <- x[subjects]
		names(list) <- subjects # in case plot not available for one subject
		attr(list, 'metaData') <- attr(x, 'metaData')
		list
	})

	# add default for reference lines
	if(!is.null(refLines)){
		refLinesLabels <- sapply(refLines, function(x) x$label)
		refLinesTime <- sapply(refLines, function(x) x$time)
		refLinesColor <- sapply(refLines, function(x) 
			ifelse("color" %in% names(x), x$color, "black"))
		refLinesLinetype <- sapply(refLines, function(x) 
			ifelse("linetype" %in% names(x), x$linetype, "dotted"))
	}
	
	## combine plots
	
	# wrapper function to combine ggplot2 objects
	combineGGPlots <- function(..., labels){
		
		listGgPlotsToCombine <- list(...)
		
		# 'empty' plot in case a specific plot is not available for this subject
		isEmpty <- which(sapply(listGgPlotsToCombine, is.null))
		if(any(isEmpty)){
			listGgPlotsToCombine[isEmpty] <- lapply(isEmpty, function(i)
				if(labels[i] != ""){
					gg <- ggplot() + theme_bw() + 
						ggtitle(paste("No", labels[i], "available."))
					class(gg) <- c("subjectProfileEmptyPlot", class(gg))
					gg
				}
			)
		}
		listGgPlotsToCombine <- listGgPlotsToCombine[!sapply(listGgPlotsToCombine, is.null)]
		
		if(length(listGgPlotsToCombine) > 0){
			
			# set same limits for the time/x-axis and reference lines
			plotsToModify <-  which(sapply(listGgPlotsToCombine, function(gg)
				!inherits(gg, "subjectProfileTextPlot") & !inherits(gg, "subjectProfileEmptyPlot")
			))
			if(length(plotsToModify) > 0)
				listGgPlotsToCombine[plotsToModify] <- lapply(listGgPlotsToCombine[plotsToModify], function(gg){
					gg <- gg + coord_cartesian(xlim = timeLim)
					if(!is.null(refLines))
						gg <- gg + geom_vline(
							xintercept = refLinesTime, 
							color = refLinesColor,
							linetype = refLinesLinetype, 
							alpha = 0.5,
							size = 1
						)
					gg				
			})

			# extract number of lines in the y-axis
			nLinesPlot <- sapply(listGgPlotsToCombine, getNLinesYGgplot)

			# add labels of reference lines in last plot
			if(length(plotsToModify) > 0 & !is.null(refLines)){
				# add label reference line
				idxPlotRefLines <- max(plotsToModify)
				ggWithRefLines <- addLabelReferenceLines(
					gg = listGgPlotsToCombine[[idxPlotRefLines]], 
					refLinesLabels = refLinesLabels,
					refLinesTime = refLinesTime,
					refLinesColor = refLinesColor
				)
				listGgPlotsToCombine[[idxPlotRefLines]] <- ggWithRefLines				
				nLinesPlot[idxPlotRefLines] <- nLinesPlot[idxPlotRefLines] + attributes(ggWithRefLines)$nLinesLabelRefLines
			}

			# relative height of each plot
			relHeights <- nLinesPlot/sum(nLinesPlot)
			
			# combine all plots
			plot <- do.call(plot_grid,
				c(
					listGgPlotsToCombine,
					list(align = "v", ncol = 1, axis = "lr",
						rel_heights = relHeights
					)
				)
			)
			
			# store the number of lines in the y-axis (used to adapt size during export)
			attributes(plot) <- c(attributes(plot), 
				list(nLinesPlot = sum(nLinesPlot), nSubplots = length(listGgPlotsToCombine))
			)
			
			class(plot) <- c("subjectProfilePlot", class(plot))
			
			plot
			
		}
		
	}
	
	# combine all plots per subject
	# this returns a list of 'gtable' object
	plotLabels <- sapply(listPlotsAll, function(x){
		label <- attributes(x)$metaData$label
		ifelse(is.null(label), "", label)
	})
	listPlotsPerSubject <- do.call(mapply, 
		c(
			list(FUN = combineGGPlots, SIMPLIFY = FALSE, MoreArgs = list(labels = plotLabels)),
			listPlotsAll
		)
	)
		
	# add title
	listPlotsPerSubject <- sapply(names(listPlotsPerSubject), function(subject){
		
		# create a ggplot for title only
		title <- ggdraw() + 
			draw_label(paste(" Subject:", subject), fontface = 'bold', 
				x = 0, hjust = 0, lineheight = 2
			)
		
		plotSubject <- listPlotsPerSubject[[subject]]
		# extract number of lines in main plot...
		nLinesPlot <- attr(plotSubject, "nLinesPlot")
		# to get relative height of title/main plot
		relHeights <- c(4, nLinesPlot)/(4+nLinesPlot)
#		convertX(ggplot_build(gg)$plot$theme$plot.margin, unitTo = "lines", valueOnly = TRUE)
		
		# combine title and plot
		plotSubjectWithTitle <- plot_grid(title, plotSubject, ncol = 1, rel_heights = relHeights)
		
		nLinesPlotTotal <- nLinesPlot + 4
		
		# store the number of lines in the y-axis (used to adapt size during export)
		attributes(plotSubjectWithTitle) <- c(
			attributes(plotSubjectWithTitle), 
			list(
				nLinesPlot = nLinesPlotTotal,
				nSubplots = attr(plotSubject, "nSubplots")
			)
		)
		class(plotSubjectWithTitle) <- c("subjectProfilePlot", class(plotSubjectWithTitle))
		
		plotSubjectWithTitle
		
	}, simplify = FALSE)

	return(listPlotsPerSubject)
	
}

#' Get limits for a list of plots.
#' 
#' These limits are extracted from the maximal range
#' of the x-coordinates across all plots
#' @param listPlots list of \code{subjectProfile[X]Plot} plots
#' @return vector of length 2 with limits for the x-axis
#' @importFrom ggplot2 ggplot_build
#' @author Laure Cougnaud
getXLimSubjectProfilePlots <- function(listPlots){
	
	xlimList <- lapply(listPlots, function(list)
		lapply(list, function(gg) 
			if(!inherits(gg, "subjectProfileTextPlot"))
				range(ggplot_build(gg)$data[[1]]$x)
		)
	)
	
	timeLim <- range(unlist(xlimList, recursive = TRUE), na.rm = TRUE)
	
	return(timeLim)
	
}

#' Add label of reference lines to a \code{\link[ggplot2]{ggplot}} object
#' @param gg \code{\link[ggplot2]{ggplot}} object
#' @param refLinesLabels character vector with label for the reference line(s)
#' @param refLinesTime numeric vector with time(x) coordinates for the reference line(s)
#' @param refLinesColor vector with color for the reference line(s)
#' @return \code{\link[gtable]{gtable}} objects,
#' with additional attributes 'nLinesLabelRefLines' containing the number of
#' lines for the label for the reference lines
#' @import ggplot2
#' @importFrom grid textGrob gpar
#' @author Laure Cougnaud
#' @export
addLabelReferenceLines <- function(gg, 
	refLinesLabels, refLinesTime,
	refLinesColor = "black"){
	
	nLinesRefLines <- max(nchar(refLinesLabels))
	
	if(length(refLinesColor) == 1)
		refLinesColor <- rep(refLinesColor, length(refLinesLabels))
	
	gg <- gg + theme(plot.margin = unit(c(1, 3, nLinesRefLines + 3, 1), "lines"))
	for(i in seq_along(refLinesLabels)){
		x <- refLinesTime[i]
		gg <- gg + annotation_custom(
			grob = textGrob(
				refLinesLabels[i], rot = 90, hjust = 1,
					gp = gpar(col = refLinesColor[i])
				), 
				xmin = x, xmax = x, ymin = -3, ymax = -3
		)
	}
	
	ggT <- ggplot_gtable(ggplot_build(gg))
	ggT$layout$clip[ggT$layout$name == "panel"] <- "off"
	
	#	grid.draw(ggT)
	
	attr(ggT, "nLinesLabelRefLines") <- nLinesRefLines
	
	return(ggT)
	
}