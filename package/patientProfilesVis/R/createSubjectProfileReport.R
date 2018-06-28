
createSubjectProfileReport <- function(listPlots, 
	timeLim = getXLimSubjectProfilePlots(listPlots),
	landscape = FALSE,
	outputFile = "subjectProfile.pdf"){
	
	# combine
	listPlotsPerSubject <- subjectProfileCombine(listPlots, timeLim = timeLim)
	
	pathTemplate <- getPathTemplate("subjectProfile.Rnw")
	
	## convert Rnw -> tex
	outputFileKnitr <- knitr::knit(input = pathTemplate)
	
	## convert tex -> pdf
	
	# texi2pdf cannot deal with space in name and file should be in current directory
	oldwd <- getwd()
	setwd(outputDir)
	
	# convert to pdf
	texi2pdf(file = outputFileKnitr, clean = TRUE)
	
	# rename output file
	outputTexi2pdf <- paste0(file_path_sans_ext(outputFileKnitr), ".pdf")
	pdfPath <- file.path(pathFiguresPdf, paste0(pathCaptionShort, ".pdf"))
	file.rename(outputTexi2pdf, to = outputFile)
	
	setwd(oldwd)
	
}

#' Combine subject profile plots.
#' @param listPlots list of \code{\link[ggplot2]{ggplot2} objects}
#' @return a list of \code{subjectProfilePlot} object, containing the combined
#' profile plots for each subject.
#' This is in essence only a \code{\link[ggplot2]{ggplot2}} objects,
#' (with the additional attribute 'nLinesYAxis')
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @importFrom ggplot2 ggplotGrob ggplot_build
#' @inheritParams subjectProfileRangePlot
#' @author Laure Cougnaud
#' @export
subjectProfileCombine <- function(listPlots, 
	timeLim = getXLimSubjectProfilePlots(listPlots)){
	
	# extract all subjects for which at least one plot is available
	subjects <- sort(unique(unlist(lapply(listPlots, names))))
	
	# create empty element in the list if the plot is not available for a certain subject
	listPlotsAll <- lapply(listPlots, function(x){
		list <- x[subjects]
		names(list) <- subjects # in case plot not available for one subject
		list
	})
	
	## combine plots
	
	# wrapper function to combine ggplot2 objects
	combineGGPlots <- function(...){
		
		listGgPlotsToCombine <- list(...)
		listGgPlotsToCombine <- listGgPlotsToCombine[!sapply(listGgPlotsToCombine, is.null)] # remove empty plots
		if(length(listGgPlotsToCombine) > 0){
			
			# set same limits for the time/x-axis
			listGgPlotsToCombine <- lapply(listGgPlotsToCombine, function(gg){
				if(!inherits(gg, "subjectProfileTextPlot"))		
					gg <- gg + coord_cartesian(x = timeLim)
				gg				
			})
	
			# extract number of lines in the y-axis
			nLinesYAxis <- sapply(listGgPlotsToCombine, function(gg)
				length(unique(ggplot_build(gg)$data[[1]]$y))
			)
			# relative height of each plot
			relHeights <- nLinesYAxis/sum(nLinesYAxis)
			
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
			# TODO: add plots title?
			attributes(plot) <- c(attributes(plot), list(nLinesYAxis = sum(nLinesYAxis)))
			
			class(plot) <- c("subjectProfilePlot", class(plot))
			
			plot
			
		}
		
	}
	
	# combine all plots per subject
	# this returns a list of 'gtable' object
	listPlotsPerSubject <- do.call(mapply, 
		c(
			list(FUN = combineGGPlots, SIMPLIFY = FALSE),
			listPlotsAll
		)
	)
		
	# add title
	listPlotsPerSubject <- sapply(names(listPlotsPerSubject), function(subject){
		
		# create a ggplot for title only
		title <- ggdraw() + 
			draw_label(paste("Patient:", subject), fontface = 'bold', 
				x = 0, hjust = 0, lineheight = 2
			)
		
		plotSubject <- listPlotsPerSubject[[subject]]
		# extract number of lines in main plot...
		nLinesYAxisPlot <- attr(plotSubject, "nLinesYAxis")
		# to get relative height of title/main plot
		relHeights <- c(2, nLinesYAxisPlot)/(2+nLinesYAxisPlot)
		
		# combine title and plot
		plotSubjectWithTitle <- plot_grid(title, plotSubject, ncol = 1, rel_heights = relHeights)
		
		# store the number of lines in the y-axis (used to adapt size during export)
		attributes(plotSubjectWithTitle) <- c(
			attributes(plotSubjectWithTitle), 
			list(nLinesYAxis = nLinesYAxis))
		class(plotSubjectWithTitle) <- c("subjectProfilePlot", class(plotSubjectWithTitle))
		
		plotSubjectWithTitle
		
	})

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
#' @export
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