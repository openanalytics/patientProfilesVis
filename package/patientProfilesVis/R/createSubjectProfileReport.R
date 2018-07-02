#' Create subject profile report
#' @param listPlots list of plots, as returned by the \code{\link{subjectProfileTextPlot}},
#' \code{\link{subjectProfileEventPlot}} or \code{\link{subjectProfileIntervalPlot}}
#' @param timeLim vector with time range
#' @param landscape logical, if TRUE the created report is in landscape format
#' @param outputFile string, path to the output report
#' @return no returned value, the report is created at the location
#' specified by \code{outputFile}
#' @author Laure Cougnaud
#' @importFrom tools texi2pdf
#' @export
createSubjectProfileReport <- function(listPlots, 
	timeLim = getXLimSubjectProfilePlots(listPlots),
	landscape = FALSE,
	outputFile = "subjectProfile.pdf",
	exportFigures = FALSE){
	
	# combine
	listPlotsPerSubject <- subjectProfileCombine(listPlots, timeLim = timeLim)
	
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
#' @return a list of \code{subjectProfilePlot} object, containing the combined
#' profile plots for each subject.
#' This is in essence only a \code{\link[ggplot2]{ggplot2}} objects,
#' (with the additional attribute 'nLinesPlot')
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @importFrom ggplot2 coord_cartesian
#' @inheritParams subjectProfileIntervalPlot
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
					gg <- gg + coord_cartesian(xlim = timeLim)
				gg				
			})
	
			# extract number of lines in the y-axis
			nLinesPlot <- sapply(listGgPlotsToCombine, getNLinesYGgplot)

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
			# TODO: add plots title?
			attributes(plot) <- c(attributes(plot), 
				list(nLinesPlot = sum(nLinesPlot), nSubplots = length(listGgPlotsToCombine))
			)
			
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
		
		# store the number of lines in the y-axis (used to adapt size during export)
		attributes(plotSubjectWithTitle) <- c(
			attributes(plotSubjectWithTitle), 
			list(
				nLinesPlot = nLinesPlot,
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