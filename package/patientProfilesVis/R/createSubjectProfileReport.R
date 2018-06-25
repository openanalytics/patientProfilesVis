
createSubjectProfileReport <- function(listPlots){
	
	listPlotsCombine <- subjectProfileCombine(listPlots)
	
}

#' Combine subject profile plots.
#' @param listPlots list of \code{\link[ggplot2]{ggplot2} objects}
#' @return a list of \code{subjectProfilePlot} object, containing the combined
#' profile plots for each subject.
#' This is in essence only a \code{\link[gtable]{gtable}} object,
#' wrapped within a \code{subjectProfilePlot} class for dedicated
#' 'print' function.
#' @importFrom gridExtra gtable_combine
#' @importFrom ggplot2 ggplotGrob ggplot_build
#' @importFrom grid unit
#' @importFrom gridExtra gtable_combine
#' @author Laure Cougnaud
#' @export
subjectProfileCombine <- function(listPlots){
	
	# extract all subjects for which at least one plot is available
	subjects <- sort(unique(unlist(lapply(listPlots, names))))
	
	# create empty element in the list if the plot is not available for a certain subject
	listPlotsAll <- lapply(listPlots, function(x) x[subjects])
	
	## combine plots
	
	# wrapper function to combine ggplot2 objects
	combineGGPlots <- function(...){
		
		listGgPlotsToCombine <- list(...)
		listGgPlotsToCombine <- listGgPlotsToCombine[!sapply(listGgPlotsToCombine, is.null)] # remove empty plots
		if(length(listGgPlotsToCombine) > 0){
			
			listGrobPlots <- lapply(listGgPlotsToCombine, function(gg){
					
				# convert ggplot2 -> gtable 
				ggGrob <- ggplotGrob(gg) 
				
				## scale each plot to have height set to the number of lines in the y-axis
				
				# extract number of lines in the y-axis
				nLinesYAxis <- nrow(ggplot_build(gg)$data[[1]])
				# get vertical index of panel
				idxVPanel <- ggGrob$layout[ggGrob$layout$name == "panel", "t"]
				# set the height of the panel to the number of lines in the plot
				ggGrob$heights[[idxVPanel]] <- unit(nLinesYAxis, "lines")
				
				colnames(ggGrob) <- paste0(seq_len(ncol(ggGrob)))
				ggGrob
				
			})
	
			plot <- do.call(gtable_combine, c(listGrobPlots, list(along = 2)))
			class(plot) <- c("subjectProfilePlot", class(plot))
			plot
		}
		
	}
	
	# combine all plots per subject
	# this returns a list of 'gtable' object
	listPlotsPerSubject <- do.call(mapply, c(list(FUN = combineGGPlots), listPlotsAll))
	
	return(listPlotsPerSubject)
	
}

#' convenience function to print a subject profile plot (of class \code{gtable}).
#' This plot is printed with the \code{grid.draw} function
#' @param x object of class \code{subjectProfilePlot}
#' @param ... additional arguments, currently not used
#' @return no returned value, a plot is drawn in the current window
#' @importFrom grid grid.newpage grid.draw
#' @S3method print subjectProfilePlot
#' @export
#' @author Laure Cougnaud
print.subjectProfilePlot <- function(x, ...){
	grid.newpage();grid.draw(x)
}