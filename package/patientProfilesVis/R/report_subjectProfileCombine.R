#' Combine subject profile plots.
#' @param listPlots list of subject profiles (modules/subjects)
#' @param shiny logical, set to TRUE (FALSE by default) if the report is generated from a Shiny application.
#' Messages during report creation will be included in the Shiny interface,
#' and it will be mentioned at the end of the report.
#' In this case, the \code{shiny} package should be available.
#' @return a list of \code{subjectProfilePlot} object, containing the combined
#' profile plots for each subject.
#' @importFrom cowplot ggdraw draw_label
#' @inheritParams prepareSubjectProfile
#' @inheritParams combineVerticallyGGplot
#' @author Laure Cougnaud
#' @export
subjectProfileCombine <- function(
	listPlots, 
	timeLim = getXLimSubjectProfilePlots(listPlots),
	subjectVar = "USUBJID",
	maxNLines = NULL,
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL,
	shiny = FALSE){
	
	if(shiny && !requireNamespace("shiny", quietly = TRUE))
		stop("The package 'shiny' is required to report progress.")
	
	# extract all subjects for which at least one plot is available
	subjects <- sort(unique(unlist(lapply(listPlots, names))))
	
	# create empty element in the list if the plot is not available for a certain subject
	listPlotsAll <- lapply(listPlots, function(x){
		list <- x[subjects]
		names(list) <- subjects # in case plot not available for one subject
		attr(list, 'metaData') <- attr(x, 'metaData')
		list
	})
	
	if(shiny)	incProgress(0.1, detail = "Create empty profile, set common time limits and reference lines.")
	
	# extract label
	plotLabels <- sapply(listPlotsAll, function(x){
		label <- attributes(x)$metaData$label
		ifelse(is.null(label), "", label)
	})
	
	# re-format plots: same timeLim, ...
	listPlotsPerSubject <- do.call(mapply, 
		c(
			list(
				FUN = prepareSubjectProfile, SIMPLIFY = FALSE, 
				MoreArgs = list(
					labels = plotLabels, 
					timeLim = timeLim,
					maxNLines = maxNLines,
					refLines = refLines, refLinesData = refLinesData, 
					refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
					subjectVar = subjectVar
				)
			),
			listPlotsAll
		)
	)
	
	# add title
	if(shiny)	incProgress(0.5, detail = "Combine profiles across subjects/modules.")
	listPlotsPerSubject <- combineVerticallyGGplot(listPlots = listPlotsPerSubject, maxNLines = maxNLines)
	
	return(listPlotsPerSubject)
	
}

#' prepare list of subject profile (s) to be combined with the \code{\link{combineVerticallyGGplot}} 
#' @param ... list of subject profiles (across modules)
#' @param labels string with labels for the plots
#' @inheritParams subjectProfileIntervalPlot
#' @inheritParams addReferenceLinesProfilePlot
#' @return \code{subjectProfilePlot} object, containing the combined
#' profile plots
#' @author Laure Cougnaud
#' @importFrom ggplot2 coord_cartesian ggplot theme_bw ggtitle
prepareSubjectProfile <- function(
	..., 
	labels, 
	timeLim = NULL, 
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL,
	subjectVar = "USUBJID"){
	
	listGgPlotsToCombineInit <- list(...)
	
	# 'empty' plot in case a specific plot is not available for this subject
	isEmpty <- which(sapply(listGgPlotsToCombineInit, is.null))
	if(length(isEmpty) > 0){
		listGgPlotsToCombineInit[isEmpty] <- lapply(isEmpty, function(i)
			if(labels[i] != ""){
				gg <- ggplot() + theme_bw() + 
					ggtitle(paste("No", labels[i], "available."))
				class(gg) <- c("subjectProfileEmptyPlot", class(gg))
				list(gg)
			}
		)
	}
	listGgPlotsToCombineInit <- listGgPlotsToCombineInit[!sapply(listGgPlotsToCombineInit, is.null)]
	
	plot <- if(length(listGgPlotsToCombineInit) > 0){
		
		listGgPlotsToCombine <- unlist(listGgPlotsToCombineInit, recursive = FALSE)
		
		# extract number of lines in the y-axis
		nLinesPlot <- sapply(listGgPlotsToCombine, getNLinesYGgplot)
		
		# set same limits for the time/x-axis and reference lines
		plotsToModify <-  which(sapply(listGgPlotsToCombine, function(gg)
			!inherits(gg, "subjectProfileTextPlot") & !inherits(gg, "subjectProfileEmptyPlot")
		))
		if(length(plotsToModify) > 0){
			
			# set same coordinates and include reference lines if any
			newPlots <- lapply(plotsToModify, function(i){
						
				gg <- listGgPlotsToCombine[[i]]
				if(!is.null(timeLim))	
					gg <- gg + coord_cartesian(xlim = timeLim, default = FALSE)
				
				plot <- addReferenceLinesProfilePlot(
					gg = gg, 
					subjectVar = subjectVar,
					refLines = refLines,
					refLinesData = refLinesData,
					refLinesTimeVar = refLinesTimeVar,
					refLinesLabelVar = refLinesLabelVar,
					addLabel = (i == plotsToModify[length(plotsToModify)]),
					timeLim = timeLim
				)
				
			})
			
			# extract new plot(s) with possibly reference lines
			newPlotWithLabels <- which(!sapply(newPlots, inherits, "ggplot"))
			newPlotsGG <- newPlots
			if(length(newPlotWithLabels) > 0)
				newPlotsGG[newPlotWithLabels] <- lapply(newPlotsGG[newPlotWithLabels], function(x) x$gg)
			listGgPlotsToCombine[plotsToModify] <- newPlotsGG
			
			# extract plot with label of reference lines
			if(length(newPlotWithLabels) > 0){
				plotRefLinesLabels <- lapply(newPlots[newPlotWithLabels], function(x) x$ggRefLines)[[1]]
				listGgPlotsToCombine <- c(listGgPlotsToCombine, list(plotRefLinesLabels))
				nLinesPlot <- c(nLinesPlot, attributes(plotRefLinesLabels)$metaData$nLinesLabelRefLines)
			}
			
		}
		
		attr(listGgPlotsToCombine, 'metaData') <- list(nLines = nLinesPlot)
		listGgPlotsToCombine
		
	}
	
	return(plot)
	
}