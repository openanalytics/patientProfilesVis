#' Combine subject profile plots.
#' @param listPlots list of subject profiles (modules/subjects)
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
	timeLim = getXLimSubjectProfilePlots(listPlots, align = timeAlign),
	timeAlign = TRUE,
	subjectVar = "USUBJID",
	maxNLines = NULL,
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL,
	shiny = FALSE, verbose = FALSE,
	nCores = 1){
	
	if(shiny && !requireNamespace("shiny", quietly = TRUE))
		stop("The package 'shiny' is required to report progress.")
	
	if(verbose)	message("Extract available subjects.")
	
	# extract all subjects for which at least one plot is available
	subjects <- sort(unique(unlist(lapply(listPlots, names))))
	
	# create empty element in the list if the plot is not available for a certain subject
	listPlotsAll <- lapply(listPlots, function(x){
		list <- x[subjects]
		names(list) <- subjects # in case plot not available for one subject
		attr(list, 'metaData') <- attr(x, 'metaData')
		list
	})
	
	msgProgress <- "Create empty profile, set common time limits and reference lines."
	if(verbose)	message(msgProgress)
	if(shiny)	incProgress(0.1, detail = msgProgress)
	
	# extract label
	if(verbose)	message("Extract plot labels.")
	plotLabels <- sapply(listPlotsAll, function(x){
		label <- attributes(x)$metaData$label
		ifelse(is.null(label), "", label)
	})
	
	# re-format plots: same timeLim, ...
	if(verbose)	message("Prepare subject profiles to be combined.")
	listPlotsPerSubject <- do.call(mapply, 
		c(
			list(
				FUN = prepareSubjectProfile, SIMPLIFY = FALSE, 
				MoreArgs = list(
					labels = plotLabels, 
					timeLim = timeLim,
					refLines = refLines, refLinesData = refLinesData, 
					refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
					subjectVar = subjectVar,
					timeAlign = timeAlign
				)
			),
			listPlotsAll
		)
	)	
	
	# for debugging:
#	sapply(names(listPlotsAll[[1]]), function(id){
#		print(id)
#		argsPSP <- c(
#			list(
#				labels = plotLabels, 
#				timeLim = timeLim,
#				refLines = refLines, refLinesData = refLinesData, 
#				refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
#				subjectVar = subjectVar
#			),
#			sapply(listPlotsAll, function(x)	x[[id]], simplify = FALSE)
#		)
#		do.call(prepareSubjectProfile, argsPSP)
#	})
	
	# add title
	listPlotsPerSubject <- combineVerticallyGGplot(
		listPlots = listPlotsPerSubject, 
		maxNLines = maxNLines,
		verbose = verbose,
		shiny = shiny,
		nCores = nCores
	)
	
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
	timeAlign = TRUE,
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
				title <- paste("No", labels[i], "available.")
				gg <- ggplot() + theme_bw() + ggtitle(title)
				attr(gg, 'metaData') <- list(nLines = getNLinesLabel(value = title, elName = "title"))
				class(gg) <- c("subjectProfileEmptyPlot", class(gg))
				list(gg)
			}
		)
	}
	listGgPlotsToCombineInit <- listGgPlotsToCombineInit[!sapply(listGgPlotsToCombineInit, is.null)]
	
	plot <- if(length(listGgPlotsToCombineInit) > 0){
		
		listGgPlotsToCombine <- unlist(listGgPlotsToCombineInit, recursive = FALSE)
		
		# extract number of lines in the y-axis
		# from metadata or directly from the object if not present (slower)
		nLinesPlot <- unlist(lapply(listGgPlotsToCombineInit, function(x) 
			sapply(x, function(y){
				nLines <- attributes(y)$metaData$nLines
				if(is.null(nLines))	nLines <- getNLinesYGgplot(y)
				nLines 
			})
		))
		if(length(nLinesPlot) != length(listGgPlotsToCombine))
			stop("Issue extracting number of lines for each plot.")
		
		# set same limits for the time/x-axis and reference lines
		plotsToModify <-  which(sapply(listGgPlotsToCombine, function(gg)
			!inherits(gg, "subjectProfileTextPlot") & !inherits(gg, "subjectProfileEmptyPlot")
		))
		if(length(plotsToModify) > 0){
			
			# set same coordinates and include reference lines if any
			newPlots <- lapply(plotsToModify, function(i){
						
				gg <- listGgPlotsToCombine[[i]]
				if(!is.null(timeLim))	
					gg <- gg + coord_cartesian(xlim = timeLim, default = TRUE)
				
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