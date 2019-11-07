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
	timeLim = NULL,
	timeAlign = "all", timeAlignPerSubject = "none",
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
	
	if(is.null(timeLim))
		timeLim <- getXLimSubjectProfilePlots(
			listPlots, 
			timeAlign = timeAlign, timeAlignPerSubject = timeAlignPerSubject
		)
	
	# extract all subjects for which at least one plot is available
	subjects <- sort(unique(unlist(lapply(listPlots, names))))
	
	# create empty element in the list if the plot is not available for a certain subject
	listPlotsAll <- sapply(listPlots, function(x){
		list <- setNames(x[subjects], subjects) # in case plot not available for one subject
		list <- sapply(subjects, function(subj){
			structure(list[[subj]], metaData = list(subject = subj))
		}, simplify = FALSE)
		attr(list, 'metaData') <- attr(x, 'metaData')
		list
	}, simplify = FALSE)
	
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
#' @param timeLim time limits, as returned by the
#' \code{\link{getXLimSubjectProfilePlots}} function.
#' If NULL, the original time limits of the plot are used.
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
	
	listPlotsInit <- list(...)
	
	subjectID <- unique(unlist(lapply(listPlotsInit, function(x)	attr(x, "metaData")$subject)))
	
	if(is.null(subjectID) || length(subjectID) != 1)
		stop("Issue during extraction of subject IDs.")
	
	# 'empty' plot in case a specific plot is not available for this subject
	isEmpty <- which(sapply(listPlotsInit, is.null))
	if(length(isEmpty) > 0){
		listPlotsInit[isEmpty] <- lapply(isEmpty, function(i)
			if(labels[i] != ""){
				title <- paste("No", labels[i], "available.")
				gg <- ggplot() + theme_bw() + ggtitle(title)
				attr(gg, 'metaData') <- list(nLines = getNLinesLabel(value = title, elName = "title"))
				class(gg) <- c("subjectProfileEmptyPlot", class(gg))
				list(gg)
			}
		)
	}
	listPlotsInit <- listPlotsInit[!sapply(listPlotsInit, is.null)]
	
	plot <- if(length(listPlotsInit) > 0){
		
		listPlots <- unlist(listPlotsInit, recursive = FALSE)
		# retain module names (for timLim)
		names(listPlots) <- rep(names(listPlotsInit), sapply(listPlotsInit, length))
		
		# extract number of lines in the y-axis
		# from metadata or directly from the object if not present (slower)
		nLinesPlot <- unlist(lapply(listPlotsInit, function(x) 
			sapply(x, function(y){
				nLines <- attributes(y)$metaData$nLines
				if(is.null(nLines))	nLines <- getNLinesYGgplot(y)
				nLines 
			})
		))
		if(length(nLinesPlot) != length(listPlots))
			stop("Issue extracting number of lines for each plot.")
		
		# set same limits for the time/x-axis and reference lines
		plotsToModify <- which(sapply(listPlots, function(gg)
			!inherits(gg, c("subjectProfileEmptyPlot", "subjectProfileTextPlot"))
		))
		if(length(plotsToModify) > 0){
			
			# set same coordinates and include reference lines if any
			newPlots <- lapply(plotsToModify, function(i){
						
				gg <- listPlots[[i]]
				mod <- names(listPlots)[i]
				
				getTimeLim <- function(timeLim, el){
					if(is.list(timeLim)){
						if(el %in% names(timeLim)){
							getTimeLim(timeLim[[el]], el = subjectID)
						}
					}else	timeLim
				}
				getTimeLim
				
				# extract time limits for the module
				timeLimMod <- getTimeLim(timeLim, el = mod)
				if(!is.null(timeLimMod))
					gg <- gg + coord_cartesian(xlim = timeLimMod, default = TRUE)
				
				plot <- addReferenceLinesProfilePlot(
					gg = gg, 
					subjectVar = subjectVar,
					refLines = refLines,
					refLinesData = refLinesData,
					refLinesTimeVar = refLinesTimeVar,
					refLinesLabelVar = refLinesLabelVar,
					addLabel = (i == plotsToModify[length(plotsToModify)]),
					timeLim = timeLimMod
				)
				
			})
			
			# extract new plot(s) with possibly reference lines
			newPlotWithLabels <- which(!sapply(newPlots, inherits, "ggplot"))
			newPlotsGG <- newPlots
			if(length(newPlotWithLabels) > 0)
				newPlotsGG[newPlotWithLabels] <- lapply(newPlotsGG[newPlotWithLabels], function(x) x$gg)
			listPlots[plotsToModify] <- newPlotsGG
			
			# extract plot with label of reference lines
			if(length(newPlotWithLabels) > 0){
				plotRefLinesLabels <- lapply(newPlots[newPlotWithLabels], function(x) x$ggRefLines)[[1]]
				listPlots <- c(listPlots, list(plotRefLinesLabels))
				nLinesPlot <- c(nLinesPlot, attributes(plotRefLinesLabels)$metaData$nLinesLabelRefLines)
			}
			
		}
		
		attr(listPlots, 'metaData') <- list(nLines = nLinesPlot)
		listPlots
		
	}
	
	return(plot)
	
}