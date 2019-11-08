#' Combine subject profile plots.
#' @param listPlots list of subject profiles (modules/subjectsID)
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
	
	if(is.null(timeLim)){
		msgProgress <- "Get limits x-axis."
		if(verbose)	message(msgProgress)
		if(shiny)	incProgress(0, detail = msgProgress)
		timeLim <- getXLimSubjectProfilePlots(
			listPlots, 
			timeAlign = timeAlign, 
			timeAlignPerSubject = timeAlignPerSubject
		)
	}
	
	# extract all subjectsID for which at least one plot is available
	if(verbose)	message("Extract available subjectsID.")
	subjectsID <- sort(unique(unlist(lapply(listPlots, names))))
	
	# create empty element in the list if the plot is not available for a certain subject
	msgProgress <- "Create empty profile."
	if(verbose)	message(msgProgress)
	if(shiny)	incProgress(0.1, detail = msgProgress)
	listPlotsAll <- sapply(listPlots, function(x){
		list <- setNames(x[subjectsID], subjectsID) # in case plot not available for one subject
		list <- sapply(subjectsID, function(subj){
			structure(list[[subj]], metaData = list(subject = subj))
		}, simplify = FALSE)
		attr(list, 'metaData') <- attr(x, 'metaData')
		list
	}, simplify = FALSE)
	
	# extract label
	msgProgress <- "Extract plot labels."
	if(verbose)	message(msgProgress)
	if(shiny)	incProgress(0, detail = msgProgress)
	plotLabels <- sapply(listPlotsAll, function(x){
		label <- attributes(x)$metaData$label
		ifelse(is.null(label), "", label)
	})

	# get required time transformation for each module
	msgProgress <- "Check time transformations."
	if(verbose)	message(msgProgress)
	if(shiny)	incProgress(0, detail = msgProgress)
	timeTrans <- checkTimeTrans(listPlots = listPlotsAll, timeLim = timeLim)

	# re-format plots: same timeLim, ...
	msgProgress <- "Prepare subject profiles to be combined."
	if(verbose)	message(msgProgress)
	if(shiny)	incProgress(0, detail = msgProgress)
	listPlotsPerSubject <- do.call(mapply, 
		c(
			list(
				FUN = prepareSubjectProfile, SIMPLIFY = FALSE, 
				MoreArgs = list(
					labels = plotLabels, 
					refLines = refLines, refLinesData = refLinesData, 
					refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
					subjectVar = subjectVar,
					timeLim = timeLim, timeTrans = timeTrans
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
#' @param timeTrans Time transformation, or list of such transformation
#' named by module. If NULL, no transformation are done.
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
	subjectVar = "USUBJID",
	timeTrans = NULL){
	
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
		plotsToModify <- which(sapply(listPlots, isSubjectProfileTimeVariant, empty = TRUE))
		if(length(plotsToModify) > 0){
			
			# set same coordinates and include reference lines if any
			newPlots <- lapply(plotsToModify, function(i){
						
				gg <- listPlots[[i]]
				mod <- names(listPlots)[i]

				# set time transformation
				timeTransMod <- if(!is.null(timeTrans)){
					if(is.list(timeTrans)){
						if(mod %in% names(timeTrans))	timeTrans[[mod]]
					}else timeTrans
				}
				if(!is.null(timeTransMod))
					gg <- gg + scale_x_continuous(trans = timeTransMod)
				
				# extract time limits for the module
				getTimeLim <- function(timeLim, el){
					if(is.list(timeLim)){
						if(el %in% names(timeLim)){
							getTimeLim(timeLim[[el]], el = subjectID)
						}
					}else	timeLim
				}
				timeLimMod <- getTimeLim(timeLim, el = mod)
				if(!is.null(timeLimMod))
					gg <- gg + coord_cartesian(xlim = timeLimMod, default = TRUE)
		
				# add reference lines
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



#' Check if some of the modules are time transformed,
#' and if some of the plots to align (with specified \code{timeLim})
#' don't have/have different alignment options.
#' @param listPlots 
#' @param timeLim 
#' @return List of time transformation for each module
#' (named by \code{listPlots})
#' @author Laure Cougnaud
checkTimeTrans <- function(listPlots, timeLim = NULL){
	
	# Set time transformation, if some modules
	# have time transformation but not all (or not the same )
	timeTransMod <- sapply(listPlots, function(x) 
		attr(x, "metaData")$timeTrans, 
		simplify = FALSE
	)	
	timeTrans <- NULL
	if(!is.null(timeLim)){
		
		# consider only the modules to be aligned
		if(is.list(timeLim))
			timeTransMod <- timeTransMod[names(timeLim)]
		# and time variant
		modTimeVariant <- names(which(sapply(listPlots, isSubjectProfileTimeVariant, empty = FALSE)))
		timeTransMod <- timeTransMod[modTimeVariant]
		
		checkFctId <- function(list)	
			if(length(list) > 1){
				all(mapply(identical, head(list, -1), tail(list, -1)))
			}else	TRUE
		
		# if the transformation is not the same for all modules to be aligned
		if(!checkFctId(timeTransMod)){
			
			# if only one transformation is specified
			timeTransModSpec <- timeTransMod[!sapply(timeTransMod, is.null)]
			if(checkFctId(timeTransModSpec)){
				
				timeTransUsed <- timeTransModSpec[[1]]	
				# set new transformation for all modules without this transformation
				timeTrans <- if(is.list(timeLim)){
					modToTrans <- setdiff(names(timeLim), names(timeTransModSpec))
					if(length(modToTrans) > 0){
						modToTransST <- paste("Modules:", toString(modToTrans))
						setNames(replicate(length(modToTrans), timeTransUsed, simplify = FALSE), modToTrans)
					}
				}else{
					modToTransST <- "All modules"
					timeTransUsed
				}
				if(!is.null(timeTrans))
					message(paste(modToTransST,
						"is transformed with", timeTransUsed$name, 
						"to be time aligned."
					))
				
				# if different time transformations are used,
				# the modules cannot be aligned
			}else{
				
				timeTransModName <- sapply(timeTransMod, function(x) ifelse(is.null(x), "none", x$name))
				timeTransModNameST <- toString(paste(names(timeTransModName), timeTransModName, sep = ": "))
				stop("Different time transformations are ",
					"specified for different modules to align: ",
					timeTransModNameST, ", so these modules cannot be correctly ",
					"aligned."
				)
			}
		}
		
	}
	
	return(timeTrans)
	
}