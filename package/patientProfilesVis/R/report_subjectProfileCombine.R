#' Combine subject profile plots.
#' 
#' Visualizations of different modules are combined
#' by subject.
#' The plots are aligned in the time axis (if requested).
#' If the plots should be aligned:
#' \itemize{
#' \item{the same time limits are set for all plots}
#' \item{the time axis is transformed if any of the
#' plot was created with a time transformation}
#' \item{the time axis is expanded for all plots 
#' if any of the plot was created with a time axis expanded.\cr
#' The \code{\link[ggplot2]{expansion}} object for
#' the combined plot is created 
#' from the max of each expansion element across modules.}
#' }
#' If some plots are missing for a specific subject,
#' an empty plot is created, containing information
#' as a text based on the \code{label} with which the plot was created.
#' @return a nested list of \code{\link[ggplot2]{ggplot}} object, 
#' containing the combined
#' profile plots across modules for each subject/page.\cr
#' Each plot object contains in the associated attribute:
#' \code{metaData} containing: \code{nLines}: an estimation
#' of the number of 'lines' each plot occupies 
#' (e.g. to set height of the exported figure).
#' @inheritParams prepareSubjectProfile
#' @inheritParams combineVerticallyGGplot
#' @inheritParams getTimeLimSubjectProfilePlots
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
	nCores = 1,
	reportPerSubject = FALSE){
	
	if(shiny && !requireNamespace("shiny", quietly = TRUE))
		stop("The package 'shiny' is required to report progress.")
	
	if(is.null(timeLim)){
		msgProgress <- "Get limits x-axis."
		if(verbose)	message(msgProgress)
		if(shiny)	shiny::incProgress(0, detail = msgProgress)
		timeLim <- getTimeLimSubjectProfilePlots(
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
	if(shiny)	shiny::incProgress(0.1, detail = msgProgress)
	listPlotsAll <- sapply(listPlots, function(x){
		list <- setNames(x[subjectsID], subjectsID) # in case plot not available for one subject
		list <- sapply(subjectsID, function(subj){
			xObj <- if(!is.null(list[[subj]])){
				list[[subj]]
			}else	list()
			structure(xObj, metaData = list(subject = subj))
		}, simplify = FALSE)
		attr(list, 'metaData') <- attr(x, 'metaData')
		list
	}, simplify = FALSE)
	
	# extract label
	msgProgress <- "Extract plot labels."
	if(verbose)	message(msgProgress)
	if(shiny)	shiny::incProgress(0, detail = msgProgress)
	plotLabels <- sapply(listPlotsAll, function(x){
		label <- attributes(x)$metaData$label
		ifelse(is.null(label), "", label)
	})

	# get required time transformation for each module
	msgProgress <- "Check time transformations."
	if(verbose)	message(msgProgress)
	if(shiny)	shiny::incProgress(0, detail = msgProgress)
	timeTrans <- checkTimeTrans(listPlots = listPlotsAll, timeLim = timeLim)
	
	# get required time expand for each module
	msgProgress <- "Check time expand."
	if(verbose)	message(msgProgress)
	if(shiny)	shiny::incProgress(0, detail = msgProgress)
	timeExpand <- checkTimeExpand(listPlots = listPlotsAll, timeLim = timeLim)

	# re-format plots: same timeLim, ...
	msgProgress <- "Prepare subject profiles to be combined."
	if(verbose)	message(msgProgress)
	if(shiny)	shiny::incProgress(0, detail = msgProgress)
	listPlotsPerSubject <- do.call(mapply, 
		c(
			list(
				FUN = prepareSubjectProfile, SIMPLIFY = FALSE, 
				MoreArgs = list(
					labels = plotLabels, 
					refLines = refLines, refLinesData = refLinesData, 
					refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
					subjectVar = subjectVar,
					timeLim = timeLim, timeTrans = timeTrans,
					timeExpand = timeExpand
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
		nCores = nCores,
		reportPerSubject = reportPerSubject
	)
	
	return(listPlotsPerSubject)
	
}

#' prepare list of subject profile (s) to be combined with the \code{\link{combineVerticallyGGplot}} 
#' @param timeLim time limits, as returned by the
#' \code{\link{getTimeLimSubjectProfilePlots}} function.
#' @param timeTrans Time transformation, or list of such transformation
#' named by module. If NULL, no transformation are done.
#' @param ... list of subject profiles (across modules)
#' @param labels string with labels for the plots
#' @inheritParams patientProfilesVis-common-args
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
	timeTrans = NULL,
	timeExpand = NULL){
	
	listPlotsInit <- list(...)
	
	subjectID <- unique(unlist(lapply(listPlotsInit, function(x)	attr(x, "metaData")$subject)))
	
	if(is.null(subjectID) || length(subjectID) != 1)
		stop("Issue during extraction of subject IDs.")
	
	# 'empty' plot in case a specific plot is not available for this subject
	isEmpty <- which(sapply(listPlotsInit, function(x) is.null(x) | length(x) == 0))
	if(length(isEmpty) > 0){
		listPlotsInit[isEmpty] <- lapply(isEmpty, function(i)
			if(labels[i] != ""){
				title <- paste("No", labels[i], "available.")
				gg <- ggplot() + theme_bw() + theme(panel.border = element_blank()) + ggtitle(title)
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
				if(is.null(nLines))	nLines <- getNLinesSubjectProfile(y)
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
					if(inherits(timeTrans, "trans")){
						timeTrans
					}
				}
				# set time expand
				timeExpandMod <- if(!is.null(timeExpand)){
					if(is.list(timeExpand)){
						if(mod %in% names(timeExpand))	timeExpand[[mod]]
					}else timeExpand
				}
				argsScaleX <- c(
					if(!is.null(timeExpandMod))	list(expand = timeExpandMod),
					if(!is.null(timeTransMod))	list(trans = timeTransMod)
				)
				# warning removed because message already include at checkTime[Trans|Expand]
				if(length(argsScaleX) > 0)
					suppressMessages(
						gg <- gg + do.call("scale_x_continuous", argsScaleX)
					)
				
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

#' Get the limits to set for the subject profile plots,
#' depending on the alignment policy set.
#' 
#' These limits are extracted from specified \code{timeLim} for each
#' module (stored in the \code{attributes()$metaData$timeLim}), 
#' and if empty for all modules: from the maximal range
#' of the x-coordinates across all plots.
#' @param listPlots list of list of \code{subjectProfile[X]Plot} plots
#' @param timeAlign Character vector with time alignment across modules/subjects, either:
#' \itemize{
#' \item{'all' (by default): }{all plots have the same time limits}
#' \item{'none': }{each of the plot (module*subject) has its own time limits}
#' \item{character vector with names of the modules which
#' should have the same time limits
#' (should correspond to the names of \code{listPlots})}
#' }
#' @param timeAlignPerSubject Character vector, specifying if the plots
#' should be aligned (or not) across subjects
#' \itemize{
#' \item{'none' (by default): }{all modules to align
#' have the same time limit across subjects}
#' \item{'all': }{all modules to align
#'  have different time limits per subject}
#' \item{character vector with subset of the modules to align per subject
#' (should correspond to the names of \code{listPlots})}
#' }
#' Only the modules already specified in \code{timeAlign}
#' can be aligned by subject.
#' @return Time limits, as a numeric vector of length 2.
#' If time limits should be set by module, named list
#' with time limits by module.
#' If time limits should be set by module and subject, nested list
#' with time limits 1) by module 2) by subject.\cr
#' The names of the list contains the module/subject name extracted
#' from the names of \code{listPlots}.\cr
#' The time limits are only returned if they will need to be explicitly set
#' for a plot. Otherwise, NULL is returned.
#' @importFrom ggplot2 ggplot_build
#' @author Laure Cougnaud
getTimeLimSubjectProfilePlots <- function(
	listPlots, 
	timeAlign = "all", timeAlignPerSubject = "none"){
	
	if(is.logical(timeAlign)){
		
		warning(
			"'timeAlign' as a logical is deprecated, ",
			"please use instead: timeAlign = 'all' if TRUE or 'none' if FALSE."
		)
		timeAlign <- ifelse(timeAlign, "all", "none")
		
	}
	
	if(length(timeAlign) == 1 && timeAlign == "none"){
		
		timeLim <- NULL
		
	}else{
		
		if(is.null(names(listPlots)))
			stop("'listPlots' should be named if time alignment is required.")
		
		if(any(duplicated(names(listPlots))))
			stop("'listPlots' should be have unique names if time alignment is required.")
		
		modTimeVariant <- names(which(sapply(listPlots, isSubjectProfileTimeVariant, empty = FALSE)))
		
		if(length(timeAlign) == 1 && timeAlign == "all"){
			
			alignMod <- modTimeVariant
			
		}else{
			
			alignModulesNA <- setdiff(timeAlign, names(listPlots))
			if(length(alignModulesNA > 0)){
				warning(paste("Module(s) to align:", toString(alignModulesNA), "are not available",
					"in the names of the list of plots."))
			}
			alignModuleNotTV <- setdiff(timeAlign, modTimeVariant)
			if(length(alignModuleNotTV) > 0){
				warning(paste("Module(s) to align:", toString(alignModuleNotTV), "are not",
					"time variant, so these won't be aligned."))
			}
			
			alignMod <- intersect(timeAlign, modTimeVariant)
			
		}
		
		if(length(alignMod) > 0){
			
			# extract modules that should be aligned per subject
			alignPerSubjectMod <- if(length(timeAlignPerSubject) == 1){
				switch(
					timeAlignPerSubject,
					'all' = names(listPlots),
					'none' = character(),
					timeAlignPerSubject
				)
			}else	timeAlignPerSubject
			
			alignPerSubjectModNA <- setdiff(alignPerSubjectMod, alignMod)
			if(!(length(timeAlignPerSubject) == 1 && timeAlignPerSubject == "all") &
				length(alignPerSubjectModNA) > 0){
				warning(paste("Module(s):", toString(alignPerSubjectModNA),
					"should be aligned per subject, but are not specified/available among",
					"the modules to align, so these are ignored."))
			}
			alignPerSubjectMod <- intersect(alignPerSubjectMod, alignMod)
			
			# create empty element in case subject not present in one of the module
			# for the 'mapply' below...
			subjectsID <- unique(unlist(lapply(listPlots, names)))
			listPlotsAll <- sapply(listPlots, function(x){
				list <- setNames(x[subjectsID], subjectsID) # in case plot not available for one subject
				list <- sapply(subjectsID, function(subj){
					if(!is.null(list[[subj]])){
						structure(list[[subj]], metaData = list(subject = subj))
					}
				}, simplify = FALSE)
				attr(list, 'metaData') <- attr(x, 'metaData')
				list
			}, simplify = FALSE)
			
			# compute time limits for each module and subject
			timeLimPlotsSubj <- sapply(alignMod, function(mod){
						
				listPlotsMod <- listPlotsAll[[mod]]
				
				# extract time limits were specified for a specific plot
				timeLimPlots <- attributes(listPlotsMod)$metaData$timeLim
				
				# if time limits not specified
				if(is.null(timeLimPlots)){
					
					timeTrans <- attr(listPlotsMod, "metaData")$timeTrans
					
					timeLimDataSubject <- sapply(listPlotsMod, function(listPlotsSubj){
								
						# extract time limits for all elements
						timeLimDataList <- lapply(listPlotsSubj, function(gg)
							if(!inherits(gg, "subjectProfileTextPlot"))
								lapply(ggplot_build(gg)$data, function(dataPlot){
									xPlot <- c(dataPlot$x, if("xend" %in% colnames(dataPlot))	dataPlot$xend)
									xPlot[!is.na(xPlot)]
								})
						)
						timeLimData <- unlist(timeLimDataList)
						if(length(timeLimData) == 0)	timeLimData <- NULL
						
						if(!is.null(timeLimData)){
							
							# apply transformation if specified
							if(!is.null(timeTrans)){
								if(is.function(timeTrans$inverse)){
									timeLimData <- timeTrans$inverse(timeLimData)
								}else{
									warning(paste("Time transformation in module", mod,
										"not available as a function,",
										"so time limits for plots of this module are not considered."))
									timeLimData <- NULL
								}
							}
							
							# extract time limits
							if(!is.null(timeLimData))	range(timeLimData, na.rm = TRUE)
							
						}

					}, simplify = FALSE)
				
				}else{
					
					if(length(alignPerSubjectMod))
						warning(paste("Alignment per subject is not available for module", mod,
							"because time limits were specified during module creation."))
					
					# if timeLim same across subjects: replicate
					if(!is.list(timeLimPlots)){
						setNames(
							replicate(length(listPlotsMod), timeLimPlots, simplify = FALSE),
							names(listPlotsMod)
						)
					# if not (e.g. one of the specified timeLim is missing)
					}else	timeLimPlots[names(listPlotsMod)]
					
				}
				
			}, simplify = FALSE)
			
			# extract limits across modules for each subject
			getRangeCustom <- function(...){
				x <- unlist(list(...))
				if(!is.null(x)){range(x, na.rm = TRUE)}
			}
			timeLimPerSubj <- do.call(mapply, 
				c(
					timeLimPlotsSubj[alignMod],
					list(FUN = getRangeCustom, SIMPLIFY = FALSE)
				)
			)
			timeLim <- setNames(
				replicate(length(alignMod), timeLimPerSubj, simplify = FALSE),
				alignMod
			)
			
			# if alignment across subjects
			# should extract limits across subjects for each module
			alignAcrossSubjectMod <- setdiff(alignMod, alignPerSubjectMod)
			getRangeList <- function(...){
				if(all(sapply(..., is.null))){
					NULL
				}else	range(..., na.rm = TRUE)
			}
			if(length(alignAcrossSubjectMod) > 0)
				timeLim[alignAcrossSubjectMod] <- sapply(timeLim[alignAcrossSubjectMod], function(x){
					getRangeList(unlist(x)) 
				}, simplify = FALSE)
			
		}else	timeLim <- NULL
		
	}
	
	return(timeLim)
	
}

#' Check if some of the modules are time expanded,
#' and extract maximum time expand for each module.
#' @inheritParams checkTimeTrans
#' @return List of time expand for each module
#' (named by \code{listPlots})
#' @importFrom utils head tail
#' @author Laure Cougnaud
checkTimeExpand <- function(listPlots, timeLim = NULL){
	
	# Set time transformation, if some modules
	# have time transformation but not all (or not the same )
	timeExpandMod <- sapply(listPlots, function(x) 
		attr(x, "metaData")$timeExpand, 
		simplify = FALSE
	)	
	timeExpand <- NULL
	if(!is.null(timeLim)){
		
		# consider only the modules to be aligned
		if(is.list(timeLim))
			timeExpandMod <- timeExpandMod[names(timeLim)]
		# and time variant
		modTimeVariant <- names(which(sapply(listPlots, isSubjectProfileTimeVariant, empty = FALSE)))
		timeExpandMod <- timeExpandMod[modTimeVariant]
		
		# time expand specified
		timeExpandModSpec <- timeExpandMod[!sapply(timeExpandMod, is.null)]
		
		if(length(timeExpandModSpec) > 0 & length(timeExpandMod) > 0){
		
			checkFctId <- function(list)	
				if(length(list) > 1){
					all(mapply(identical, head(list, -1), tail(list, -1)))
				}else	TRUE
			
			isTimeExpandSet <- all(names(timeExpandMod) %in% names(timeExpandModSpec)) && checkFctId(timeExpandModSpec)
			
			if(!isTimeExpandSet){
			
				# take max of all expand
				timeExpandMax <- do.call(pmax, timeExpandModSpec)
				
				timeExpand <- setNames(
					replicate(n = length(timeExpandMod), timeExpandMax, simplify = FALSE),
					names(timeExpandMod)
				)
				message(paste0(toString(names(timeExpandMod)), " modules",
					" are expanded (expand is: ", toString(timeExpandMax), ")",
					" to be time aligned."
				))
		
			}
		
		}
		
	}
	
	return(timeExpand)
	
}



#' Check if the subject profiles are time transformed,
#' and if some of the plots to align (with specified \code{timeLim})
#' have compatible time transformation alignments.
#' @param listPlots list of plots
#' @param timeLim time limits
#' @return List of time transformation for each module
#' (named by \code{listPlots})
#' @importFrom utils head tail
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
		
		# consider only the modules to be aligned...
		if(is.list(timeLim))
			timeTransMod <- timeTransMod[names(timeLim)]
		
		# ... and time variant
		modTimeVariant <- names(which(sapply(listPlots, isSubjectProfileTimeVariant, empty = FALSE)))
		timeTransMod <- timeTransMod[modTimeVariant]
		
		timeTransModSpec <- timeTransMod[!sapply(timeTransMod, is.null)]
		
		# set transformation for all modules in case timeExpand specified for one of them
		# or plots should be time-aligned
		if(length(timeTransModSpec) > 0){
			
			# if only one transformation is specified
			checkFctId <- function(list)	
				if(length(list) > 1){
					all(mapply(identical, head(list, -1), tail(list, -1)))
				}else	TRUE
			if(checkFctId(timeTransModSpec)){
				
				timeTransUsed <- timeTransModSpec[[1]]	
				
				# set transformation for all modules in case transformation specified for one of them
				timeTrans <- timeTransUsed
				if(!is.null(timeTrans))
					message(paste("All modules are transformed with", timeTrans$name, 
						"to be time aligned."
					))
				
			# if different time transformations are used,
			# the modules cannot be aligned
			}else{
				
				timeTransModName <- sapply(timeTransModSpec, function(x) ifelse(is.null(x), "none", x$name))
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