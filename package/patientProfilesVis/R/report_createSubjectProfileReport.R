#' Create subject profile report.
#' 
#' By default all subjects available in at least one module of \code{listPlots} are considered.
#' If only a set of subjects are of interest, these are specified either:
#' \itemize{
#' \item{directly with the subject IDs of interest via \code{subjectSubset}}
#' \item{by extracting subjects with a specific value (\code{subjectSubsetValue})
#' in a variable (\code{subjectSubsetVar}) in a specific dataset \code{subjectSubsetData}}
#' }
#' @param listPlots nested list of plots, as returned by the \code{\link{subjectProfileTextPlot}},
#' \code{\link{subjectProfileEventPlot}}, \code{\link{subjectProfileIntervalPlot}} or
#' \code{\link{subjectProfileLinePlot}} functions.
#' @param bookmarkData data.frame with data containing information on which the index should be based
#' @param bookmarkVar variable(s) of \code{data} of interest for the index
#' @param subjectSortData data.frame with data containing information on how the subjects 
#' should be sorted in the report, by default same as \code{bookmarkData}
#' @param subjectSortVar variable(s) of \code{data} indicating the order for the subjects in the report,
#' by default same as \code{bookmarkVar}
#' @param subjectSubsetData data.frame with data used to select subset of subjects of interest.
#' It should contain the \code{subjectVar} variable.
#' @param subjectSubsetVar string with variable of \code{subjectSubsetData} used for subsetting.
#' @param subjectSubsetValue Character vector with value(s) of \code{subjectSubsetVar}
#' of interest to select subjects on.
#' @param subjectSubset subjectSubset (optional) Character vector with subjects of interest 
#' (available in \code{subjectVar}), NULL by default.
#' @param subset Character vector with subjects of interest 
#' (among names of each list in \code{listPlots}).
#' @param timeLim Time limits, as a numeric vector of length 2,
#' or a list with time limits for each module,
#' or nested list with time limits for each module and subject.
#' If not specified, these are set to the time limits specified
#' when creating each module (stored in \code{attributes(x)$metaData$timeLim})
#' otherwise to the range defined by \code{timeAlign} and \code{timeAlignPerSubject}.
#' Note that this doesn't modify the geoms of the plots, it only extends the
#' axis range. So for interval module(s) if the specified \code{timeLim}
#' is smaller than the time limits in the input plot, no arrows are created in case than
#' the time goes above/below specified \code{timeLim} (the segment is cut).
#' @param reportPerSubject Logical, if TRUE (FALSE by default)
#' export a subject profile report by subject.
#' @inheritParams subjectProfileCombine
#' @inheritParams subjectProfileIntervalPlot
#' @inheritParams subjectProfileExport
#' @return The path(s) of the report(s) is returned invisibly, and the
#' the report is created at the location
#' specified by \code{outputFile}.
#' @author Laure Cougnaud
#' @importFrom plyr ddply
#' @importFrom tools file_ext file_path_sans_ext
#' @export
createSubjectProfileReport <- function(
	listPlots, 
	timeLim = NULL,
	timeAlign = "all", timeAlignPerSubject = "none",
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL,
	bookmarkData = NULL,
	bookmarkVar = NULL,
	subjectSortData = bookmarkData,
	subjectSortVar = bookmarkVar,
	subjectVar = "USUBJID",
	subjectSubset = NULL,
	subjectSubsetData = NULL,
	subjectSubsetVar = NULL,
	subjectSubsetValue = NULL,
	subset = NULL,
	outputFile = "subjectProfile.pdf",
	exportFigures = FALSE,
	reportPerSubject = FALSE,
	labelVars = NULL,
	maxNLines = NULL,
	shiny = FALSE,
	formatReport = subjectProfileReportFormat(),
	verbose = FALSE,
	nCores = 1){

	if(shiny && !requireNamespace("shiny", quietly = TRUE))
		stop("The package 'shiny' is required to report progress.")

	# margin of document in inches
	if(is.null(maxNLines)){
		if(verbose)	message("Get maximum number of lines for each page.")
		inputGetMNL <- formatReport[names(formatReport) != "yLabelWidth"]
		maxNLines <- do.call(getMaxNLinesCombinePlot, inputGetMNL)
	}
	
	
	# plots should be named in case timeAlign/timeAlignPerSubject is specified
	if(is.null(names(listPlots)))
		names(listPlots) <- paste0("module", seq_along(listPlots))

	# filter subjects if subset[Data/Var/Value] is specified
	if(!is.null(subset) | !is.null(subjectSubsetData)){
		
		if(verbose)	message("Filter subjects of interests.")
		
		if(!is.null(subjectSubsetData)){
		
			# extract subjects for specified subset
			dataSubjectSubset <- filterData(
				data = subjectSubsetData, 
				subsetVar = subjectSubsetVar, 
				subsetValue = subjectSubsetValue,
				subjectSubset = subjectSubset,
				subjectVar = subjectVar
			)
			subset <- union(subset,
				unique(as.character(dataSubjectSubset[, subjectVar]))
			)
			
		}
		
		# filter 'listPlots' to only retain selected subjects
		listPlots <- sapply(listPlots, function(x){
			metaDataX <- attributes(x)$metaData
			newX <- x[which(names(x) %in% subset)]
			attributes(newX)$metaData <- metaDataX
			newX
		}, simplify = FALSE)		
		listPlots <- listPlots[sapply(listPlots, length) > 0]
		
	}
	
	# combine plots
	listPlotsPerSubjectList <- subjectProfileCombine(
		listPlots, 
		timeLim = timeLim, 
		timeAlign = timeAlign, timeAlignPerSubject = timeAlignPerSubject,
		refLines = refLines, refLinesData = refLinesData, 
		refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
		subjectVar = subjectVar,
		maxNLines = maxNLines,
		shiny = shiny,
		verbose = verbose,
		nCores = nCores
	)
	
	if(!is.null(subjectSortData) & !is.null(subjectSortVar)){
		if(verbose)	message("Order subjects based on subjectSortData/subjectSortVar.")
		subjectsOrdered <- ddply(unique(subjectSortData[, c(subjectVar, subjectSortVar)]), subjectSortVar)[[subjectVar]]
		if(all(names(listPlotsPerSubjectList) %in% subjectsOrdered)){
			# in case more subjects are available in sortData than in the plot(s)
			subjectsOrderedInData <- subjectsOrdered[subjectsOrdered %in% names(listPlotsPerSubjectList)]
			listPlotsPerSubjectList <- listPlotsPerSubjectList[subjectsOrderedInData]
		}else{
			warning("The subjects are not ordered according to the specified 'subjectSortVar',",
				"because not all subjects are contained in the 'subjectSortData'.")
		}
	}
	
	# extract bookmark(s) (if any)
	index <- if(!is.null(bookmarkData) & !is.null(bookmarkVar)){
		if(verbose)	message("Extract bookmarks.")
		defineIndex(
			subjects = names(listPlotsPerSubjectList), 
			data = bookmarkData,
			var = bookmarkVar,
			subjectVar = subjectVar,
			labelVars = labelVars
		)
	}
	
	names(listPlotsPerSubjectList) <- sub("/", "-", names(listPlotsPerSubjectList))
		
	if(!reportPerSubject){
		
		msgProgress <- "Create subject profile report."
		if(verbose)	message(msgProgress)
		if(shiny)	incProgress(0.3, detail = msgProgress)
		
		subjectProfileExport(
			listPlotsSubject = listPlotsPerSubjectList, 
			outputFile = outputFile, 
			index = index, 
			formatReport = formatReport, 
			shiny = shiny, verbose = verbose, nCores = nCores,
			exportFigures = exportFigures
		)
		
		res <- outputFile
		
	}else{
		
		res <- outputFiles <- sapply(names(listPlotsPerSubjectList), function(subject){
			paste0(
				file_path_sans_ext(outputFile), 
				"-", subject, ".", file_ext(outputFile)
			)		
		})
		
		for(subject in names(listPlotsPerSubjectList)){
		
			msgProgress <- paste("Create subject profile report for subject:", subject)
			if(verbose)	message(msgProgress)
			if(shiny)	incProgress(0.3, detail = msgProgress)
			
			subjectProfileExport(
				listPlotsSubject = listPlotsPerSubjectList[subject], 
				outputFile = outputFiles[subject], 
				index = NULL, 
				formatReport = formatReport, 
				shiny = shiny, verbose = verbose, nCores = nCores,
				exportFigures = exportFigures
			)
		
		}
		
	}
	
	invisible(res)
	
}

#' Create report
#' @param listPlotsSubject List of plots for each subject 
#' @param outputFile string, path to the output report
#' @param index Index, output from \code{\link{defineIndex}}
#' @param exportFigures logical, if TRUE (FALSE by default) the figures are also exported
#' in png format in a 'figures' folder
#' @return No returned value, the plots are exported to \code{outputDir}
#' @inheritParams defineIndex
#' @inheritParams subjectProfileCombine 
#' @inheritParams subjectProfileIntervalPlot
#' @importFrom tools texi2pdf file_path_sans_ext
#' @importFrom knitr knit
#' @author Laure Cougnaud
subjectProfileExport <- function(
	listPlotsSubject, 
	outputFile = "subjectProfile.pdf",
	index = NULL, 
	formatReport = subjectProfileReportFormat(), 
	shiny = FALSE, verbose = FALSE, nCores = NULL,
	exportFigures = FALSE){

	outputDir <- normalizePath(dirname(outputFile), winslash = "/", mustWork = FALSE)
	if(!dir.exists(outputDir))	dir.create(outputDir, recursive = TRUE)

	pathTemplate <- getPathTemplate("subjectProfile.Rnw")
	pathTemplateWd <- file.path(outputDir, basename(pathTemplate))
	tmp <- file.copy(from = pathTemplate, to = pathTemplateWd, overwrite = TRUE)
	pathTexFile <- paste0(file_path_sans_ext(pathTemplateWd), ".tex")
	
	## input parameters for the child document:
	# save them in a new environment, passed to the 'knitr::knit' function
	inputParametersEnv <- new.env()
	inputParameters <- list(
		listPlotsPerSubjectList = listPlotsSubject,
		index = index,
		formatReport = formatReport,
		shiny = shiny,
		verbose = verbose,
		nCores = nCores
	)
	assign("inputParameters", inputParameters, envir = inputParametersEnv)
	
	## convert Rnw -> tex
	outputFileKnitr <- knitr::knit(
		input = pathTemplateWd, 
		output = pathTexFile,
		quiet = TRUE,
		envir = inputParametersEnv
	)
	
	## convert tex -> pdf
	
	# texi2pdf cannot deal with space in name and file should be in current directory
	oldwd <- getwd()
	setwd(outputDir)	
	
	# convert to pdf
	texFile <- basename(pathTexFile)
	texi2pdf(file = texFile, clean = TRUE)
	
	# rename output file
	outputTexi2pdf <- paste0(file_path_sans_ext(texFile), ".pdf")
	file.rename(from = outputTexi2pdf, to = basename(outputFile))
	
	# clean output directory
	tmp <- file.remove(c(basename(pathTemplateWd), texFile)) # remove tex file
	
	if(!exportFigures)	unlink("figures/", recursive = TRUE)
	
	setwd(oldwd)
	
}


#' Get limits for a list of plots.
#' 
#' These limits are extracted from specified \code{timeLim} for each
#' module (stored in the \code{attributes()$metaData$timeLim}), 
#' and if empty for all modules: from the maximal range
#' of the x-coordinates across all plots.
#' @param listPlots list of list of \code{subjectProfile[X]Plot} plots
#' @param timeAlign Character vector with time alignment, either:
#' \itemize{
#' \item{'all' (by default): }{all plots have the same time limits}
#' \item{'none': }{each of the plot has its own time limits}
#' \item{character vector with names of the modules which
#' should have the same time limits
#' (should correspond to the names of \code{listPlots})}
#' }
#' @param timeAlignPerSubject Character vector specifying
#' which modules of \code{timeAlign} should have subject-specific time limits
#' \itemize{
#' \item{'none' (by default): }{these modules have the same time limit across subjects}
#' \item{'all': }{these modules have different time limits per subject}
#' \item{character vector with subset of these modules which should have
#'  different time limits per subject
#' (should correspond to the names of \code{listPlots})}
#' }
#' This is only used for the modules with which \code{timeAlign} is specified.
#' @return Time limits, as a numeric vector of length 2,
#' or a list with time limits for each module,
#' or nested list with time limits for each module and subject.
#' @importFrom ggplot2 ggplot_build
#' @author Laure Cougnaud
getXLimSubjectProfilePlots <- function(
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
		
		modTimeVariant <- names(which(sapply(listPlots, isSubjectProfileTimeVariant, empty = FALSE)))
		
		if(is.null(names(listPlots)))
			stop("'listPlots' should be named if time alignment is required.")
	
		if(length(timeAlign) == 1 && timeAlign == "all"){
			
			alignMod <- modTimeVariant
			
		}else{
						
			alignModulesNA <- setdiff(timeAlign, names(listPlots))
			if(length(alignModulesNA > 0)){
				warning(paste("Modules to align:", toString(alignModulesNA), "are not available",
					"in the names of the list of plots."))
			}
			alignModuleNotTV <- setdiff(timeAlign, modTimeVariant)
			if(length(alignModuleNotTV) > 0){
				warning(paste("Modules to align:", toString(alignModuleNotTV), "are not",
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
					'none' = NULL,
					timeAlignPerSubject
				)
			}else	timeAlignPerSubject
				
			alignPerSubjectModNA <- setdiff(alignPerSubjectMod, alignMod)
			if(length(alignPerSubjectModNA) > 0){
				warning(paste("Modules:", toString(alignPerSubjectModNA),
					"are specified to be aligned per subject, but are not specified/available among",
					"the modules to align, so these are ignored."))
			}
			alignPerSubjectMod <- intersect(alignPerSubjectMod, alignMod)
			
			# create empty element in case subject not present in one of the module
			# for the 'mapply' below...
			subjectsID <- unique(unlist(lapply(listPlots, names)))
			listPlotsAll <- sapply(listPlots, function(x){
				list <- setNames(x[subjectsID], subjectsID) # in case plot not available for one subject
				list <- sapply(subjectsID, function(subj){
					structure(list[[subj]], metaData = list(subject = subj))
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
								lapply(ggplot_build(gg)$data, function(dataPlot) 
									c(dataPlot$x, if("xend" %in% colnames(dataPlot))	dataPlot$xend)
								)
						)
						timeLimData <- unlist(timeLimDataList)
						
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
						
					}, simplify = FALSE)
					
				}else{
					
					if(timeAlignPerSubject)
						warning(paste("Alignment per subject is not available for module", mod,
							"because time limits were specified during module creation."))

					setNames(
						replicate(length(listPlotsMod), timeLimPlots, simplify = FALSE),
						names(listPlotsMod)
					)
					
				}
			
			}, simplify = FALSE)
			
			getRange <- function(...){
				if(all(sapply(..., is.null))){
					NULL
				}else	range(..., na.rm = TRUE)
			}
			
			# extract limits across modules for each subject
			timeLimPerSubj <- do.call(mapply, 
				c(timeLimPlotsSubj[alignMod],
				list(FUN = range, na.rm = TRUE, SIMPLIFY = FALSE))
			)
			timeLim <- setNames(
				replicate(length(alignMod), timeLimPerSubj, simplify = FALSE),
				alignMod
			)

			# if alignment across subjects
			# should extract limits across subjects for each module
			alignAcrossSubjectMod <- setdiff(alignMod, alignPerSubjectMod)
			if(length(alignAcrossSubjectMod) > 0)
				timeLim[alignAcrossSubjectMod] <- sapply(timeLim[alignAcrossSubjectMod], function(x){
					getRange(unlist(x)
				) 
			}, simplify = FALSE)
	
		}else	timeLim <- NULL
		
	}

	return(timeLim)
	
}

