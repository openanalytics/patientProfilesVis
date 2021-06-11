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
#' @param bookmarkData,bookmarkVar Data.frame with data containing information for the index,
#' and character vector with corresponding variable(s) of interest.
#' An index will be created at the end of the subject profile report.\cr
#' The index contains a section per variable, referencing the 
#' pages of the report containing subject profiles
#' for each category/variable.
#' @param subjectSortData Data.frame with data containing information on how the subjects 
#' should be sorted (by default same as \code{bookmarkData}):
#' \itemize{
#' \item{in the report, in case one single report is created for all subjects}
#' \item{for the export, in case \code{reportPerSubject} is TRUE}
#' }
#' This data should contain \code{subjectSortVar} and \code{subjectVar}.
#' @param subjectSortVar Character vector, 
#' variable(s) of \code{subjectSortData} indicating the order for the subjects in the report,
#' (by default same as \code{bookmarkVar}).
#' @param subjectSortDecreasing Logical, if TRUE (FALSE by default)
#' subjects are sorted based on decreasing order of \code{subjectSortVar}.
#' @param subjectSubsetData Data.frame used to select subset of subjects of interest.
#' @param subjectSubsetVar String with variable of \code{subjectSubsetData} 
#' that should be considered to filter subjects.
#' If not specified, all subjects available in \code{subjectSubsetData} are considered.
#' @param subjectSubsetValue Character vector with value(s) of \code{subjectSubsetVar}
#' of interest to filter subjects on.
#' @param subjectSample (optional) Integer of length 1
#' with number of random subject(s) that should be considered in the specified subset dataset.
#' By default, all specified subjects are considered (set to NULL).
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
#' @param exportBatchSize (optional) Integer, if specified, the
#' patient-profile reports are created by batch of this number of subjects.
#' This might speed up the export for a high number of subjects.
#' Only available if report is created by subject (\code{reportPerSubject} is TRUE)
#' and modules are not aligned across subjects (\code{timeAlignPerSubject} is: 'all').
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams subjectProfileCombine
#' @inheritParams subjectProfileExport
#' @inheritParams filterData
#' @return The path(s) of the report(s) is returned invisibly, and the
#' report is created at the location
#' specified by \code{outputFile}.\cr
#' If the report is created by subject, 
#' the name of the exported subject profile is built as:
#' \code{[filename]-[subjectID].pdf}, with [filename] extracted from
#' \code{outputFile}.
#' Space and platform-specific file separator are replaced by
#' a dash in the filename.\cr
#' If no patient profiles are available in the input,
#' nothing is returned and a warning is triggered.
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
	subjectSortDecreasing = FALSE,
	subjectVar = "USUBJID",
	subjectSubset = NULL,
	subjectSubsetData = NULL,
	subjectSubsetVar = NULL,
	subjectSubsetValue = NULL,
	subjectSample = NULL, 
	seed = 123,
	subset = NULL,
	outputFile = "subjectProfile.pdf",
	exportFigures = FALSE,
	reportPerSubject = FALSE,
	exportBatchSize = NULL,
	labelVars = NULL,
	maxNLines = NULL,
	shiny = FALSE,
	formatReport = subjectProfileReportFormat(),
	verbose = FALSE,
	nCores = 1){

	# store input parameters of the function
	inputArgs <- c(as.list(environment())) #, list(...)
	
	if(sum(sapply(listPlots, length)) == 0){
		warning("No patient profiles available in the input.")
		return(invisible())
	}

	# filter subjects if subset[Data/Var/Value] is specified
	if(!is.null(subset) | !is.null(subjectSubsetData) | !is.null(subjectSample)){
		
		if(verbose)	message("Filter subjects of interests.")
		
		if(!is.null(subjectSubsetData)){
		
			# extract subjects for specified subset
			dataSubjectSubset <- filterData(
				data = subjectSubsetData, 
				subsetVar = subjectSubsetVar, 
				subsetValue = subjectSubsetValue,
				subjectSubset = subjectSubset,
				subjectVar = subjectVar,
				subjectSample = subjectSample, 
				seed = seed
			)
			subset <- union(subset,
				unique(as.character(dataSubjectSubset[, subjectVar]))
			)
			
		}else	if(!is.null(subjectSample)){
			warning("Dataset should be provided in case a sample of subjects is considered. 'subjectSample' is ignored.")
		}
		
		if(!is.null(subset)){
			
			# filter 'listPlots' to only retain selected subjects
			listPlots <- sapply(listPlots, function(x){
				metaDataX <- attributes(x)$metaData
				newX <- x[which(names(x) %in% subset)]
				attributes(newX)$metaData <- metaDataX
				newX
			}, simplify = FALSE)		
			listPlots <- listPlots[sapply(listPlots, length) > 0]
			
		}
		
	}
	
	## export per batch
	if(!is.null(exportBatchSize)){
		if(!reportPerSubject){
			warning(paste(
				"Creation of patient profiles per batch not possible",
				"for one single report across subjects.",
				"You might want to set 'reportPerSubject' to TRUE."
			))
		}else	if(!(length(timeAlignPerSubject) == 1 && timeAlignPerSubject == "all")){
			warning(paste(
				"Creation of patient profiles per batch not possible",
				"if plots should be aligned across subjects.",
				"You might want to set 'timeAlignPerSubject' to 'all'."
			))
		}else{
			
			# all subjects
			subjects <- Reduce(union, lapply(listPlots, names)) 
			
			# sort them (if specified)
			subjects <- sortSubjects(
				subjects = subjects, 
				subjectSortData = subjectSortData,
				subjectSortVar = subjectSortVar,
				subjectSortDecreasing = subjectSortDecreasing,
				subjectVar = subjectVar,
				verbose = verbose
			)
			
			# create report per batch:
			subjectsBatchId <- ceiling(seq(from = 1, to = length(subjects))/exportBatchSize)
			subjectsPerBatch <- split(subjects, subjectsBatchId)
			
			res <- lapply(seq_along(subjectsPerBatch), function(batch){
				if(verbose)	
					message(paste0("Creation of patient profiles for batch ", batch, "."))
				inputArgsBatch <- inputArgs
				inputArgsBatch$subset <- subjectsPerBatch[[batch]]
				inputArgsBatch$exportBatchSize <- NULL
				do.call(createSubjectProfileReport, inputArgsBatch)
			})
			res <- Reduce(c, res)
			return(invisible(res))
			
		}
	}
	
	if(shiny && !requireNamespace("shiny", quietly = TRUE))
		stop("The package 'shiny' is required to report progress.")	
	
	# plots should be named in case timeAlign/timeAlignPerSubject is specified
	if(is.null(names(listPlots)))
		names(listPlots) <- paste0("module", seq_along(listPlots))
	
	
	# margin of document in inches
	if(is.null(maxNLines)){
		if(verbose)	message("Get maximum number of lines for each page.")
		inputGetMNL <- formatReport[names(formatReport) != "yLabelWidth"]
		maxNLines <- do.call(getMaxNLinesCombinePlot, inputGetMNL)
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
		nCores = nCores,
		reportPerSubject = reportPerSubject
	)
	
	# sort plots based on specified dataset/var, ...
	subjectsOrdered <- sortSubjects(
		subjects = names(listPlotsPerSubjectList), 
		subjectSortData = subjectSortData,
		subjectSortVar = subjectSortVar,
		subjectSortDecreasing = subjectSortDecreasing,
		subjectVar = subjectVar,
		verbose = verbose
	)
	listPlotsPerSubjectList <- listPlotsPerSubjectList[subjectsOrdered]
	
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
		if(shiny)	shiny::incProgress(0.3, detail = msgProgress)
		
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
			if(shiny)	shiny::incProgress(0.3, detail = msgProgress)
			
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

#' Sort subjects based on a specified dataset/variable.
#' @param subjects Character vector with subjects of interest
#' @param subjectSortData Data.frame with data containing information on how the subjects 
#' should be sorted.
#' @param subjectSortVar Variable(s) of \code{subjectSortData} 
#' used to order the subjects
#' @param subjectSortDecreasing Logical, if TRUE (FALSE by default)
#' subjects are sorted based on inverse order of \code{subjectSortVar}.
#' @param verbose logical, if TRUE print messages during execution
#' @inheritParams patientProfilesVis-common-args
#' @return Updated \code{subjects}
#' @author Laure Cougnaud
sortSubjects <- function(
	subjects, 
	subjectVar = "USUBJID",
	subjectSortData = NULL,
	subjectSortVar = NULL,
	subjectSortDecreasing = FALSE,
	verbose = FALSE){

	if(!is.null(subjectSortData) & !is.null(subjectSortVar)){
		
		varsNotInData <- setdiff(c(subjectSortVar, subjectVar), colnames(subjectSortData))
		if(length(varsNotInData) == 0){
			
			if(verbose)	message(paste0("Order subjects based on: ", toString(sQuote(subjectSortVar)), "."))
			
			# sort subjects based on: 'subjectSortData'/'subjectSortVar'
			idxOrderSubjects <- order(subjectSortData[, subjectSortVar], decreasing = subjectSortDecreasing)
			subjectsSorted <- unique(subjectSortData[idxOrderSubjects, subjectVar])
			
			# only specified subjects
			subjectsSpecifiedAndSorted <- intersect(subjectsSorted, subjects)
			# subjects not in 'sortData'
			subjectsSpecifiedAndNotInSortData <- setdiff(subjects, subjectsSpecifiedAndSorted) 
			subjects <- c(subjectsSpecifiedAndSorted, subjectsSpecifiedAndNotInSortData)
			
		}else{
			warning("The subjects are not sorted as specified, ",
				"because the variable(s): ", toString(sQuote(varsNotInData)), 
				" are not available in the data.")
		}
	}
	
	return(subjects)

}

#' Create report
#' @param listPlotsSubject List of plots for each subject 
#' @param outputFile string, path to the output report
#' @param index Index, output from \code{\link{defineIndex}}
#' @param exportFigures Logical, if TRUE (FALSE by default) the 
#' subject profile figures are also exported
#' in pdf format in a 'figures' folder.\cr
#' Figures are named as \code{[subjectID]-[page].pdf}
#' @return No returned value, the plots are exported to \code{outputDir}
#' @inheritParams defineIndex
#' @inheritParams subjectProfileCombine 
#' @inheritParams patientProfilesVis-common-args
#' @importFrom tools texi2pdf file_path_sans_ext
#' @importFrom knitr knit knit_patterns knit_hooks pat_rnw render_sweave
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
	# in order that the function be called within Rmd doc:
	# Sweave-specific patterns and render function should be set
	# before calling: 'knitr::knit'
	knitPatInit <- knit_patterns$get();knitHookInit <- knit_hooks$get()
	# set Sweave pre-defined pattern list
	pat_rnw()
	# set output hooks for Sweave
	# suppress warning("unable to find LaTeX package 'Sweave'; will use a copy from knitr")
	suppressWarnings(render_sweave())
	outputFileKnitr <- knitr::knit(
		input = pathTemplateWd, 
		output = pathTexFile,
		envir = inputParametersEnv,
		quiet = TRUE
	)
	# use 'restore' and not: 'set' to avoid issue 
	# when rmd is created afterwards
	knit_patterns$restore(knitPatInit);knit_hooks$restore(knitHookInit)
	
	## convert tex -> pdf
	
	# texi2pdf cannot deal with space in name and file should be in current directory
	oldwd <- getwd()
	on.exit(setwd(oldwd))
	
	setwd(outputDir)
	
	# convert to pdf
	texFile <- basename(pathTexFile)
	texi2pdf(file = texFile, clean = TRUE)
	
	# rename output file
	outputTexi2pdf <- paste0(file_path_sans_ext(texFile), ".pdf")
	file.rename(from = outputTexi2pdf, to = basename(outputFile))
	
	# clean output directory
	filesToRemove <- c(file.path(outputDir, "Sweave.sty"), basename(pathTemplateWd), texFile)
	unlink(filesToRemove)
	
	if(!exportFigures)	unlink("figures", recursive = TRUE)
	
	unlink("Sweave.sty")
	
}


