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
#' @param timeLim vector of length 2 with time limits.
#' If not specified, these are set to the time limits specified
#' when creating each module (stored in \code{attributes(x)$metaData$timeLim})
#' otherwise to the maximum range contained in the data.
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
	timeAlign = TRUE,
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
	
	if(is.null(timeLim)){
		if(verbose)	message("Get limits x-axis.")
		timeLim <- getXLimSubjectProfilePlots(listPlots, align = timeAlign)
	}
	
	# combine plots
	listPlotsPerSubjectList <- subjectProfileCombine(
		listPlots, 
		timeLim = timeLim, timeAlign = timeAlign,
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

	outputDir <- normalizePath(dirname(outputFile), winslash = "/")
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
		quiet = !verbose,
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
#' @param align Logical, if TRUE (by default) the plots are aligned,
#' the limits of all plots are combined to extract the limits.
#' @return vector of length 2 with limits for the x-axis
#' @importFrom ggplot2 ggplot_build
#' @author Laure Cougnaud
getXLimSubjectProfilePlots <- function(listPlots, align = TRUE){
	
	if(align){
	
		# in case the time limits were specified for a specific plot
		timeLimPlots <- unlist(lapply(listPlots, function(x) attributes(x)$metaData$timeLim))
		if(!is.null(timeLimPlots)){
			
			timeLim <- range(timeLimPlots, na.rm = TRUE)
			
		}else{
			
			listPlots1 <- unlist(unlist(listPlots, recursive = FALSE), recursive = FALSE)
			
			xlimList <- lapply(listPlots1, function(gg)
				if(!inherits(gg, "subjectProfileTextPlot"))
					range(
						unlist(
						 lapply(ggplot_build(gg)$data, function(dataPlot) 
							c(dataPlot$x, if("xend" %in% colnames(dataPlot))	dataPlot$xend)
						)
					)
				)
			)
			
			xlimVect <- unlist(xlimList)
			
			timeLim <- if(!is.null(xlimVect)){
				range(xlimVect, na.rm = TRUE)
			}else{c(-Inf, Inf)}
	
		}
		
	}else	timeLim <- NULL
		
	return(timeLim)
	
}

