#' Create subject profile report
#' @param listPlots nested list of plots, as returned by the \code{\link{subjectProfileTextPlot}},
#' \code{\link{subjectProfileEventPlot}}, \code{\link{subjectProfileIntervalPlot}} or
#' \code{\link{subjectProfileLinePlot}} functions.
#' @param outputFile string, path to the output report
#' @param exportFigures logical, if TRUE (FALSE by default) the figures are also exported
#' in png format in a 'figures' folder
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
#' @param timeLim vector of length 2 with time limits.
#' If not specified, these are set to the time limits specified
#' when creating each module (stored in \code{attributes(x)$metaData$timeLim})
#' otherwise to the maximum range contained in the data.
#' Note that this doesn't modify the geoms of the plots, it only extends the
#' axis range. So for interval module(s) if the specified \code{timeLim}
#' is smaller than the time limits in the input plot, no arrows are created in case than
#' the time goes above/below specified \code{timeLim} (the segment is cut).
#' @inheritParams subjectProfileCombine
#' @inheritParams defineIndex
#' @inheritParams subjectProfileIntervalPlot
#' @return no returned value, the report is created at the location
#' specified by \code{outputFile}
#' @author Laure Cougnaud
#' @importFrom tools texi2pdf file_path_sans_ext
#' @importFrom plyr ddply
#' @export
createSubjectProfileReport <- function(
	listPlots, 
	timeLim = NULL,
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL,
	bookmarkData = NULL,
	bookmarkVar = NULL,
	subjectSortData = bookmarkData,
	subjectSortVar = bookmarkVar,
	subjectVar = "USUBJID",
	subjectSubsetData = NULL,
	subjectSubsetVar = NULL,
	subjectSubsetValue = NULL,
	outputFile = "subjectProfile.pdf",
	exportFigures = FALSE,
	labelVars = NULL,
	maxNLines = NULL,
	shiny = FALSE,
	formatReport = subjectProfileReportFormat(),
	verbose = FALSE){

	if(shiny && !requireNamespace("shiny", quietly = TRUE))
		stop("The package 'shiny' is required to report progress.")

	# margin of document in inches
	if(is.null(maxNLines)){
		inputGetMNL <- formatReport[names(formatReport) != "yLabelWidth"]
		maxNLines <- do.call(getMaxNLinesCombinePlot, inputGetMNL)
	}

	# filter subjects if subset[Data/Var/Value] is specified
	if(!is.null(subjectSubsetData)){
		
		# extract subjects for specified subset
		dataSubjectSubset <- filterData(
			data = subjectSubsetData, 
			subsetVar = subjectSubsetVar, 
			subsetValue = subjectSubsetValue
		)
		subjectsSubset <- unique(as.character(dataSubjectSubset[, subjectVar]))
		
		# filter 'listPlots' to only retain selected subjects
		listPlots <- sapply(listPlots, function(x){
			metaDataX <- attributes(x)$metaData
			newX <- x[which(names(x) %in% subjectsSubset)]
			attributes(newX)$metaData <- metaDataX
			newX
		}, simplify = FALSE)		
		listPlots <- listPlots[sapply(listPlots, length) > 0]
		
	}
	
	if(is.null(timeLim))
		timeLim <- getXLimSubjectProfilePlots(listPlots)
	
	# combine plots
	listPlotsPerSubjectList <- subjectProfileCombine(
		listPlots, 
		timeLim = timeLim, 
		refLines = refLines, refLinesData = refLinesData, 
		refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
		subjectVar = subjectVar,
		maxNLines = maxNLines,
		shiny = shiny,
		verbose = verbose
	)
	
	if(!is.null(subjectSortData) & !is.null(subjectSortVar)){
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
	index <- if(!is.null(bookmarkData) & !is.null(bookmarkVar))
		defineIndex(
			subjects = names(listPlotsPerSubjectList), 
			data = bookmarkData,
			var = bookmarkVar,
			subjectVar = subjectVar,
			labelVars = labelVars
		)
	
	msgProgress <- "Create subject profile report."
	if(verbose)	message(msgProgress)
	if(shiny)	incProgress(0.1, detail = msgProgress)
	
	pathTemplate <- getPathTemplate("subjectProfile.Rnw")
	
	outputDir <- normalizePath(dirname(outputFile), winslash = "/")
	
	## input parameters for the child document:
	# save them in a new environment, passed to the 'knitr::knit' function
	inputParametersEnv <- new.env()
	inputParameters <- list(
		listPlotsPerSubjectList = listPlotsPerSubjectList,
		outputDir = outputDir,
		index = index,
		formatReport = formatReport,
		shiny = shiny
	)
	assign("inputParameters", inputParameters, envir = inputParametersEnv)
	
	## convert Rnw -> tex
	outputFileKnitr <- knitr::knit(
		input = pathTemplate, quiet = FALSE,
		envir = inputParametersEnv
	)
	
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


#' Get limits for a list of plots.
#' 
#' These limits are extracted from specified \code{timeLim} for each
#' module (stored in the \code{attributes()$metaData$timeLim}), 
#' and if empty for all modules: from the maximal range
#' of the x-coordinates across all plots.
#' @param listPlots list of list of \code{subjectProfile[X]Plot} plots
#' @return vector of length 2 with limits for the x-axis
#' @importFrom ggplot2 ggplot_build
#' @author Laure Cougnaud
getXLimSubjectProfilePlots <- function(listPlots){
	
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
	
	return(timeLim)
	
}

