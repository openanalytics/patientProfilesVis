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
#' @param timeLim vector of length 2 with time limits.
#' If not specified, these are set to the time limits specified
#' when creating each module (stored in \code{attributes(x)$metaData$timeLim})
#' otherwise to the maximum range contained in the data.
#' @inheritParams subjectProfileCombine
#' @inheritParams defineIndex
#' @inheritParams subjectProfileIntervalPlot
#' @return no returned value, the report is created at the location
#' specified by \code{outputFile}
#' @author Laure Cougnaud
#' @importFrom tools texi2pdf
#' @export
createSubjectProfileReport <- function(
	listPlots, 
	timeLim = getXLimSubjectProfilePlots(listPlots),
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL,
	bookmarkData = NULL,
	bookmarkVar = NULL,
	subjectSortData = bookmarkData,
	subjectSortVar = bookmarkVar,
	subjectVar = "USUBJID",
	outputFile = "subjectProfile.pdf",
	exportFigures = FALSE,
	labelVars = NULL,
	maxNLines = NULL,
	shiny = FALSE,
	formatReport = subjectProfileReportFormat()){

	if(shiny && !requireNamespace("shiny", quietly = TRUE))
		stop("The package 'shiny' is required to report progress.")

	# margin of document in inches
	if(is.null(maxNLines)){
		maxNLines <- do.call(getMaxNLinesCombinePlot, formatReport)
	}

	# combine plots
	listPlotsPerSubjectList <- subjectProfileCombine(
		listPlots, 
		timeLim = timeLim, 
		refLines = refLines, refLinesData = refLinesData, 
		refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
		subjectVar = subjectVar,
		maxNLines = maxNLines,
		shiny = shiny
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
	
	if(shiny)	incProgress(0.1, detail = "Create subject profile report.")
	
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

#' Combine subject profile plots.
#' @param listPlots list of \code{\link[ggplot2]{ggplot2}} objects
#' @param shiny logical, set to TRUE (FALSE by default) if the report is generated from a Shiny application.
#' Messages during report creation will be included in the Shiny interface,
#' and it will be mentioned at the end of the report.
#' In this case, the \code{shiny} package should be available.
#' @return a list of \code{subjectProfilePlot} object, containing the combined
#' profile plots for each subject.
#' @importFrom cowplot ggdraw draw_label
#' @inheritParams subjectProfileCombineOnce
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

	if(shiny)	incProgress(0.1, detail = "Combine plots across modules for each subject.")
	
	# combine all plots per subject
	# this returns a list of 'gtable' object
	plotLabels <- sapply(listPlotsAll, function(x){
		label <- attributes(x)$metaData$label
		ifelse(is.null(label), "", label)
	})
	listPlotsPerSubject <- do.call(mapply, 
		c(
			list(FUN = subjectProfileCombineOnce, SIMPLIFY = FALSE, 
				MoreArgs = list(
					labels = plotLabels, 
					timeLim = timeLim,
					maxNLines = maxNLines,
					refLines = refLines, refLinesData = refLinesData, 
					refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
					subjectVar = subjectVar
				)),
			listPlotsAll
		)
	)
		
	# add title
	if(shiny)	incProgress(0.5, detail = "Add title.")
	listPlotsPerSubject <- sapply(names(listPlotsPerSubject), function(subject){
		
		# create a ggplot for title only
		title <- ggdraw() + 
			draw_label(paste(" Subject:", subject), fontface = 'bold', 
				x = 0, hjust = 0, lineheight = 2
			)
		
		plotSubjectList <- listPlotsPerSubject[[subject]]
		
		plotsSubjectListRF <- sapply(plotSubjectList, function(plotSubject){
		
			# extract number of lines in main plot...
			nLinesPlot <- attr(plotSubject, "nLinesPlot")
			# to get relative height of title/main plot
			relHeights <- c(4, nLinesPlot)/(4+nLinesPlot)
	#		convertX(ggplot_build(gg)$plot$theme$plot.margin, unitTo = "lines", valueOnly = TRUE)
			
			# combine title and plot
			plotSubjectWithTitle <- plot_grid(title, plotSubject, ncol = 1, rel_heights = relHeights)
			
			nLinesPlotTotal <- nLinesPlot + 4
			
			# store the number of lines in the y-axis (used to adapt size during export)
			attributes(plotSubjectWithTitle) <- c(
				attributes(plotSubjectWithTitle), 
				list(
					nLinesPlot = nLinesPlotTotal,
					nSubplots = attr(plotSubject, "nSubplots")
				)
			)
			class(plotSubjectWithTitle) <- c("subjectProfilePlot", class(plotSubjectWithTitle))
			
			plotSubjectWithTitle
			
		}, simplify = FALSE)
		
	}, simplify = FALSE)

	return(listPlotsPerSubject)
	
}

#' combine multiple \code{\link[ggplot2]{ggplot2}} objects
#' @param ... \code{\link[ggplot2]{ggplot2}} objects
#' @param labels string with labels for the plots
#' @param maxNLines maximum number of lines for a combined plot,
#' to fit in the page height
#' @inheritParams subjectProfileIntervalPlot
#' @inheritParams addReferenceLinesProfilePlot
#' @return \code{subjectProfilePlot} object, containing the combined
#' profile plots
#' @author Laure Cougnaud
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 coord_cartesian ggplot theme_bw ggtitle
subjectProfileCombineOnce <- function(..., 
	labels, 
	timeLim = NULL, 
	maxNLines = NULL,
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

#			# add number of lines used for labels reference lines in plot
#			nLinesPlot[plotsToModify] <- nLinesPlot[plotsToModify] +
#				sapply(newPlots, function(gg){
#					nLinesRef <- attributes(gg)$metaData$nLinesLabelRefLines
#					ifelse(is.null(nLinesRef), 0, nLinesRef)
#			})
		
		}
		
		# wrapper function to combine plots with 'subplot'
		combineGGPlotsOnce <- function(listGgPlotsToCombine, nLinesPlot){
			# relative height of each plot
			relHeights <- nLinesPlot/sum(nLinesPlot)
			# combine all plots
			plot <- combineVerticallyGGplot(listPlots = listGgPlotsToCombine, heights = relHeights, package = "egg")
			# store the number of lines in the y-axis (used to adapt size during export)
			attributes(plot) <- c(attributes(plot), 
				list(nLinesPlot = sum(nLinesPlot), nSubplots = length(listGgPlotsToCombine))
			)
			class(plot) <- c("subjectProfilePlot", class(plot))
			return(plot)
		}
		
		# split/combine plots to fit in a page with maximum number of lines: maxNLines
		plot <- if(!is.null(maxNLines)){
			plotPages <- getSplitVectorByInt(sizes = nLinesPlot, max = maxNLines)
			plots <- sapply(unique(plotPages), function(page){
				idx <- which(plotPages == page)
				combineGGPlotsOnce(listGgPlotsToCombine[idx], nLinesPlot[idx])
			}, simplify = FALSE)	
		}else{
			combineGGPlotsOnce(listGgPlotsToCombine, nLinesPlot)
		}
		
	}
	
	return(plot)

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



#' Add reference lines to a profile plot
#' @param gg \code{\link[ggplot2]{ggplot2}} object
#' @param refLines (optional) nested list with details for reference line(s).
#' Each sublist contains:
#' \itemize{
#' \item{(required) 'label': }{string with label for the reference line}
#' \item{(required) 'time': }{time(x) coordinate for the reference line,
#' 'dotted' by default}
#' \item{(optional) 'color': }{color for the reference line,
#' 'black' by default}
#' \item{(optional) 'linetype': }{linetype for the reference line}
#' }
#' @param refLinesData data.frame with data from which the reference line(s) should be extracted
#' @param refLinesTimeVar string, variable of \code{refLinesData} with time for reference line(s)
#' @param refLinesLabelVar string, variable of \code{refLinesData} with label for reference line(s)
#' @param refLinesColor vector of length 1 with default color for reference line(s)
#' @param refLinesLinetype vector of length 1 with default linetype for reference line(s)
#' @param addLabel logical, if TRUE (FALSE by default) add the label of the reference line(s) at the bottom of the plot
#' @param timeLim vector of length 2 with time limits
#' @inheritParams subjectProfileIntervalPlot
#' @return if \code{addLabel} is TRUE, list with:
#' 'gg': \code{\link[ggplot2]{ggplot2}} and 'ggRefLines': \code{\link[ggplot2]{ggplot2}} with labels,
#' \code{\link[ggplot2]{ggplot2}} otherwise
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom stats setNames
#' @importFrom cowplot plot_grid
addReferenceLinesProfilePlot <- function(
	gg, 
	subjectVar = "USUBJID",
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL,
	refLinesColor = "black",
	refLinesLinetype = "dotted",
	timeLim = NULL,
	addLabel = FALSE){
	
	refLinesVect <- !is.null(refLines)
	refLinesFromData <- !is.null(refLinesData) & !is.null(refLinesTimeVar) & !is.null(refLinesLabelVar) 

	if(refLinesVect){
		
		refLinesLabels <- sapply(refLines, function(x) x$label)
		refLinesTime <- sapply(refLines, function(x) x$time)
		refLinesColor <- sapply(refLines, function(x) 
			ifelse("color" %in% names(x), x$color, refLinesColor)
		)
		refLinesLinetype <- sapply(refLines, function(x) 
			ifelse("linetype" %in% names(x), x$linetype, refLinesLinetype)
		)
		
	}else if(refLinesFromData){
		
		subjectIDPlot <- attr(gg, "metaData")$subjectID
		if(is.null(subjectIDPlot))
			warning("No reference lines are added to the plot with subject ID, because no 'subjectID' available.")
		
		refLinesInfo <- subset(refLinesData, 
			get(subjectVar) == subjectIDPlot &
			!is.na(get(refLinesLabelVar)) &
			!is.na(get(refLinesTimeVar))
		)
		refLinesLabels <- refLinesInfo[, refLinesLabelVar]
		refLinesTime <- refLinesInfo[, refLinesTimeVar]
		
	}
	
	res <- if(refLinesVect | refLinesFromData && length(refLinesTime) > 0){
				
		if(length(refLinesColor) == 1)
			refLinesColor <- rep(refLinesColor, length(refLinesLabels))
		if(length(refLinesLinetype) == 1)
			refLinesLinetype <- rep(refLinesLinetype, length(refLinesLabels))
		
		# add vertical lines
		for(i in seq_along(refLinesTime)){
			gg <- gg + geom_vline(
				xintercept = refLinesTime[i], 
				color = refLinesColor[i],
				linetype = refLinesLinetype[i], 
				alpha = 0.5,
				size = 1
			)
		}
		
		# add label at the bottom of the plot
		res <- if(addLabel){
					
			# extract number of lines for label
			nLinesRefLines <- max(nchar(refLinesLabels))/2
			
			# add label(s)
			colors <- setNames(refLinesColor, refLinesLabels)
			dataText <- data.frame(x = refLinesTime, label = refLinesLabels)
			ggRefLines <- ggplot(data = dataText) +
				geom_text(aes(x = x, label = label, colour = label, y = 0), 
					angle = 90, hjust = 0.5, show.legend = FALSE
				) + theme_void() +
				scale_color_manual(values = colors, limits = names(colors))
			if(!is.null(timeLim))	ggRefLines <- ggRefLines + coord_cartesian(xlim = timeLim)
			
			attributes(ggRefLines)$metaData$nLinesLabelRefLines  <- nLinesRefLines
		
#			nLinesPlot <- c(getNLinesYGgplot(gg), nLinesRefLines)
#			relHeights <- nLinesPlot/sum(nLinesPlot)
#			# combine all plots
#			ggT <- combineVerticallyGGplot(listPlots = list(gg, ggRefLines), height = relHeights, 
#				package = "cowplot")
			
#			attr(ggT, "metaData") <- c(attr(gg, "metaData"), list(nLinesLabelRefLines = nLinesRefLines))
			
			list(gg = gg, ggRefLines = ggRefLines)
			
		}else gg
				
	}else gg

	return(res)
	
}

#' Define LaTeX index based on specified variable(s)
#' of the dataset
#' @param subjects vector with subject IDs (based on the \code{subjectVar} variable)
#' @param data data.frame with data containing information on which the index should be based
#' @param var variable(s) of \code{data} of interest for the index
#' @inheritParams subjectProfileIntervalPlot
#' @return list with elements:
#' \itemize{
#' \item{'indexDef':}{string with LaTeX code for creation of index}
#' \item{'indexInfo': }{character vector, named with named with subject ID,
#'  containing LaTeX code for index for each subject
#' specified in \code{subjects} parameter.}
#' \item{'indexPrint': }{string with LaTeX code for printing/inclusion of index}
#' }
#' @importFrom plyr daply
#' @author Laure Cougnaud
defineIndex <- function(
	subjects, 
	data,
	var,
	subjectVar = "USUBJID",
	labelVars = NULL
){
	
	# Index creation:
	# extract name used in Index (labels are the variable column names)
	indexTitles <- getLabelVar(var, labelVars = labelVars)
	indexMake <- paste(
		paste0("\\makeindex[intoc,name=", names(indexTitles), ",title={Index on ",  indexTitles, "}]"),
		collapse = "\n"
	)
	
	# Index entry creation for each subject:
	# extract values of specified 'var'
	indexInfo <- daply(data, subjectVar, function(x){	
		indexX <- unlist(x[, var, drop = FALSE])
		if(nrow(x) > 1)
			stop("Multiple information available for subject: ", unique(x[, subjectVar]), 
				" for index construction.")
		paste(
			paste0("\\index[", names(indexX), "]{", indexX, "}"),
			collapse = " "
		)#	\index[person]{Heisenberg}
	})
	
	# Index printing:
	indexPrint <- paste(
		paste0("\\printindex[", names(indexTitles), "]"),
		collapse = "\n"
	)
	
	res <- list(indexMake = indexMake, indexEntry = indexInfo, indexPrint = indexPrint)
	
	return(res)
	
}

#' Combine vertically some plots
#' @param listPlots list of \code{\link[ggplot2]{ggplot2}} objects
#' @param heights vector with heights
#' @param package string with package name used to combine the pots, 'egg' or 'cowplot'
#' @return gtable
#' @importFrom cowplot plot_grid
#' @importFrom egg ggarrange
#' @author Laure Cougnaud
combineVerticallyGGplot <- function(listPlots, heights, package = c("egg", "cowplot")){
	
	package <- match.arg(package)
	
	res <- switch(package,
			
		'cowplot' = plot_grid(
			plotlist = listPlots,
			align = "v", ncol = 1, axis = "lr", rel_heights = heights
		),
		
		'egg' = {
			ggarrange(plots = listPlots, ncol = 1, heights = heights, 
				draw = FALSE
			)
			# because the function ggplot2::ggplotGrob is called internally
			# and open a new window
			tmp <- dev.off()
		}

	)
	
	
	return(res)
}
