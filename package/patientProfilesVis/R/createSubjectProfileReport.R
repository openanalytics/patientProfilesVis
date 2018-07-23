#' Create subject profile report
#' @param listPlots list of plots, as returned by the \code{\link{subjectProfileTextPlot}},
#' \code{\link{subjectProfileEventPlot}} or \code{\link{subjectProfileIntervalPlot}}
#' @param landscape logical, if TRUE the created report is in landscape format
#' @param outputFile string, path to the output report
#' @param exportFigures logical, if TRUE (FALSE by default) the figures are also exported
#' in png format in a 'figures' folder
#' @inheritParams subjectProfileCombine
#' @inheritParams defineIndex
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
	subjectVar = "USUBJID",
	landscape = FALSE,
	outputFile = "subjectProfile.pdf",
	exportFigures = FALSE,
	labelVars = NULL){
	
	# combine plots
	listPlotsPerSubject <- subjectProfileCombine(listPlots, 
		timeLim = timeLim, refLines = refLines,
		refLinesData = refLinesData, refLinesTimeVar = refLinesTimeVar, refLinesLabelVar = refLinesLabelVar,
		subjectVar = subjectVar
	)
	
	# extract bookmark(s) (if any)
	index <- if(!is.null(bookmarkData) & !is.null(bookmarkVar))
		defineIndex(
			subjects = names(listPlotsPerSubject), 
			data = bookmarkData,
			var = bookmarkVar,
			subjectVar = subjectVar,
			labelVars = labelVars
		)
	
	pathTemplate <- getPathTemplate("subjectProfile.Rnw")
	
	outputDir <- normalizePath(dirname(outputFile), winslash = "/")
	
	## input parameters for the child document:
	# save them in a new environment, passed to the 'knitr::knit' function
	inputParametersEnv <- new.env()
	inputParameters <- list(
		listPlotsPerSubject = listPlotsPerSubject,
		landscape = landscape,
		outputDir = outputDir,
		index = index		
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
#' @inheritParams addReferenceLinesProfilePlot
#' @return a list of \code{subjectProfilePlot} object, containing the combined
#' profile plots for each subject.
#' This is in essence only a \code{\link[ggplot2]{ggplot2}} objects,
#' (with the additional attribute 'nLinesPlot')
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @importFrom ggplot2 coord_cartesian ggplot theme_bw ggtitle
#' @inheritParams subjectProfileIntervalPlot
#' @author Laure Cougnaud
#' @export
subjectProfileCombine <- function(listPlots, 
	timeLim = getXLimSubjectProfilePlots(listPlots),
	subjectVar = "USUBJID",
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL){
	
	# extract all subjects for which at least one plot is available
	subjects <- sort(unique(unlist(lapply(listPlots, names))))
	
	# create empty element in the list if the plot is not available for a certain subject
	listPlotsAll <- lapply(listPlots, function(x){
		list <- x[subjects]
		names(list) <- subjects # in case plot not available for one subject
		attr(list, 'metaData') <- attr(x, 'metaData')
		list
	})
	
	## combine plots
	
	# wrapper function to combine ggplot2 objects
	combineGGPlots <- function(..., labels){
		
		listGgPlotsToCombine <- list(...)
		
		# 'empty' plot in case a specific plot is not available for this subject
		isEmpty <- which(sapply(listGgPlotsToCombine, is.null))
		if(any(isEmpty)){
			listGgPlotsToCombine[isEmpty] <- lapply(isEmpty, function(i)
				if(labels[i] != ""){
					gg <- ggplot() + theme_bw() + 
						ggtitle(paste("No", labels[i], "available."))
					class(gg) <- c("subjectProfileEmptyPlot", class(gg))
					gg
				}
			)
		}
		listGgPlotsToCombine <- listGgPlotsToCombine[!sapply(listGgPlotsToCombine, is.null)]
		
		if(length(listGgPlotsToCombine) > 0){
			
			# extract number of lines in the y-axis
			nLinesPlot <- sapply(listGgPlotsToCombine, getNLinesYGgplot)
			
			# set same limits for the time/x-axis and reference lines
			plotsToModify <-  which(sapply(listGgPlotsToCombine, function(gg)
				!inherits(gg, "subjectProfileTextPlot") & !inherits(gg, "subjectProfileEmptyPlot")
			))
			if(length(plotsToModify) > 0){
				
				newPlots <- lapply(plotsToModify, function(i){
					
					gg <- listGgPlotsToCombine[[i]]
					gg <- gg + coord_cartesian(xlim = timeLim)
					
					gg <- addReferenceLinesProfilePlot(
						gg = gg, 
						subjectVar = subjectVar,
						refLines = refLines,
						refLinesData = refLinesData,
						refLinesTimeVar = refLinesTimeVar,
						refLinesLabelVar = refLinesLabelVar,
						addLabel = (i == plotsToModify[length(plotsToModify)])
					)
					
				})
		
				listGgPlotsToCombine[plotsToModify] <- newPlots
				nLinesPlot[plotsToModify] <- nLinesPlot[plotsToModify] +
					sapply(newPlots, function(gg){
						nLinesRef <- attributes(gg)$metaData$nLinesLabelRefLines
						ifelse(is.null(nLinesRef), 0, nLinesRef)
					})
			}

			# relative height of each plot
			relHeights <- nLinesPlot/sum(nLinesPlot)
			
			# combine all plots
			plot <- do.call(plot_grid,
				c(
					listGgPlotsToCombine,
					list(align = "v", ncol = 1, axis = "lr",
						rel_heights = relHeights
					)
				)
			)
			
			# store the number of lines in the y-axis (used to adapt size during export)
			attributes(plot) <- c(attributes(plot), 
				list(nLinesPlot = sum(nLinesPlot), nSubplots = length(listGgPlotsToCombine))
			)
			
			class(plot) <- c("subjectProfilePlot", class(plot))
			
			plot
			
		}
		
	}
	
	# combine all plots per subject
	# this returns a list of 'gtable' object
	plotLabels <- sapply(listPlotsAll, function(x){
		label <- attributes(x)$metaData$label
		ifelse(is.null(label), "", label)
	})
	listPlotsPerSubject <- do.call(mapply, 
		c(
			list(FUN = combineGGPlots, SIMPLIFY = FALSE, MoreArgs = list(labels = plotLabels)),
			listPlotsAll
		)
	)
		
	# add title
	listPlotsPerSubject <- sapply(names(listPlotsPerSubject), function(subject){
		
		# create a ggplot for title only
		title <- ggdraw() + 
			draw_label(paste(" Subject:", subject), fontface = 'bold', 
				x = 0, hjust = 0, lineheight = 2
			)
		
		plotSubject <- listPlotsPerSubject[[subject]]
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

	return(listPlotsPerSubject)
	
}

#' Get limits for a list of plots.
#' 
#' These limits are extracted from the maximal range
#' of the x-coordinates across all plots
#' @param listPlots list of \code{subjectProfile[X]Plot} plots
#' @return vector of length 2 with limits for the x-axis
#' @importFrom ggplot2 ggplot_build
#' @author Laure Cougnaud
getXLimSubjectProfilePlots <- function(listPlots){
	
	xlimList <- lapply(listPlots, function(list)
		lapply(list, function(gg) 
			if(!inherits(gg, "subjectProfileTextPlot"))
				range(ggplot_build(gg)$data[[1]]$x)
		)
	)
	
	timeLim <- range(unlist(xlimList, recursive = TRUE), na.rm = TRUE)
	
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
#' @inheritParams subjectProfileIntervalPlot
#' @return if \code{addLabel} is TRUE, \code{\link[gtable]{gtable}} object,
#' \code{\link[ggplot2]{ggplot2}} otherwise
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom grid textGrob gpar
addReferenceLinesProfilePlot <- function(
	gg, 
	subjectVar = "USUBJID",
	refLines = NULL,
	refLinesData = NULL,
	refLinesTimeVar = NULL,
	refLinesLabelVar = NULL,
	refLinesColor = "black",
	refLinesLinetype = "dotted",
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
	
	res <- if(refLinesVect | refLinesFromData){
				
		if(length(refLinesColor) == 1)
			refLinesColor <- rep(refLinesColor, length(refLinesLabels))
		if(length(refLinesLinetype) == 1)
			refLinesLinetype <- rep(refLinesLinetype, length(refLinesLabels))
		
		# add vertical lines
		gg <- gg + geom_vline(
			xintercept = refLinesTime, 
			color = refLinesColor,
			linetype = refLinesLinetype, 
			alpha = 0.5,
			size = 1
		)
		
		# add label at the bottom of the plot
		res <- if(addLabel){
					
			# extract number of lines for label
			nLinesRefLines <- max(nchar(refLinesLabels))/2
			
			# increase bottom margin
			gg <- gg + theme(plot.margin = unit(c(1, 3, nLinesRefLines + 3, 1), "lines"))
			
			# add label(s)
			for(i in seq_along(refLinesLabels)){
				x <- refLinesTime[i]
				gg <- gg + annotation_custom(
					grob = textGrob(
						refLinesLabels[i], rot = 90, hjust = 1,
						gp = gpar(col = refLinesColor[i])
					), 
					xmin = x, xmax = x, ymin = -3, ymax = -3
				)
			}
			
			# clip off the plot panel (otherwise the label doesn't appear)
			ggT <- ggplot_gtable(ggplot_build(gg))
			ggT$layout$clip[ggT$layout$name == "panel"] <- "off"
			#	grid.draw(ggT)
			
			attr(ggT, "metaData") <- c(attr(gg, "metaData"), list(nLinesLabelRefLines = nLinesRefLines))
			
			ggT
			
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
		indexX <- unlist(x[, var])
		if(nrow(x) > 1)
			stop("Multiple information available for subject: ", unique(x[, subjectVar]), 
				" for index construction.")
		paste(
			paste0("\\index[", names(indexX), "]{", indexX, "}"),
			collapse = " "
		)#	\index[person]{Heisenberg}
	})
	indexInfoSubjects <- indexInfo[subjectVar]
	
	# Index printing:
	indexPrint <- paste(
		paste0("\\printindex[", names(indexTitles), "]"),
		collapse = "\n"
	)
	
	res <- list(indexMake = indexMake, indexEntry = indexInfo, indexPrint = indexPrint)
	
	return(res)
	
}
