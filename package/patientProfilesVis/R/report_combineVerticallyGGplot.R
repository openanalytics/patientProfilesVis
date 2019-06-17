#' Combine vertically multiple \code{\link[ggplot2]{ggplot}}.
#' 
#' If the different modules for a subject don't fit in the page, there
#' are automatically split in multiple pages.
#' @param listPlots a list of subject profile (subject/modules)
#' @param maxNLines maximum number of lines for a combined plot,
#' to fit in the page height
#' @param verbose logical, if TRUE print messages during execution
#' @param nCores Integer containing the number of cores used for the computation
#' (1 by default). If more than 1, computation is parallelized, in this case
#' the package \code{parallel} is required.
#' @param shiny logical, set to TRUE (FALSE by default) if the report is generated from a Shiny application.
#' @return a list of list of \code{\link[ggplot2]{ggplot}} object (subject/page)
#' @author Laure Cougnaud
#' @importFrom parallel makeCluster parSapply stopCluster
#' @importFrom ggplot2 ggplotGrob
#' @importFrom cowplot ggdraw draw_grob
#' @importFrom grDevices graphics.off
combineVerticallyGGplot <- function(listPlots, maxNLines, 
	nCores = 1, 
	shiny = FALSE, verbose = FALSE){

	if(shiny && !requireNamespace("shiny", quietly = TRUE))
		stop("The package 'shiny' is required to report progress.")
	msgProgress <- "Combine profiles across subjects/modules."
	if(verbose)	message(msgProgress)
	if(shiny)	incProgress(0.5, detail = msgProgress)
	
	# transform all plots to gtable
	isParallel <- (nCores > 1)
	if(isParallel){
		
		cl <- makeCluster(nCores)
		grobsAllSubjects <- parSapply(
			cl = cl,
			X = names(listPlots),
			FUN = function(subjID){	
				if(verbose)	message(paste0("Combine profile: ", subjID, " across modules."))
				lapply(listPlots[[subjID]], ggplotGrob)
			}, 
			simplify = FALSE
		)
		stopCluster(cl = cl)
		
	}else{
	
		grobsAllSubjects <- sapply(names(listPlots), function(subjID){	
			if(verbose)	message(paste0("Combine profile: ", subjID, " across modules."))
			lapply(listPlots[[subjID]], ggplotGrob)
		}, simplify = FALSE)
		# because each call to the function ggplot2::ggplotGrob open a new window:
		# shuts down all open graphics devices
		graphics.off()
		
	}

	# extract maximum margin(s)
	if(verbose)	message("Adjust margins.")
	grobsMarginsInfo <- sapply(grobsAllSubjects, function(x){
				
		lapply(x, function(y){
					
			sizes <- y$widths
			idxNull <- grep("null", sizes) # this delimit the position of the margin
			if(length(idxNull) > 1)
				stop("Multiple 'null' objects.")
			idxLeftMargin <- seq_len(min(idxNull)-1)
			leftMargin <- sum(sizes[idxLeftMargin])
			idxRightMargin <- seq(from = max(idxNull)+1, to = length(sizes))
			rightMargin <- sum(sizes[idxRightMargin])
			
			res <- list(
				idxLeftMargin = idxLeftMargin, idxRightMargin = idxRightMargin, 
				rightMargin = rightMargin, leftMargin = leftMargin
			)
			
		})

	}, simplify = FALSE)

	# extract maximum of left/right margin
	getMaxMargin <- function(label){
		marMax <- lapply(grobsMarginsInfo, function(x){
			mar <- lapply(x, function(y)	y[[label]])
			do.call(max, mar)
		})
		# call convertUnit to keep only results of computation
		do.call(max, marMax)
	}
	leftMarginMax <- getMaxMargin(label = "leftMargin")
	rightMarginMax <- getMaxMargin(label = "rightMargin")
	
	# adjust margins
	grobsAllSubjectsAligned <- sapply(names(grobsAllSubjects), function(patient){
		lapply(seq_along(grobsAllSubjects[[patient]]), function(iPlot){
					
			grob <- grobsAllSubjects[[patient]][[iPlot]]
			widths <- grob$widths
			marInfo <- grobsMarginsInfo[[patient]][[iPlot]]
			# adjust left margin
			widths[1] <- leftMarginMax - sum(widths[setdiff(marInfo$idxLeftMargin, 1)])
			# adjust right margin
			idxRightMarginMax <- max(marInfo$idxRightMargin)
			widths[idxRightMarginMax] <- rightMarginMax - sum(widths[setdiff(marInfo$idxRightMargin, idxRightMarginMax)])
			grob$widths <- widths
			
			grob
			
		})		
	}, simplify = FALSE)

	# split in separated page if doesn't fit in the page:
	if(verbose)	message("Include plots in multiple pages if required.")
	listCombinePlotsPage <- sapply(names(listPlots), function(patient){
				
		nLinesPlot <- attributes(listPlots[[patient]])$metaData$nLines
		plotPages <- getSplitVectorByInt(sizes = nLinesPlot, max = maxNLines)
		plots <- sapply(unique(plotPages), function(page){
			
			idx <- which(plotPages == page)
			grobsPatientPage <- grobsAllSubjectsAligned[[patient]][idx]
			nLinesPatientPage <- nLinesPlot[idx]
			nPlots <- length(grobsPatientPage)
			
			heights <- nLinesPatientPage/sum(nLinesPatientPage)
			yPos <- 1 - cumsum(heights)/sum(heights)
			
			gg <- ggdraw()
			for(iPlot in seq_along(grobsPatientPage)){
				geomGrob <- draw_grob(
					grob = grobsPatientPage[[iPlot]], 
					x = 0, y = yPos[iPlot], 
					width = 1, height = heights[iPlot], 
					scale = 1
				)
				gg <- gg + geomGrob
			}
			gg
			
			attributes(gg) <- c(attributes(gg), list(nLinesPlot = sum(nLinesPatientPage)))
			
			gg
			
		}, simplify = FALSE)

	}, simplify = FALSE)

	return(listCombinePlotsPage)

}

#' Split/combine a vector of size(s) to have a fixed combined size
#' @param sizes vector with size
#' @param max integer with maximum combined size in output
#' @return vector of same length as \code{sizeVect},
#' containing corresponding class
#' @author Laure Cougnaud
getSplitVectorByInt <- function(sizes, max){
	
	i <- 1
	class <- vector(length = length(sizes))
	curClass <- 1
	curNLines <- 0
	while(i <= length(sizes)){
		curNLines <- curNLines + sizes[i]
		if(curNLines > max){
			curClass <- curClass + 1
			curNLines <- sizes[i]
		}
		class[i] <- curClass
		i <- i + 1
	}
	
	return(class)
	
}
