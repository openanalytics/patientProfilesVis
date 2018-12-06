#' include list of plots with different figure height, size, label in a knitr document
#' 
#' Each plot will be included in a separated chunk.
#' Reason: fig.height and fig.width options are not vectorized.
#' @param plotsList list of plots, e.g. \code{\link{ggplot2}[ggplot]}
#' @param generalLabel general label for the chunks, used to build the \code{labels}.
#' The labels are constructed as '\code{generalLabel}[i]',
#' with i the plot number (from sequence spanning the length of \code{plotsList}).
#' Only use if \code{labels} is not specified.
#' @param labels vector with labels for each chunk
#' @param titles vector with section titles
#' @param titleLevel integer with level for section header
#' @param ... any chunk parameters, will be replicated if necessary
#' See \code{\link{knitr}[opts_chunk]} for further details on available options.
#' @param type string with plot type, 'ggplot2', 'plotly' or 'gtableBaAP'
#' @return no returned value, a text is printed with chunk content
#' @author Laure Cougnaud
#' @importFrom knitr knit_expand knit
#' @export
knitPrintListPlots <- function(
	plotsList, 
	generalLabel = "plotsList",
	labels = paste0(generalLabel, seq_along(plotsList)), 
	titles = NULL, titleLevel = 2,
	type = c("ggplot2", "plotly"),
#	fig.height = 5, fig.width = 5,
#	fig.cap = NULL,
#	basePath = "./figures",
	...){
	
	type <- match.arg(type)
	
	# based on Rmd
#	tmp <- sapply(seq_len(length(plotsList)), function(i){
#				
#		# export plots
#		file <- file.path(basePath, paste0(labels[i], ".png"))
#		png(file, width = fig.width*100, height = fig.height*100)	
#		print(plotsList[[i]])
#		dev.off()
#		
#		# include plots
#		figCap <- if(!is.null(fig.cap))	fig.cap[i]	else	""
#		cat("![", figCap, "](", file, ")\n", sep = "")
#						
#	})
	

	# based on chunks
	
	# escape any non alnum character in the chunk label
	labels <- paste0("'", labels, "'")
	
	# additional chunk arguments
	argsChunk <- list(...)
	if("fig.cap" %in% names(argsChunk)){
		argsChunk[["fig.cap"]] <- paste0("'", argsChunk[["fig.cap"]], "'")
	}
	argsChunkTxt <- if(length(argsChunk) > 0)
		paste0(names(argsChunk), "=", "{{", names(argsChunk), "}}"
		)
	
	# chunk general template
	# seems that plot object cannot be passed as argument to knit_expand?
	chunkTemplate <- paste0("```{r {{label}}, ", toString(argsChunkTxt), "}\n",
		if(!is.null(titles))	
			paste0("cat('", paste(rep("#", titleLevel), collapse = ""), " {{title}}\\n')\n"),
		if(type %in% c("ggplot2"))	"print(", 
		"plotsList[[{{i}}]]",
		if(type %in% c("ggplot2"))	")", 
		"\n",
		"```\n"
	)
	
	# vectorize over plots
	argsKnitExpand <- c(
		list(FUN = knit_expand, text = chunkTemplate,
			i = seq_along(plotsList), 
			label = labels
		),
		if(!is.null(titles))	list(title = titles),
		argsChunk
	)
	chunkTxt <- do.call(mapply, argsKnitExpand)
	
	# run chunks
	cat(knit(text = paste(chunkTxt, collapse = "\n"), quiet = TRUE))
	
}