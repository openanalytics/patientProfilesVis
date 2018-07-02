#' include list of plots with different figure height, size, label in a knitr document
#' 
#' Each plot will be included in a separated chunk.
#' Reason: fig.height and fig.width options are not vectorized.
#' Note: the 'results' knitr options of the chunk calling this function
#' should be set to 'asis'.
#' Note that a (one-level) list of plotly plots can be included directly via
#' htmltools::tagList(listPlots).
#' @param plotsList list of plots, e.g. \code{ggplot} objects
#' from the \code{ggplot2} package or from the \code{plotly} packags
#' @param generalLabel general label for the chunks, used to build the \code{labels}.
#' The labels are constructed as '\code{generalLabel}[i]',
#' with i the plot number (from sequence spanning the length of \code{plotsList}).
#' Only use if \code{labels} is not specified.
#' @param labels vector with labels for each chunk
#' @param type string with plot type, 'ggplot2' or 'plotly'
#' @param includeNewpage logical, if TRUE include newpage after each plot
#' @param ... any chunk parameters, will be replicated if necessary
#' See \code{\link{knitr}[opts_chunk]} for further details on available options.
#' @return no returned value, a text is printed with chunk content
#' @author Laure Cougnaud
#' @importFrom knitr knit_expand knit
#' @export
knitPrintListPlotsKnitr <- function(plotsList, 
	generalLabel = "plotsList",
	labels = paste0(generalLabel, seq_along(plotsList)), 
	type = c("ggplot2", "plotly"),
	includeNewpage = TRUE,
	...){
	
	type <- match.arg(type)
	
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
	chunkTemplate <- paste0(
		"<<{{label}}", 
		if(!is.null(argsChunkTxt)) paste0(", ", toString(argsChunkTxt)),
		">>=\n",
		if(type == "ggplot2")	"print(", 
		"plotsList[[{{i}}]]",
		if(type == "ggplot2")	")", 
		"\n",
		if(includeNewpage)	"cat('\\\\newpage\\n')\n",
		"@\n"
	)
	
	# vectorize over plots
	argsKnitExpand <- c(
		list(FUN = knit_expand, 
			text = chunkTemplate,
			i = seq_along(plotsList), 
			label = labels
		),
		argsChunk
	)
	chunkTxt <- do.call(mapply, argsKnitExpand)
#	cat(chunkTxt)
	
	# run chunks
	cat(knit(text = paste(chunkTxt, collapse = "\n"), quiet = TRUE))
	
}