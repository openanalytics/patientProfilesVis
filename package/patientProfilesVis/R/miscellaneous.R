#' get path of the any \code{file} template in the \code{CSRFigures} package
#' @param file file name (with extension)
#' @author Laure Cougnaud
#' @export
getPathTemplate <- function(file){
	system.file(file.path("template", file), package = "patientProfilesVis")
}

#' Get approximately the number of lines in the y-axis of the plot.
#' Can be used to specify plot-specific height during the export.
#' @param gg \code{\link[ggplot2]{ggplot2}} object
#' @return vector with (approximated) number of lines
#' @author Laure Cougnaud
#' @importFrom ggplot2 ggplot_build
#' @export
getNLinesYGgplot <- function(gg){
	nLinesPlot <- length(unique(ggplot_build(gg)$data[[1]]$y))
	nLinesTitleAndXAxis <- sum(unlist(lapply(ggplot_build(gg)$plot$labels[c("title", "x")], function(label)
		if(label != "")	length(unlist(strsplit(label, split = "\n"))) * 3
	)))
	nLines <- nLinesPlot + nLinesTitleAndXAxis
	return(nLines)
}