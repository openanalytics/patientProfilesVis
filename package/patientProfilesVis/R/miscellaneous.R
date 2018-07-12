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

#' Get label(s) for a variable of the dataset
#' 
#' The label(s) are extracted either:
#' \itemize{
#' \item{if \code{data} is specified: }{from the 'label' attribute 
#' of the corresponding column in \code{data}
#' }
#' \item{if \code{labelVars} is specified: }{
#' from the specified vector of labels}
#' } 
#' @param var variable of data
#' @param data data.frame with data
#' @param labelVars named string with variable labels (names are the variable code)
#' @return string with label, \code{var} is no label is available
#' @author Laure Cougnaud
#' @export
getLabelVar <- function(var, data = NULL, labelVars = NULL){
	res <- if(!is.null(var)){
		if(is.null(data) & is.null(labelVars)){
			res <- var
			names(res) <- var
		}else{
			#		stop("'data' or 'labelVars' should be specified for the label(s) extraction.")
			res <- sapply(var, function(x){
						attrX <- if(!is.null(labelVars))
									labelVars[x]	else
									attr(data[, x], "label")
						ifelse(is.null(attrX), x, attrX)
					})
		}
	}
	return(res)
}