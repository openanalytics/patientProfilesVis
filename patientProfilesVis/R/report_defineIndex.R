#' Define LaTeX index based on specified variable(s)
#' of the dataset
#' @param subjects vector with subject IDs (based on the \code{subjectVar} variable)
#' @param data data.frame with data containing information on which the index should be based
#' @param var variable(s) of \code{data} of interest for the index
#' @inheritParams patientProfilesVis-common-args
#' @return list with elements:
#' \itemize{
#' \item{'indexMake': string with LaTeX code for creation of index, 
#' to be included directly with \code{\link{cat}} in a knitr document
#' (two backslashes)
#' }
#' \item{'indexEntry': character vector, named with named with subject ID,
#'  containing LaTeX code for index for each subject
#' specified in \code{subjects} parameter, to be included in a knitr document
#'  as text (four backslashes)}
#' \item{'indexPrint': string with LaTeX code for printing/inclusion of index, 
#' to be included directly with \code{\link{cat}} in a knitr document
#' (two backslashes)}
#' }
#' @importFrom plyr daply
#' @importFrom clinUtils getLabelVar
#' @author Laure Cougnaud
defineIndex <- function(
	subjects, 
	data,
	var,
	subjectVar = "USUBJID",
	labelVars = NULL
){
	
	varsNotInData <- setdiff(c(subjectVar, var), colnames(data))
	
	if(length(varsNotInData) == 0){
	
		# Index creation:
		# extract name used in Index (labels are the variable column names)
		indexTitles <- getLabelVar(var, labelVars = labelVars)
		indexMake <- paste(
			paste0("\\makeindex[intoc,name=", names(indexTitles), ",columns=1,title={Index based on ",  indexTitles, "}]"),
			collapse = "\n"
		)
		
		# Index entry creation for each subject:
		# extract values of specified 'var'
		indexInfo <- daply(data, subjectVar, function(x){	
			xVar <- x[, var, drop = FALSE]
			xVar <- lapply(xVar, as.character) # in case var is a factor
			indexX <- unlist(xVar)
			if(nrow(x) > 1)
				stop("Multiple information available for subject: ", unique(x[, subjectVar]), 
					" for index construction.")
			paste(
				paste0("\\index[", names(indexX), "]{", indexX, "}"),
				collapse = " "
			)
		})
		
		# Index printing:
		indexPrint <- paste(
			paste0("\\printindex[", names(indexTitles), "]"),
			collapse = "\n"
		)
		
		res <- list(indexMake = indexMake, indexEntry = indexInfo, indexPrint = indexPrint)
			
	}else{
		warning("No bookmark is created because the variable(s): ", 
			toString(sQuote(varsNotInData)), 
			" are not available in the data.")
		res <- NULL
	}
	
	return(res)
	
}
