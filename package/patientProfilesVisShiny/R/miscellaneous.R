#' capitalize the first letter of a word, from the help of the 'toupper' function
#' @param x string
#' @param onlyFirst logical, if TRUE (FALSE by default)
#' capitalize the first letter of the first forward only
#' @param rev logical, if TRUE (FALSE by default), set first letter to lower case (otherwise upper case)
#' @return string with first letter capitalized
#' @author author of the 'toupper' function?
#' @export
simpleCap <- function(x, onlyFirst = FALSE, rev = FALSE) {
	paste0c <- function(...) paste(..., sep = "", collapse = " ")
	fctToUse <- get(ifelse(rev, "tolower", "toupper"))
	simpleCap1 <- function(s) paste0c(fctToUse(substring(s, 1, 1)), substring(s, 2))
	sapply(x, function(x){	
		s <- strsplit(x, " ")[[1]]
		if(onlyFirst)	paste0c(c(simpleCap1(s[1]), s[-1]))	else	simpleCap1(s)
	})
}


#' Get variable labels for the interface.
#' This concatenate the variable code (column in data) and variable labels
#' @param data data.frame with data
#' @param labelVars named string with variable labels (names are the variable code)
#' @return named character vector with variables
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @export
getVarLabelsForUI <- function(data, labelVars){
	vars <- colnames(data)
	names(vars) <- paste0(getLabelVar(var = vars, labelVars = labelVars), " (", vars, ")")
	return(vars)	
}

#' Get potential time variables in the dataset
#' @inheritParams getVarLabelsForUI
#' @return named character vector with potential time variables
#' @author Laure Cougnaud
#' @importFrom plyr colwise
#' @export
getTimeVars <- function(data, labelVars){
	allVars <- getVarLabelsForUI(data = data, labelVars = labelVars)
	possibleVars <- names(which(unlist(colwise(is.numeric)(data))))
	possibleVars <- allVars[match(possibleVars, allVars)]
	return(possibleVars)
}