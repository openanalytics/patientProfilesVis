#' Convert specified variable to factor,
#' with ordered specified by an associated numeric variable
#' @param data data.frame with data
#' @param var character vector with variable(s)
#' @param varNum character vector with associated numeric variable(s)
#' @return data.frame with specified variable converted to factor
#' @author Laure Cougnaud
#' @export
convertVarToFactor <- function(data, var, varNum){
	
	if(length(var) != length(varNum))
		stop("'var' and 'varNum' are not of the same length.")
	
	data[, var] <- lapply(seq_along(var), function(i)
		reorder(data[, var[i]], X = data[, varNum[i]])		
	)
	
	return(data)
	
}
