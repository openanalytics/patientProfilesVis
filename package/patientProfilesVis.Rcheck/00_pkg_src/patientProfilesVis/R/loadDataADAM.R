#' Load data from ADaM file(s)
#' @param files string with path to ADaM file(s)
#' @return list with:
#' \itemize{
#' \item{'data': }{list of 
#' data.frame with data of each ADAM file
#' (if not empty)
#' }
#' \item{'labelVars': }{named vector with label of the variables}
#' }
#' @author Laure Cougnaud
#' @importFrom tools file_path_sans_ext
#' @importFrom haven read_sas
#' @export
loadDataADaM <- function(files){
	
	# extract ADaM name
	names(files) <- toupper(file_path_sans_ext(basename(files)))
	
	idxDuplFiles <- duplicated(names(files))
	if(any(idxDuplFiles))
		warning(sum(idxDuplFiles), "duplicated file name. These files",
			"will have the same name in the 'ADAM' column.")
	
	# import SAS dataset format into R
	dataList <- sapply(names(files), function(name){
				
		# read data
		data <- as.data.frame(read_sas(files[name]))
		
		if(nrow(data) > 0){
		
			# save dataset name
			data <- cbind(data, ADAM = name)
			
			# column names in lower case for some datasets
			colnames(data) <- toupper(colnames(data))
			
		}else	warning("Dataset ", name, " is empty.")
		
		data
	}, simplify = FALSE)

	# remove empty dataset(s)
	dataList <- dataList[!sapply(dataList, is.null)]
	
	# extract label variables
	labelVars <- getLabelVars(dataList)
	
	res <- list(data = dataList, labelVars = labelVars)

	return(res)

}

#' Get label of the variables in ADaM dataset(s)
#' @param dataList list with data.frame with ADaM dataset(s)
#' @return named vector with variable labels
#' @author Laure Cougnaud
#' @importFrom plyr ddply
#' @export
getLabelVars <- function(dataList){
	
	# the variable description are stored in the 'label' attribute of each column
	labelVarsList <- sapply(dataList, function(x){ 
		unlist(sapply(colnames(x), function(col) 
			attr(x[, col], which = "label"), 
			simplify = FALSE)
		)
	}, simplify = FALSE)

	# format to df
	labelVarsDfUnique <- data.frame(
		dataset = rep(names(labelVarsList), times = sapply(labelVarsList, length)),
		variable = unlist(labelVarsList),
		name = unlist(lapply(labelVarsList, names)),
		stringsAsFactors = FALSE
	)
	# combine across datasets
	labelVarsDfUnique <- ddply(labelVarsDfUnique, c("name", "variable"), function(x)
		c(dataset = toString(x$dataset))
	)
	
	# Note: used all along the analysis
	# vector with variable labels and paramcd as names
	labelVars <- labelVarsDfUnique$variable
	names(labelVars) <- labelVarsDfUnique$name
	
	return(labelVars)
	
}
