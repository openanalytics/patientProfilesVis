#' Load data from ADaM file(s).
#' 
#' This converts also date/time variable to R date/time class 
#' (see \code{\link{convertToDateTime}}) function.
#' @param files string with path to ADaM file(s)
#' @param convertToDate logical, if TRUE columns with date/time are converted to 
#' \code{\link{POSIXct}} format, which stores calendar date/time in R.
#' @param dateVars vector of columns in \code{data} containing date/time,
#' or pattern for this columns.
#' By default all columns ending with 'DTC' are used (dateVars is: 'DTC$').
#' @param verbose logical, if TRUE (by default) progress messages are printed during execution
#' @return list of data.frame with data of each ADAM file (if not empty),
#' with special attributes 'labelVars': named vector with label of the variables.
#' Each data.frame contains an additional column: 'dataset' specifying the name of the 
#' \code{files} it was read from.
#' @author Laure Cougnaud
#' @importFrom tools file_path_sans_ext
#' @importFrom haven read_sas
#' @importFrom plyr colwise
#' @export
loadDataADaMSDTM <- function(files, 
	convertToDate = FALSE, dateVars = "DTC$",
	verbose = TRUE){
	
	# extract ADaM name
	names(files) <- toupper(file_path_sans_ext(basename(files)))
	
	idxDuplFiles <- duplicated(names(files))
	if(any(idxDuplFiles))
		warning(sum(idxDuplFiles), "duplicated file name. These files",
			"will have the same name in the 'dataset' column.")
	
	# import SAS dataset format into R
	dataList <- sapply(names(files), function(name){
				
		if(verbose)
			message("Import ", name, " dataset.")
				
		# read data
		data <- as.data.frame(read_sas(files[name]))
		
		if(nrow(data) > 0){
		
			# save dataset name
			data <- cbind(data, DATASET = name)
			
			if(convertToDate){
				colsDate <- grep(dateVars, colnames(data), value = TRUE)
				data[, colsDate] <- lapply(colsDate, function(col){
					convertToDateTime(data[, col], colName = col)
				})
			}
			
			# column names in lower case for some datasets
			colnames(data) <- toupper(colnames(data))
			
		}else	if(verbose)	warning("Dataset ", name, " is empty.")
		
		data
	}, simplify = FALSE)

	# remove empty dataset(s)
	dataList <- dataList[!sapply(dataList, is.null)]
	
	# extract label variables
	labelVars <- c(getLabelVars(dataList), 'DATASET' = "Dataset Name")
	attr(dataList, "labelVars") <- labelVars
	
	return(dataList)

}

#' Convert character vector to date/time object
#' @param x character vector to convert to date/time
#' @param format string with possible format(s) of the input date/time in the ADaM dataset.
#' If multiple are specified, each format is tested successively, until at least one
#' element in the input vector is converted with the specified format
#' (non missing, following the approach described in
#' the \code{format} parameter of the \code{\link{strptime}} function).
#' See the 'Details' section of the help of the function,
#' for more information about this format.
#' @param colName string with name of column, used in message (if any).
#' @param verbose logical, if TRUE (by default) progress messages are printed during execution
#' @return vector of class \code{\link{POSIXct}}
#' @author Laure Cougnaud
#' @export
convertToDateTime <- function(x, format = c("%Y-%m-%dT%H:%M", "%Y-%m-%d"), 
	colName = NULL, verbose = TRUE){
	
	isEmpty <- function(x) is.na(x) | x == ""
	
	newTime <- .POSIXct(rep(NA_real_, length(x))) 
	for(formatI in format){
		idxMissingRecords <- which(is.na(newTime) & !isEmpty(x))
		newTime[idxMissingRecords] <- as.POSIXct(x[idxMissingRecords], format = formatI)
		if(all(!is.na(newTime[!isEmpty(x)])))
			break
	}
	
	if(any(is.na(newTime[!isEmpty(x)]))){
		if(verbose)
			message("Vector", if(!is.null(colName)) paste0(": ", colName), 
				" not of specified calendar date format, so is not converted to date/time format.")
		newTime <- x
	}else if(verbose)	message("Convert vector", if(!is.null(colName)) paste0(": ", colName), 
		" to calendar date/time format.")
	
	return(newTime)
	
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
