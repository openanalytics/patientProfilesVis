#' 

#' @return 
#' @author Laure Cougnaud

#' @importFrom patientProfilesVis getLabelVars
#' @export
subjectProfileSummary <- function(data, 
	paramVar = "PARAM",
	paramLab = getLabelVar(paramVar, labelVars = labelVars),
	var = "AVAL",
	varLab = getLabelVar(paramVar, labelVars = labelVars),
	by = "TRTP", 
	subjectVar = "USUBJID",
	includeTotal = TRUE,
	labelVars = NULL){
	
	# compute 
	summaryTable <- getSummaryTable(
		data = data, 
		paramVar = paramVar,
		var = var, 
		by = by,
		subjectVar = subjectVar,
		includeTotal = TRUE
	)
	
	# create the plot
	subjectProfileSummaryPlot(
		data = data
	)
		
	
	
}

subjectProfileSummaryPlot <- function(data, paramVar){
	
#	ggplot(data = summaryTable) +
		
	
}

#' Get summary statistics for a specific dataset and variables of interest
#' @param by string, variable of \code{data} with grouping variable,
#' if NULL (by default) no grouping variable is considered
#' @param paramVar string, variable of \code{data} with parameter,
#' 'PARAMCD' by default
#' @param includeTotal logical, if TRUE (by default) and \code{by} is specified,
#' include also the summary statistics for the entire dataset
#' @inheritParams getSummaryStatistics
#' @inherit getSummaryStatistics return
#' @author Laure Cougnaud
#' @importFrom dplyr ddply n_distinct
#' @importFrom plyr rbind.fill
#' @export
getSummaryTable <- function(data, 
	paramVar = "PARAM", 
	var = "AVAL", 
	by = "TRTP", 
	subjectVar = "USUBJID",
	includeTotal = TRUE
){
	
	# get general statistics (by group if specified)
	summaryTable <- ddply(data, c(paramVar, by), function(x){
		getSummaryStatistics(data = x, var = var)
	})
	
	# get statistics for the entire dataset
	if(includeTotal & !is.null(by)){
		summaryTableTotal <- ddply(data, paramVar, function(x)
			getSummaryStatistics(data = x, var = var)
		)
		summaryTable <- rbind.fill(summaryTable, summaryTableTotal)
	}
	
	return(summaryTable)
	
}

#' Get summary statistics of interest
#' @param data data.frame with data
#' @param var string, variable of \code{data} with variable to use,
#' 'AVAL' by default
#' @param USUBJID string, variable of \code{data} with subject ID,
#' 'USUBJID' by default
#' @return data.frame with summary statistics in columns:
#' \itemize{
#' \item{'N': }{number of subjects}
#' \item{'Mean': }{mean of \code{var}}
#' \item{'SD': }{standard deviation of \code{var}}
#' \item{'SE': }{standard error of \code{var}}
#' \item{'Median': }{median of \code{var}}
#' \item{'Min': }{minimum of \code{var}}
#' \item{'Max': }{maximum of \code{var}}
#' }
#' @author Laure Cougnaud
#' @export
getSummaryStatistics <- function(data, var,
	subjectVar = "USUBJID"){
	
	val <- data[, var]
	res <- data.frame(
		N = n_distinct(data[, subjectVar]),
		Mean = mean(val),
		SD = sd(val),
		SE = sd(val)/sqrt(length(val)),
		Median = median(val),
		Min = min(val),
		Max = max(val)
	)		
	
	return(res)
	
}