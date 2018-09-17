#' Get summary statistics for a specific dataset and variables of interest
#' @param totalInclude logical, if TRUE (by default) and \code{by} is specified,
#' include also the summary statistics for the entire dataset
#' @param totalLabel string label for the column with the total,
#' in case \code{totalInclude} is TRUE,
#' 'total' by default.
#' @param byWithin variable of \code{data} used 
#' for grouping. If \code{totalInclude} is TRUE, the total will be 
#' included across groups of this variable.
#' @param byAcross character vector with variable(s) of \code{data} used 
#' for grouping. No total is included for this column if \code{totalInclude} is TRUE. 
#' @inheritParams getSummaryStatistics
#' @inherit getSummaryStatistics return
#' @author Laure Cougnaud
#' @importFrom dplyr n_distinct
#' @importFrom plyr ddply rbind.fill
#' @export
getSummaryStatisticsTable <- function(data,  
	var = "AVAL", 
	byAcross = NULL,
	byWithin = NULL,
	subjectVar = "USUBJID",
	totalInclude = TRUE,
	totalLabel = "Total"
){
	
	# get general statistics (by group if specified)
	summaryTable <- ddply(data, c(byAcross, byWithin),function(x){
		getSummaryStatistics(data = x, var = var)
	})
	
	# get statistics for the entire dataset
	if(totalInclude & !is.null(byWithin)){
		summaryTableTotal <- ddply(data, byAcross, function(x)
			getSummaryStatistics(data = x, var = var)
		)
		summaryTableTotal[, byWithin] <- totalLabel
		summaryTable <- rbind.fill(summaryTable, summaryTableTotal)
	}
	
	return(summaryTable)
	
}

#' Get summary statistics of interest
#' @param data data.frame with data
#' @param var string, variable of \code{data} with variable to use,
#' 'AVAL' by default
#' @param subjectVar string, variable of \code{data} with subject ID,
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
#' @importFrom stats na.omit median sd
#' @export
getSummaryStatistics <- function(data, var,
	subjectVar = "USUBJID"){
	
	val <- data[, var]
	res <- data.frame(
		N = as.integer(n_distinct(data[, subjectVar])),
		Mean = mean(val),
		SD = sd(val),
		SE = sd(val)/sqrt(length(val)),
		Median = median(val),
		Min = min(val),
		Max = max(val)
	)		
	
	return(res)
	
}
