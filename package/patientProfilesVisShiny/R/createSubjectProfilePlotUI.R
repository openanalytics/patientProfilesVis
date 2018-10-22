#' Create subject profile plot(s) based on parameters from the Shiny UI
#' @inheritParams getModuleParams
#' @return subject profile plot(s)
#' @author Laure Cougnaud
#' @import shiny
#' @export
createSubjectProfileFromShinyInput <- function(input, results){

	listParams <- getModuleParams(
		input = input, 
		results = results,
		use = "plotFunction"
	)	
	
	namesParam <- c(
		'data' = 'data', 'title' = 'title', 'label' = 'label',
		'timeLim' = 'time limits',
		'timeLimData' = 'time limits dataset',
		'timeLimStartVar' = 'time limits start variable',
		'timeLimEndVar' = 'time limits end variable',
		'subjectVar' = "subject identifier",
		'paramGroupVar' = "grouping variable",
		'paramValueVar' = 'column with parameter value',
		'paramNameVar' = 'column with variable name',
		'paramValueRangeVar' = "columns with reference range",
		'paramVar' = "column with parameter",
		'timeVar' = "time variable",
		'facetVar' = "variable",
		'timeStartVar' = "start time variable",
		'timeEndVar' = "end time variable",
		'colorVar' = "color variable",
		'shapeVar' = "symbol variable",
		'subsetVar' = "filtering variable",
		'subsetValue' = "filtering value(s)"
	)
	
	# check if all parameter(s) are specified
	reqParam <- c("data", "title",
		switch(input$moduleType, 
			'text' = "paramValueVar",
			'event' = c("paramVar", "timeVar"),
			'interval' = c("paramVar", "timeStartVar", "timeEndVar"),
			'line' = c("paramNameVar", "paramValueVar", "timeVar")
		)
	)
	specParams <- sapply(listParams[reqParam], isTruthy)
	validate(need(all(specParams), 
		paste0("Some parameters (", toString(namesParam[names(which(!specParams))]), ") are missing.")
	))

	createSubjectProfileType(listParams = listParams, type = input$moduleType)

}

#' Create subject profile plot for list of parameters 
#' (e.g. sublist of output of the \code{\link{getDefaultModules}} function).
#' @param listParams list of parameters
#' @param data data.frame with data
#' @param labelVars named vector with label of the variables
#' @return subject profile plot(s)
#' @author Laure Cougnaud
#' @export
createSubjectProfileFromParam <- function(listParams, data, labelVars){
	
	type <- listParams$type
	listParams$data <- data[[listParams$data]]
	if("timeLimData" %in% names(listParams))
		listParams$timeLimData <- data[[listParams$timeLimData]]
	listParams$labelVars <- labelVars
	listParams$type <- listParams$timeLimSelect <- 
		listParams$exportSettings <- listParams$exportSettingsID <- 
		listParams$general <- listParams$varSpecType <- NULL

	idxParamsToRemove <- which(sapply(listParams, function(x) length(x) == 1 && x =="none"))
	if(length(idxParamsToRemove) > 0)
		listParams[idxParamsToRemove] <- NULL

	createSubjectProfileType(listParams, type = type)
	
}	


#' Create subject profile plot of specified type
#' @param listParams list of parameters
#' @param type string with type of profile plot
#' @return subject profile plot(s)
#' @author Laure Cougnaud
#' @export
createSubjectProfileType <- function(listParams, type){
	
	subjectProfileFct <- paste0("subjectProfile", simpleCap(type), "Plot")
	plotsList <- do.call(subjectProfileFct, listParams)
	
	return(plotsList)
	
}
