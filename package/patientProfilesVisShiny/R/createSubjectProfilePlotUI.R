#' Create subject profile plot(s) based on parameters from the Shiny UI
#' @param input Shiny input object
#' @param results reactiveValues object with list of results created from the server script
#' @return subject profile plot(s)
#' @author Laure Cougnaud
#' @export
createSubjectProfileUI <- function(input, results){

	listParams <- c(
		list(
			data = results$dataCurrent(),
			subjectVar = input$moduleSubjectVar,
			title = input$moduleTitle,
			label = input$moduleLabel,
			labelVars = results$labelVars(),
			paramGroupVar = input$moduleParamGroupVar,
			subsetVar = input$moduleSubsetVar,
			subsetValue = input$moduleSubsetValue
		),
		switch(input$moduleType,
			'text' = switch(input$moduleTextVarSpecType,
				'1' = list(paramValueVar = input$moduleTextParamValueVar),
				'2' = list(
					paramValueVar = input$moduleTextParamValueVarPair,
					paramNameVar = input$moduleTextParamNameVarPair
				)
			),
			'event' = list(
				paramVar = input$moduleParamVar,
				timeVar = input$moduleTimeVar,
				colorVar = input$moduleColorVar,
				shapeVar = input$moduleEventShapeVar
			),
			'interval' = list(
				paramVar = input$moduleParamVar,
				timeStartVar = input$moduleIntervalTimeStartVar,
				timeEndVar = input$moduleIntervalTimeEndVar,
				colorVar = input$moduleColorVar
			),
			'line' = list(
				paramNameVar = input$moduleLineParamNameVar,
				paramValueVar = input$moduleLineParamValueVar,
				paramValueRangeVar = input$moduleLineParamNameRangeVar,
				timeVar = input$moduleTimeVar,
				colorVar = input$moduleColorVar				
			)
		)
	)
	
	namesParam <- c(
		'data' = 'data', 'title' = 'title', 'label' = 'label',
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
	
	# remove empty optional parameters
	listParams <- listParams[sapply(listParams, function(x) 
		!(length(x) == 1 && x == "none")
	)]
	
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
	
	subjectProfileFct <- paste0("subjectProfile", simpleCap(input$moduleType), "Plot")
	plotsList <- do.call(subjectProfileFct, listParams)
	
	return(plotsList)
	
}
