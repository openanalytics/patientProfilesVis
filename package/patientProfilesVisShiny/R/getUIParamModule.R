#' Get list of module parameters specified in the UI
#' (to enable display of previously saved module)
#' @inheritParams getModuleParams
#' @inherit getModuleParams return
#' @author Laure Cougnaud
#' @export
getUIParamModule <- function(input){
	
	paramsModule <- getModuleParams(input = input, use = "UI")
	
	return(paramsModule)
	
}

#' Get list of module parameters.
#' @param input Shiny input object.
#' @param use String with use or the output, either:
#' \itemize{
#' \item{'UI': }{output will be displayed in the Shiny UI}
#' \item{'plotFunction': }{output will be used as input 
#' parameters for the plotting functions.
#' In this case, the \code{results} parameter should be specified.}
#' }
#' @param results reactiveValues object with list of results created from the server script.
#' @return list of parameters
#' @author Laure Cougnaud
getModuleParams <- function(input, 
	use = c("UI", "plotFunction"),
	results = NULL){

	use <- match.arg(use)
	if(use == "plotFunction" & is.null(results))
		stop("'results' should be specified if 'use' is 'plotFunction'.")
	
	# Get name of parameters
	moduleParamNames <- c(
			
		# parameters common to all module type
		if(use == "UI")	"moduleData", 
		"moduleSubjectVar", "moduleTitle",
		"moduleParamGroupVar", "moduleSubsetVar", "moduleSubsetValue",
		if(use == "UI")	"moduleType",
		
		# parameters specific of each module
		switch(
			input$moduleType,
			'text' = c(
				if(use == "UI")	"moduleTextVarSpecType",
				switch(input$moduleTextVarSpecType,
					'1' = "moduleTextParamValueVar",
					'2' = c("moduleTextParamValueVarPair", "moduleTextParamNameVarPair")
				)
			),
			'event'	= c(
				"moduleParamVar", "moduleTimeVar", 
				"moduleColorVar", "moduleEventShapeVar"
			),
			'interval' = c(
				"moduleParamVar", 
				"moduleIntervalTimeStartVar", "moduleIntervalTimeEndVar",
				"moduleColorVar", 
				if(use == "UI")	"moduleIntervalTimeLimSelect",
				switch(input$moduleIntervalTimeLimSelect,
					'fixed' = "moduleIntervalTimeLim",
					'subject-specific' = c(
						if(use == "UI")	"moduleIntervalTimeLimData", 
						"moduleIntervalTimeLimStartVar", "moduleIntervalTimeLimEndVar")
				)
			),
			'line' = c(
				"moduleLineParamNameVar", "moduleLineParamValueVar",
				"moduleLineParamValueRangeVar", "moduleTimeVar", "moduleColorVar"
				)
			)
	)
	
	# extract value of parameters specified in the UI
	paramsModule <- sapply(moduleParamNames, function(name)
		input[[name]]			
	, simplify = FALSE)
	
	# remove empty parameters
	paramsModule <- paramsModule[
		!sapply(paramsModule, is.null) & 
		!sapply(paramsModule, function(x) (length(x) == 1 && x == "none"))
	]
	
	# get names of parameters (names of input parameters of the plotting function)
	paramsModuleNameNew <- sub("Pair$", "",
		simpleCap(
			sub("^module(Event|Interval|Text|Line)*(.+)", "\\2", names(paramsModule)),
			rev = TRUE
		)
	)
	names(paramsModule) <-  paramsModuleNameNew
	
	if(use == "plotFunction"){
		paramsModuleFromResults <- list(
			data = results$dataCurrent(),
			labelVars = results$labelVars()
		)
		if(input$moduleType == "interval" && input$moduleIntervalTimeLimSelect == "subject-specific")
			paramsModuleFromResults <- c(
				paramsModuleFromResults,
				list(timeLimData = results$timeLimData())
			)
		paramsModule <- c(paramsModule, paramsModuleFromResults)
	}
				
	return(paramsModule)
	
}
