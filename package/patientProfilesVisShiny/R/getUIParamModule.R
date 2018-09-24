#' Get list of module parameters specified in the UI
#' (to enable display of previously saved module)
#' @param input Shiny input object
#' @return list of parameters
#' @author Laure Cougnaud
#' @export
getUIParamModule <- function(input){
	
	moduleParamNames <- grep("^module", names(input), value = TRUE)
	paramsToRemove <- c("moduleChoice", "moduleSettings*") # "moduleTextVarSpecType"
	moduleParamNames <- grep(
		paste0(paramsToRemove, collapse = "|"),
		moduleParamNames,
		value = TRUE, invert = TRUE
	)
	# remove also parameters of other type of text module
	if(input$moduleType == "text"){
		paramsToRemoveText <- switch(
				input$moduleTextVarSpecType,
				"1" = c("moduleTextParamValueVarPair", "moduleTextParamNameVarPair"),
				"2" = "moduleTextParamValueVar"
		)
		moduleParamNames <- moduleParamNames[moduleParamNames != paramsToRemoveText]
	}
	paramsModule <- sapply(moduleParamNames, function(name)
		input[[name]]			
	, simplify = FALSE)
	paramsModule <- paramsModule[!sapply(paramsModule, is.null)]
	paramsModuleNameNew <- sub("Pair$", "",
		simpleCap(
			sub("^module(Event|Interval|Text|Line)*(.+)", "\\2", names(paramsModule)),
			rev = TRUE
		)
	)
	names(paramsModule) <-  paramsModuleNameNew
	return(paramsModule)
	
}

