#' Get list of module parameters specified in the UI
#' (to enable display of previously saved module)
#' @param input Shiny input object
#' @return list of parameters
#' @author Laure Cougnaud
#' @export
getUIParamModule <- function(input){
	
	moduleParamNames <- grep("^module", names(input), value = TRUE)
	paramsToRemove <- c("moduleChoice", "moduleTextVarSpecType", "moduleSettings*")
	moduleParamNames <- grep(
		paste0(paramsToRemove, collapse = "|"),
		moduleParamNames,
		value = TRUE, invert = TRUE
	)
	paramsModule <- sapply(moduleParamNames, function(name)
		input[[name]]			
	, simplify = FALSE)
	names(paramsModule) <- simpleCap(
		sub("^module(Event|Interval|Text|Line)*(.+)(Pair)*", "\\2", names(paramsModule)),
		rev = TRUE
	)
	return(paramsModule)
	
}

