#' Create plot of subject profiles for events.
#' 
#' There are two ways to specify the variables of interest to include:
#' \itemize{
#' \item{by specifying column(s) of interest containing parameter values, passed
#' to the \code{paramValueVar} parameter
#' }
#' \item{by specifying a combination of a variable containing the parameter name
#' (\code{paramNameVar}), coupled with a variable containing the 
#' parameter values (\code{paramValueVar})
#' }
#' }
#' @param paramValueVar string, variable of \code{data} containing the . 
#' Either:
#' \itemize{
#' \item{if used in combination with \code{paramNameVar}: }{
#' \itemize{
#' \item{string containing the variable of \code{data} with parameter
#' value to represent}
#' \item{function taking \code{data} as input and 
#' returning a new variable (of length equal to number of rows in \code{data}
#' ) with parameter value to represent}
#' }
#' }
#' \item{otherwise: }{vector with names of variable(s) (multiple possible) 
#' of \code{data} with parameter value to represent. 
#' If variables should be concatenated in the same line, 
#' they should be specified separated by '|', e.g 'SEX|AGE'.
#' }
#' }
#' @param paramNameVar (optional) string, variable of \code{data} with parameter name.
#' If specified, \code{paramValueVar} should be an unique variable.
#' @param paramGroupVar (optional) string, variable of \code{data} with grouping.
#' If specified, the parameters will be grouped by this variable in the y-axis, and
#' \code{paramValueVar} should be an unique variable.
#' @param paramVarSep string with character(s) used to concatenate multiple 
#' \code{paramNameVar} or \code{paramValueVar}, ' - ' by default.
#' @inheritParams subjectProfileIntervalPlot
#' @return list of \code{\link[ggplot2]{ggplot2} objects}, also of class
#' \code{subjectProfileTextPlot}
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom plyr dlply
#' @importFrom reshape2 melt
#' @export
subjectProfileTextPlot <- function(
	data,
	paramValueVar,
	paramNameVar = NULL,
	paramGroupVar = NULL,
	subjectVar = "USUBJID",
	xLab = "",
	yLab = "",
	title = "Subject information",
	label = title,
	labelVars = NULL,
	paramVarSep = " - "
){
		
	if(is.null(paramNameVar)){

		# in case variable should be concatenated
		varsToConcatenate <- grep("|", paramValueVar, value = TRUE, fixed = TRUE)
		if(length(varsToConcatenate) > 1){
			varsToConcatenateList <- strsplit(varsToConcatenate, split = "|", fixed = TRUE)
			data[, varsToConcatenate] <- lapply(varsToConcatenateList, function(vars){
				do.call(paste, c(as.list(data[, vars, drop = TRUE]), list(sep = paramVarSep)))
			})
			if(!is.null(labelVars))
				labelVars[varsToConcatenate] <- sapply(varsToConcatenateList, function(name)
					paste(labelVars[name], collapse = paramVarSep))
		}
		
		# transform data from wide to long format
		dataToTransform <- unique(data[, c(subjectVar, paramValueVar)])
		dataPlot <- melt(
			dataToTransform, 
			id.vars = subjectVar, 
			measure.vars = paramValueVar, 
			variable.name = "variable",
			value.name = "value"
		)
		
		# use the labels for the names of the variables
		# sort in revert order of specified parameters
		# to have the variable sorted from top to bottom in ggplot2
		dataPlot$variable <- if(!is.null(labelVars)){
			varsLabels <- getLabelVar(paramValueVar, labelVars = labelVars)
			factor(
				varsLabels[dataPlot$variable],
				levels = rev(labelVars[paramValueVar])
			)
		}else factor(dataPlot$variable, levels = rev(paramValueVar))

	}else{
		
		# extract the value to display in the plot
		data$value <- if(is.function(paramValueVar))
			paramValueVar(data)	else
			data[, paramValueVar]
		# in case multiple value for the same variable, concatenate them
		dataPlot <- ddply(data, c(subjectVar, paramNameVar, paramGroupVar), function(x)
			data.frame(value = paste(unique(x$value), collapse = paramVarSep))
		)
		colnames(dataPlot) <- c(subjectVar, 'variable', paramGroupVar, 'value')
		
		# if paramGroupVar is specified: change order levels of 'variable'
		if(!is.null(paramGroupVar))
			dataPlot[, "variable"] <- with(dataPlot, reorder(variable, get(paramGroupVar), unique))
		
	}
		
	# create the plot
	listPlots <- dlply(dataPlot, subjectVar, function(dataSubject){	
		gg <- ggplot(data = dataSubject) +
			geom_text(
				aes(x = 0, y = variable, label = value),
				hjust = 0,
				size = rel(2)
			) +
			xlim(c(0, 1)) +
			subjectProfileTheme() +
			theme(
				panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				axis.ticks = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank()
			) +
			labs(title = title, x = xLab, y = yLab)
		
		if(xLab == ""){
			marDefault <- theme_bw()$plot.margin
			marNew <- margin(t = marDefault[1], r = marDefault[2], 
				b = 0, l = marDefault[4], unit = "pt")
			gg <- gg + theme(plot.margin = marNew)
		}
		
		class(gg) <- c("subjectProfileTextPlot", class(gg))
		
		gg
		
	})

	# metaData:
	# stored plot label
	attr(listPlots, 'metaData') <- list(label = label)
	
	return(listPlots)
	
}