#' Create plot of subject profiles for events
#' @param vars vector with string names of variables of \code{data} to consider
#' @inheritParams subjectProfileRangePlot
#' @return list of \code{\link[ggplot2]{ggplot2} objects}
#' @author Laure Cougnaud
#' @importFrom glpgUtility getLabelVar
#' @import ggplot2
#' @importFrom plyr dlply
#' @importFrom reshape2 melt
#' @export
subjectProfileTextPlot <- function(
	data,
	vars, 
	paramVar, paramValueVar,
	subjectVar = "USUBJID",
	xLab = "",
	yLab = "",
	title = "Subject information",
	labelVars = NULL
){
	
	if(!missing(vars)){

		# transform data from wide to long format
		dataPlot <- melt(
			data, id.vars = subjectVar, 
			measure.vars = vars, 
			variable.name = "variable",
			value.name = "value"
		)
		
		# use the labels for the names of the variables
		dataPlot$variable <- if(!is.null(labelVars)){
			varsLabels <- getLabelVar(vars, labelVars = labelVars)
			factor(
				varsLabels[dataPlot$variable],
				levels = labelVars[vars]
			)
		}else factor(dataPlot$variable, levels = vars)

	}else if(!missing(paramVar) & !missing(paramValueVar)){
		
		dataPlot <- data[, c(subjectVar, paramVar, paramValueVar)]
		colnames(dataPlot) <- c(subjectVar, 'variable', 'value')
		
	}else		
		stop("The text variables should be specified via the parameter 'vars'",
			"or the combinations of the variables 'paramVar' and 'eventVar'.")
		
	listPlots <- dlply(dataPlot, subjectVar, function(dataSubject){	
			
		# create the plot
		gg <- ggplot(data = dataSubject) +
			geom_text(
				aes(x = 0, y = variable, label = value),
				hjust = 0
			) +
			xlim(c(0, 1)) +
			theme_bw() +
			theme(
				panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				axis.ticks = element_blank(),
				axis.text.x = element_blank(),
				axis.ticks.x = element_blank()
			) +
			labs(title = title, x = xLab, y = yLab)
		
	})
	
	return(listPlots)
	
}