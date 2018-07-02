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
#' @param paramValueVar string, variable of \code{data} containing the parameter
#' value to represent. This should be of length 1 if used in combination with
#' \code{paramNameVar}, otherwise multiple are possible.
#' @param paramNameVar (optional) string, variable of \code{data} with parameter name.
#' If specified, \code{paramValueVar} should be an unique variable.
#' @inheritParams subjectProfileIntervalPlot
#' @return list of \code{\link[ggplot2]{ggplot2} objects}, also of class
#' \code{subjectProfileTextPlot}
#' @author Laure Cougnaud
#' @importFrom glpgUtility getLabelVar
#' @import ggplot2
#' @importFrom plyr dlply
#' @importFrom reshape2 melt
#' @export
subjectProfileTextPlot <- function(
	data,
	paramValueVar,
	paramNameVar = NULL,
	subjectVar = "USUBJID",
	xLab = "",
	yLab = "",
	title = "Subject information",
	labelVars = NULL
){
		
	if(is.null(paramNameVar)){

		# transform data from wide to long format
		dataPlot <- melt(
			data, id.vars = subjectVar, 
			measure.vars = paramValueVar, 
			variable.name = "variable",
			value.name = "value"
		)
		
		# use the labels for the names of the variables
		dataPlot$variable <- if(!is.null(labelVars)){
			varsLabels <- getLabelVar(paramValueVar, labelVars = labelVars)
			factor(
				varsLabels[dataPlot$variable],
				levels = labelVars[paramValueVar]
			)
		}else factor(dataPlot$variable, levels = paramValueVar)

	}else{
		
		dataPlot <- data[, c(subjectVar, paramNameVar, paramValueVar)]
		colnames(dataPlot) <- c(subjectVar, 'variable', 'value')
		
	}
		
	listPlots <- dlply(dataPlot, subjectVar, function(dataSubject){	
			
		# create the plot
		gg <- ggplot(data = dataSubject) +
			geom_text(
				aes(x = 0, y = variable, label = value),
				hjust = 0,
				size = rel(3)
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
	
		if(xLab == ""){
			marDefault <- theme_bw()$plot.margin
			marNew <- margin(t = marDefault[1], r = marDefault[2], 
				b = 0, l = marDefault[4], unit = "pt")
			gg <- gg + theme(plot.margin = marNew)
		}
	
		class(gg) <- c("subjectProfileTextPlot", class(gg))
		
		gg
		
	})
	
	return(listPlots)
	
}