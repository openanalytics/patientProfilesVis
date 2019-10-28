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
#' @param paramValueVar string, variable of \code{data}, iither:
#' \itemize{
#' \item{if used in combination with \code{paramNameVar}: }{
#' \itemize{
#' \item{vector with names of variable(s) (multiple possible) 
#' of \code{data} with parameter value to represent.
#' }
#' \item{function taking \code{data} as input and 
#' returning a new variable (of length equal to number of rows in \code{data}
#' ) with parameter value to represent}
#' }
#' }
#' \item{otherwise: }{vector with names of variable(s) (multiple possible) 
#' of \code{data} with parameter value to represent. 
#' If variables should be concatenated in the same line and not displayed as table: 
#' they should be specified separated by '|', e.g 'SEX|AGE'.
#' }
#' }
#' @param paramValueLab (optional) string with labels for \code{paramValueVar},
#' only used if the variabless are displayed as table.
#' @param paramNameVar (optional) string, variable of \code{data} with parameter name.
#' If specified, \code{paramValueVar} should be an unique variable.
#' @param paramGroupVar (optional) string, variable of \code{data} with grouping.
#' If specified, the parameters will be grouped by this variable in the y-axis, and
#' \code{paramValueVar} should be an unique variable.
#' @param paramVarSep string with character(s) used to concatenate multiple 
#' \code{paramNameVar} or \code{paramValueVar}, ' - ' by default.
#' @param table Logical, if TRUE (FALSE by default) the information
#' contained in the variables: \code{paramValueVar} is displayed as a table. 
#' Otherwise, the values of the different variables are concatenated in the same line.
#' @inheritParams subjectProfileIntervalPlot
#' @inheritParams formatParamVarTextPlot
#' @return list of (across subjects) of list (across modules) of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileTextPlot}, with additional metaData attributes containing
#' 'label' and 'timeLim'.
#' @author Laure Cougnaud
#' @import ggplot2
#' @importFrom gridExtra tableGrob ttheme_minimal
#' @importFrom plyr dlply ddply
#' @importFrom reshape2 melt
#' @importFrom glpgUtilityFct getLabelVar
#' @export
subjectProfileTextPlot <- function(
	data,
	paramValueVar,
	paramValueLab = getLabelVar(paramValueVar, labelVars = labelVars),
	paramNameVar = NULL, 
	paramGroupVar = NULL,
	subsetData = NULL, subsetVar = NULL, subsetValue = NULL, 
	subjectVar = "USUBJID", subjectSubset = NULL,
	xLab = "",
	yLab = "",
	title = "Subject information",
	label = title,
	labelVars = NULL,
	paramVarSep = " - ",
	formatReport = subjectProfileReportFormat(),
	paging = TRUE,
	table = FALSE, colWidth = NULL
){
	
	# in case data is a tibble:
	data <- as.data.frame(data)
	
	# only keep records of interest
	data <- filterData(
		data = data, 
		subsetData = subsetData,
		subsetVar = subsetVar, 
		subsetValue = subsetValue,
		subjectVar = subjectVar, 
		subjectSubset = subjectSubset
	)
	
	if(table){
		
		dataPlot <- data
		
	}else{
		
		combineMultipleVars <- function(vars)
			do.call(paste, c(as.list(data[, vars, drop = FALSE]), list(sep = paramVarSep)))
	
		if(is.null(paramNameVar)){
	
			# in case variable should be concatenated
			varsToConcatenate <- grep("|", paramValueVar, value = TRUE, fixed = TRUE)
			if(length(varsToConcatenate) > 1){
				varsToConcatenateList <- strsplit(varsToConcatenate, split = "|", fixed = TRUE)
				data[, varsToConcatenate] <- lapply(varsToConcatenateList, combineMultipleVars)
				if(!is.null(labelVars))
					labelVars[varsToConcatenate] <- sapply(varsToConcatenateList, function(name)
						toString(labelVars[name]))
			}
			
			# transform data from wide to long format
			dataToTransform <- unique(data[, unique(c(subjectVar, paramValueVar)), drop = FALSE])
			dataPlot <- melt(
				dataToTransform, 
				id.vars = subjectVar, 
				measure.vars = paramValueVar, 
				variable.name = "variable",
				value.name = "value"
			)
			dataPlot <- unique(dataPlot)
			
			# in case multiple value for the same variable, concatenate them
			dataPlot <- ddply(dataPlot, c(subjectVar, "variable"), function(x)
				data.frame(value = paste(unique(x$value), collapse = paramVarSep), stringsAsFactors = FALSE)
			)
			
			# use the labels for the names of the variables
			# sort in revert order of specified parameters
			# to have the variable sorted from top to bottom in ggplot2
			dataPlot$variable <- if(!is.null(labelVars)){
				varsLabels <- getLabelVar(paramValueVar, labelVars = labelVars)
				factor(
					unname(varsLabels[as.character(dataPlot$variable)]),
					levels = rev(labelVars[paramValueVar])
				)		
			}else factor(dataPlot$variable, levels = rev(paramValueVar))		
	
		}else{
	
			# extract the value to display in the plot
			data$value <- if(is.function(paramValueVar))
				paramValueVar(data)	else	combineMultipleVars(paramValueVar)
			# in case multiple value for the same variable, concatenate them
			dataPlot <- ddply(data, c(subjectVar, paramNameVar, paramGroupVar), function(x)
				data.frame(value = toString(unique(x$value)), stringsAsFactors = FALSE)
			)
			colnames(dataPlot)[which(colnames(dataPlot) == paramNameVar)] <- "variable"
			if(paramNameVar == subjectVar)	dataPlot[, subjectVar] <- dataPlot$variable
			
		}	
		
	}
		
	# create the plot
	listPlots <- dlply(dataPlot, subjectVar, function(dataSubject){	
				
		subject <- unique(dataSubject[, subjectVar])
#		print(subject)
		
		# if paramGroupVar is specified: change order levels of 'variable'
		# wrap text in multiple lines if too long
		dataSubject <- formatParamVarTextPlot(
			data = dataSubject, 
			paramVar = if(!table)	"variable", 
			paramValueVar = if(table)	paramValueVar	else	"value",
			paramValueLab = if(table)	paramValueLab,
			paramGroupVar = if(table || !is.null(paramNameVar))	paramGroupVar,
			revert = !is.null(paramNameVar), 
			formatReport = formatReport,
			table = table, colWidth = colWidth
		)
		colWidth <- attr(dataSubject, "colWidth")
		
		# split plot into multiple page(s)
		dataSubject <- getPageVar(
			data = dataSubject, 
			var = if(table)	paramValueVar	else	"variable", 
			typeVar = "y",
			formatReport = formatReport,
			title = !is.null(title),
			xLab = !is.null(xLab) && xLab != "",
			caption = FALSE,
			paging = paging, 
			table = table
		)
		
		listPlots <- dlply(dataSubject, "pagePlot", function(dataSubjectPage){
			
			if(table){
				
				dataTable <- dataSubjectPage[, paramValueVar, drop = FALSE]
				
				# set labels with same width that table content
				dataTableCols <- sapply(seq_along(paramValueVar), function(i)
					formatLongLabel(
						x = paramValueLab[paramValueVar][i], 
						width = colWidth[i]
					)
				)
				colnames(dataTable) <- dataTableCols
				
				themeTable <- ttheme_minimal(
					base_size = 9,
					padding = unit(c(0.2, 6), "mm"),
					core = list(fg_params = list(hjust = 0, x = 0.01)),
					colhead = list(fg_params = list(hjust = 0, x = 0.01))
				)
				gtable <- tableGrob(
					dataTable, theme = themeTable, 
					rows = NULL
				)
				if(!is.null(colWidth))
					gtable$widths <- unit(colWidth/sum(colWidth), "npc")
				gg <- ggplot() + annotation_custom(gtable)
			
			}else{
			
				gg <- ggplot(data = dataSubjectPage) + 
					geom_text(
						aes(x = 0, y = variable, label = value),
						hjust = 0,
						size = rel(3)
					)
			
			}
			
			gg <- gg + xlim(c(0, 1)) +
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
			
			## extract number of lines
			
			# plot content: labels y-axis and text
			if(table){
				nLinesMax <- apply(dataSubjectPage[, paramValueVar], 1, function(text) max(countNLines(text)))
				nLinesHeader <- max(countNLines(colnames(dataTable)))
				nLinesAll <- c(nLinesHeader, nLinesMax)
				nLinesPlot <- sum(nLinesAll) + 0.8 * (length(nLinesAll) - 1)
			}else{
				nLinesMax <- apply(dataSubjectPage[, c("variable", "value")], 1, function(text) max(countNLines(text)))
				nLinesPlot <- sum(nLinesMax) + 0.8 * (length(nLinesMax) - 1)
			}
			
			# in title and axes
			nLinesTitleAndXAxis <- sum(c(
				getNLinesLabel(value = title, elName = "title"), 
				getNLinesLabel(value = xLab, elName = "x")
			))
			nLines <- nLinesPlot + nLinesTitleAndXAxis
			
			## set plot attributes
			attr(gg, 'metaData') <- list(subjectID = subject, nLines = nLines)
			class(gg) <- c("subjectProfileTextPlot", class(gg))
			
			gg
			
		})
		
	})

	# metaData:
	# stored plot label
	attr(listPlots, 'metaData') <- list(label = label)
	
	return(listPlots)
	
}