#' Visualize text-information in subject profiles.
#' 
#' There are two ways to specify the variables of interest to include:
#' \itemize{
#' \item{by specifying column(s) of interest containing parameter values, passed
#' to the \code{paramValueVar} parameter.\cr
#' In this case, variable value is displayed in the plot area,
#' and variable name in the label of the y-axis, as:\cr
#' variable 1 | value 1 - value 2 - ...\cr
#' variable 2 | value 1 - value 2 - ...
#' }
#' \item{by specifying column(s) of interest containing parameter values,
#' displayed as a \code{table}. \cr
#' In this case, variable are displayed in columns
#' in the plot area. Variable names are displayed 
#' on top of table, and associated values below, as:\cr
#' | variable 1      variable 2\cr
#' | value 1         value 1
#' | ...
#' }
#' \item{by specifying a combination of a variable containing the parameter name
#' (\code{paramNameVar}), coupled with a variable containing the 
#' parameter values (\code{paramValueVar}).\cr
#' In this case, parameter values (if multiple) are concatenated 
#' and displayed in the plot area for each parameter name,
#' displayed in the label of the y-axis, as:\cr
#' variable name 1 | variable value 1 - variable value 2 - ...\cr
#' variable name 2 | variable value 1 - ...
#' }
#' }
#' @param paramValueVar Character vector, either:
#' \itemize{
#' \item{vector with names of variable(s) (multiple are possible) 
#' of \code{data} of interest.
#' The values are displayed in the plot area and variable name in the labels
#' of the y-axis.\cr
#' Multiple variables can be concatenated in the same line by specifying them,
#' as an unique string separated by a 'pipe', e.g 'SEX|AGE'. Variable
#' label(s) are concatenated (with ', ') and displayed in the y-axis.}
#' \item{if \code{paramNameVar} is specified: 
#' \itemize{
#' \item{character vector with names of variable(s) (multiple possible) 
#' of \code{data} with values to represent in the plot area.
#' }
#' \item{function taking \code{data} as input and 
#' returning a new variable (of length equal to number of rows in \code{data}) 
#' with parameter value to represent}
#' }}
#' }
#' @param paramValueLab (optional) Named character vector with
#' labels for \code{paramValueVar}.
#' @param paramNameVar (optional) Character vector of length 1 with
#' variable of \code{data} with parameter name. This
#' is displayed in the labels of the y-axis.
#' @param paramVarSep String (' - ' by default) 
#' with character(s) used to concatenate multiple 
#' variables for the same record in the plot area.
#' @param table Logical, if TRUE (FALSE by default) the information
#' contained in the variables: \code{paramValueVar} is displayed as a table. 
#' Otherwise, the values of the different variables are concatenated in the same line.
#' @param title String with title, 'Subject information' by default.
#' @inheritParams patientProfilesVis-common-args
#' @inheritParams filterData
#' @inheritParams formatParamVarTextPlot
#' @inheritParams getPageVar
#' @return list of (across subjects) of list (across modules) of \code{\link[ggplot2]{ggplot2} objects}, 
#' also of class \code{subjectProfileTextPlot}, with additional \code{metaData} 
#' attributes containing
#' '\code{label}' and 'timeLim'.
#' @author Laure Cougnaud
#' @family patient profiles plotting function
#' @import ggplot2
#' @importFrom gridExtra tableGrob ttheme_minimal
#' @importFrom plyr dlply ddply
#' @importFrom reshape2 melt
#' @importFrom clinUtils getLabelVar formatLongLabel
#' @export
subjectProfileTextPlot <- function(
	data,
	paramValueVar,
	paramValueLab = getLabelVar(paramValueVar, labelVars = labelVars),
	paramNameVar = NULL, 
	paramGroupVar = NULL,
	subsetData = NULL, subsetVar = NULL, subsetValue = NULL, 
	subjectVar = "USUBJID", subjectSubset = NULL,
	subjectSample = NULL, seed = 123,
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
	
	# check if specified variable(s) are available in the data
	checkVar(var = subjectVar, data = data)
	checkVar(var = paramGroupVar, data = data)
	checkVar(var = paramNameVar, data = data)
	
	# only keep records of interest
	data <- filterData(
		data = data, 
		subsetData = subsetData,
		subsetVar = subsetVar, 
		subsetValue = subsetValue,
		subjectVar = subjectVar, 
		subjectSubset = subjectSubset,
		subjectSample = subjectSample, 
		seed = seed
	)
	
	if(table){
		
		dataPlot <- data
		
	}else{
		
		combineMultipleVars <- function(vars){
			checkVar(var = vars, data = data)
			do.call(paste, c(as.list(data[, vars, drop = FALSE]), list(sep = paramVarSep)))
		}
		
		if(is.null(paramNameVar)){
	
			# in case variable should be concatenated
			varsToConcatenate <- grep("|", paramValueVar, value = TRUE, fixed = TRUE)
			if(length(varsToConcatenate) > 0){
				
				varsToConcatenateList <- strsplit(varsToConcatenate, split = "|", fixed = TRUE)
				data[, varsToConcatenate] <- lapply(varsToConcatenateList, combineMultipleVars)
				varsToConcatenateLab <- sapply(varsToConcatenateList, function(vars)
					toString(getLabelVar(var = vars, labelVars = labelVars, label = paramValueLab))
				)
				paramValueLab[varsToConcatenate] <- varsToConcatenateLab
			}
			
			# transform data from wide to long format
			dataToTransform <- unique(data[, unique(c(subjectVar, paramValueVar)), drop = FALSE])
			# to avoid warning in 'melt': 
			# - 'attributes are not identical across measure variables; they will be dropped'
			if(!is.null(paramValueVar))
				dataToTransform[, paramValueVar] <- lapply(
					dataToTransform[, paramValueVar, drop = FALSE], 
					as.character
				)
			dataPlot <- melt(
				dataToTransform, 
				id.vars = subjectVar, 
				measure.vars = paramValueVar, 
				variable.name = "variable",
				value.name = "value"
			)
			dataPlot <- unique(dataPlot)
			
			# in case multiple value for the same variable or subject, concatenate them
			dataPlot <- ddply(dataPlot, c(subjectVar, "variable"), function(x)
				data.frame(value = paste(unique(x$value), collapse = paramVarSep), stringsAsFactors = FALSE)
			)
			
			# use the labels for the names of the variables
			# sort in revert order of specified parameters
			# to have the variable sorted from top to bottom in ggplot2
			varsLabels <- getLabelVar(
				paramValueVar, 
				labelVars = labelVars, 
				label = paramValueLab
			)
			dataPlot$variable <- factor(
				unname(varsLabels[as.character(dataPlot$variable)]),
				levels = varsLabels[paramValueVar]
			)	
	
		}else{
	
			if(length(paramNameVar) > 1)
				stop("'paramNameVar' should be of length 1.")
			
			# extract the value to display in the plot
			data$value <- if(is.function(paramValueVar))
				paramValueVar(data)	else	combineMultipleVars(paramValueVar)
			# in case multiple value for the same variable, concatenate them
			dataPlot <- ddply(data, c(subjectVar, paramNameVar, paramGroupVar), function(x)
				data.frame(value = toString(unique(x$value)), stringsAsFactors = FALSE)
			)
			# store in different column (in case subject is 'paramNameVar')
			dataPlot$variable <- dataPlot[, paramNameVar]
			
		}	
		
	}
		
	# create the plot
	listPlots <- dlply(dataPlot, subjectVar, function(dataSubject){	
				
		subject <- unique(dataSubject[, subjectVar])
		
		# if paramGroupVar is specified: change order levels of 'variable'
		# wrap text in multiple lines if too long
		dataSubject <- formatParamVarTextPlot(
			data = dataSubject, 
			paramVar = if(!table)	"variable", 
			paramValueVar = if(table)	paramValueVar	else	"value",
			paramValueLab = if(table)	paramValueLab,
			paramGroupVar = if(table || !is.null(paramNameVar))	paramGroupVar,
			revert = !table, 
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
			xLab = !is.null(xLab),
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
				if(!is.null(colWidth)){
					colWidth <- rep(colWidth, length.out = length(paramValueVar))
					gtable$widths <- unit(colWidth/sum(colWidth), "npc")
				}
				gg <- ggplot() + annotation_custom(gtable)
			
			}else{
			
			  aesText <- list(x = 0, y = sym("variable"), label = sym("value"))
				gg <- ggplot(data = dataSubjectPage) + 
					geom_text(
						mapping = do.call(aes, aesText),
						hjust = 0,
						size = rel(3)
					)
			
			}
			
			gg <- gg + xlim(c(0, 1)) +
				subjectProfileTheme() +
				theme(
					# panel.grid.major = element_blank doesn't work with ggplot2 >= 3.3.0
					panel.grid.major.x = element_blank(), 
					panel.grid.minor.x = element_blank(),
					panel.grid.major.y = element_blank(), 
					panel.grid.minor.y = element_blank(),
					axis.ticks = element_blank(),
					axis.text.x = element_blank(),
					axis.ticks.x = element_blank()
				)
		
			if(!is.null(title))
				gg <- gg + ggtitle(title)
		
			if(!is.null(xLab))
				gg <- gg + xlab(xLab)
		
			if(!is.null(yLab))
				gg <- gg + ylab(yLab)
			
			if(is.null(xLab) || xLab == ""){
				marDefault <- theme_bw()$plot.margin
				marNew <- margin(t = marDefault[1], r = marDefault[2], 
					b = 0, l = marDefault[4], unit = "pt")
				gg <- gg + theme(plot.margin = marNew)
			}
			
			## extract number of lines
			
			# plot content: labels y-axis and text
			if(table){
				nLinesMax <- apply(
					dataSubjectPage[, paramValueVar, drop = FALSE], 
					1, 
					function(text) max(countNLines(text))
				)
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