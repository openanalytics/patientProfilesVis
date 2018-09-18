#' Export a summary table in \code{docx} format.
#' @param byAcross character vector with variable(s) of \code{data}
#' used for grouping in rows.
#' @param byAcrossLab label for each variable of \code{byAcross}.
#' @param byWithin string with variable of \code{data} used for grouping in column.
#' @param file string with path of the file where the table should be exported
#' @inheritParams subjectProfileSummaryPlot
#' @return \code{\link[flextable]{flextable}} object
#' @author Laure Cougnaud
#' @importFrom reshape2 melt dcast
#' @importFrom stats as.formula
#' @importFrom patientVisUtility getLabelVar
#' @importFrom officer read_docx
#' @import flextable
#' @importFrom magrittr "%>%"
#' @export
exportSummaryStatisticsTable <- function(data, 
	byAcross = NULL, byAcrossLab = getLabelVar(byAcross, labelVars = labelVars),
	byWithin = NULL, 
	labelVars = NULL, 
	file = NULL, landscape = TRUE){

	# convert from wide to long format
	statsVar <- c("N", "Mean", "SD", "SE", "Median", "Min", "Max")
	dataLong <- melt(data, 
		id.vars = c(byAcross, byWithin),
		measure.vars = statsVar,
		value.name = "StatisticValue",
		variable.name = "Statistic"
	)
	
	# format statistic value
	dataLong$StatisticValue <- formatC(dataLong$StatisticValue)
	
	# put elements in 'byWithin' in different columns (long -> wide format)
	if(!is.null(byWithin)){
		varsRows <- setdiff(colnames(dataLong), c("StatisticValue", byWithin))
		formulaWithin <- as.formula(paste(
			paste(byAcross, collapse = " + "), 
			"+ Statistic ~", 
			paste(byWithin, collapse = " + ")
		))
		dataLong <- dcast(dataLong, formula = formulaWithin, value.var = "StatisticValue")
	}
	
	# re-label columns
	colnames(dataLong)[match(byAcross, colnames(dataLong))] <- byAcrossLab
		
	## print to docx
	
	# re-label the columns to avoid the error: 'invalid col_keys, flextable support only syntactic names'
	colsData <- colnames(dataLong)
	names(colsData) <- paste0("col", seq_len(ncol(dataLong)))
	colnames(dataLong) <- names(colsData)
	
	ft <- flextable(dataLong)
	
	if(!is.null(byAcross)){
		
		byAcrossNew <- names(colsData)[match(byAcrossLab, colsData)]
		# merge columns
		ft <- merge_v(ft, j = byAcrossNew)
		
	}
	
	# set correct alignments
	colsAlignLeft <- names(colsData)[match(c("Statistic", byAcrossLab), colsData)]
	colsAlignCenter <- setdiff(names(colsData), colsAlignLeft)
	ft <- align(ft, j = colsAlignLeft, align = "left", part = "all")
	ft <- align(ft, j = colsAlignCenter, align = "center", part = "all")
	
	# set to correct headers
	ft <- do.call(set_header_labels, c(list(x = ft), as.list(colsData)))
	
	# set fontsize
	ft <- fontsize(ft, size = 8, part = "all")
	
	if(!is.null(file)){
		doc <- read_docx()
		doc <- doc %>%
#			body_add_par(value = '\r\n') %>%
			body_add_flextable(value = ft) #%>%
#			body_end_section_landscape()
		print(doc, target = file)
	}
	
	return(ft)
	
}