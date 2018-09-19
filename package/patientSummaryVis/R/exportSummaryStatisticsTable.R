#' Export a summary table in \code{docx} format.
#' @param byAcross character vector with variable(s) of \code{data}
#' used for grouping in rows.

#' @param file string with path of the file where the table should be exported
#' @author Laure Cougnaud

#' @importFrom patientVisUtility getLabelVar
#' @importFrom officer read_docx
#' @import flextable
#' @importFrom magrittr "%>%"
#' @export
exportSummaryStatisticsTable <- function(data, 
	byAcross = NULL, byAcrossLab = getLabelVar(byAcross, labelVars = labelVars),
	byWithin = NULL, 
	labelVars = NULL, 
	file = NULL, landscape = TRUE, margin = 1,
	title = "Table: Descriptive statistics",
	subtitle = NULL){

	## format table
	summaryTableLong <- formatSummaryStatisticsForExport(
		data = data,
		byAcross = byAcross, byWithin = byWithin
	)
	
	# create flextable only with header to extract dimensions header
	summaryTableFt <- convertSummaryStatisticsToFlextable(
		data = summaryTableLong, 
		landscape = landscape, margin = margin,
		title = title,
		subtitle = subtitle
	)	
	
	# split table to fit in the page
	widthPage <- getWidthPage(landscape = landscape, margin = margin)
	headerHeight <- sum(summaryTableFt$header$rowheights)
	bodyHeights <- summaryTableFt$body$rowheights
	
	bodyHeightsCumsum <- cumsum(bodyHeights)
	breaks <- c(seq(from = 0, to = max(bodyHeightsCumsum), by = widthPage - headerHeight), Inf)
	idxRowsByPage <- cut(bodyHeightsCumsum, breaks)
	tableList <- by(summaryTableLong, idxRowsByPage, function(table)
		convertSummaryStatisticsToFlextable(
			data = table, 
			landscape = landscape, margin = margin,
			title = title,
			subtitle = subtitle
		)
	)

	## TODO: print to docx	
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

#' Format summary statistics table for export
#' @inheritParams subjectProfileSummaryPlot
#' @param byAcrossLab label for each variable of \code{byAcross}.
#' @param byWithin string with variable of \code{data} used for grouping in column.
#' @return data reformatted in long format
#' @author Laure Cougnaud
#' @importFrom reshape2 melt dcast
#' @importFrom stats as.formula
formatSummaryStatisticsForExport <- function(data,
	byAcross = NULL, byWithin = NULL){
	
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
	
	return(dataLong)
	
}

#' 
#' @param data summary statistics table in long format,
#' as returned by \code{\link{formatSummaryStatisticsForExport}}
#' @param landscape logical, if TRUE (by defaut) the table is presented in landscape
#' format
#' @param margin margin in the document in inches
#' @param title string with title for the table.
#' Set to NULL if no title should be included.
#' @param subtitle string with subtitle for the table,
#' empty (NULL) by default.
#' @return \code{\link[flextable]{flextable}} object
#' @author Laure Cougnaud
convertSummaryStatisticsToFlextable <- function(data, 
	landscape = TRUE, margin = 1,
	title = "Table: Descriptive statistics",
	subtitle = NULL
	){
	
	# re-label the columns to avoid the error: 'invalid col_keys, flextable support only syntactic names'
	colsData <- colnames(data)
	names(colsData) <- paste0("col", seq_len(ncol(data)))
	colnames(data) <- names(colsData)
	
	getNewCol <- function(initCol)
		names(colsData)[match(initCol, colsData)]
	
	ft <- flextable(data)
	
	if(!is.null(byAcross))
		ft <- merge_v(ft, j = getNewCol(byAcrossLab)) # merge rows
	
	# set correct alignments
	colsAlignLeft <- getNewCol(c("Statistic", byAcrossLab))
	colsAlignCenter <- setdiff(names(colsData), colsAlignLeft)
	ft <- align(ft, j = colsAlignLeft, align = "left", part = "all")
	ft <- align(ft, j = colsAlignCenter, align = "center", part = "all")
	
	# add title/subtitle
	setHeader <- function(header){
		headerList <- as.list(setNames(rep(header, length(colsData)), names(colsData)))
		ft <- do.call(add_header, c(list(x = ft, top = TRUE), headerList))
		ft <- merge_h(x = ft, part = "header")
		return(ft)
	}
	if(!is.null(subtitle))	ft <- setHeader(subtitle)
	if(!is.null(title))	ft <- setHeader(header = title)
	
	# set to correct headers
	ft <- do.call(set_header_labels, c(list(x = ft), as.list(colsData)))
	
	# set fontsize
	ft <- fontsize(ft, size = 8, part = "all")
	
	# adjust to fit in document:
	widthPage <- getWidthPage(landscape = landscape, margin = margin)
	varFixed <- getNewCol(setdiff(c("Statistic", "Total"), colnames(data)))
	varFixedWidth <- 0.5
	ft <- width(ft, j = varFixed, width = 0.5)
	varsOther <- setdiff(names(colsData), varFixed)
	varsOtherWidth <- (widthPage - length(varFixed) * varFixedWidth)/length(varsOther)
	ft <- width(ft, j = varsOther, width = varsOtherWidth)
	
	# borders
	ft <- border_remove(ft) %>%
		hline_top(border = fp_border(), part = "body") %>% 
		hline_top(border = fp_border(), part = "header") %>%
		hline_bottom(border = fp_border(), part = "body")
	
	return(ft)
	
}

getWidthPage <- function(landscape = TRUE, margin = 1){
	# landscape: 29.7 * 21 cm ~ 11 * 8 inches ~ 2138.4 * 1512 ptx
	widthPage <- ifelse(landscape, 11, 8) - 2 * margin
	return(widthPage)
}