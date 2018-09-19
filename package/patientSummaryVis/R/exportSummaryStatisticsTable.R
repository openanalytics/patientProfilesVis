#' Export a summary table in \code{docx} format.
#' @param file string with path of the file where the table should be exported
#' @inheritParams formatSummaryStatisticsForExport
#' @inheritParams convertSummaryStatisticsTableToFlextable
#' @author Laure Cougnaud
#' @importFrom patientVisUtility getLabelVar
#' @import officer
#' @importFrom magrittr "%>%"
#' @export
exportSummaryStatisticsTable <- function(data, 
	byAcross = NULL, byAcrossLab = getLabelVar(byAcross, labelVars = labelVars),
	byWithin = NULL, 
	labelVars = NULL, 
	file = NULL, landscape = TRUE, 
	title = "Table: Descriptive statistics",
	subtitle = NULL){

	margin <- 1

	## format table
	summaryTableLong <- formatSummaryStatisticsForExport(
		data = data,
		byAcross = byAcross, byAcrossLab = byAcrossLab,
		byWithin = byWithin
	)
	
	convertSummaryStatisticsTableToFlextableCustom <- function(...){
		convertSummaryStatisticsTableToFlextable(...,
			landscape = landscape, margin = margin,
			title = title,
			subtitle = subtitle,
			byAcross = byAcrossLab
		)	
	}
	
	# create flextable only with header to extract dimensions header
	summaryTableFt <- convertSummaryStatisticsTableToFlextableCustom(data = summaryTableLong)	
	
	##  split table between different sections
	# Note: there is an automated implementation in the officer package
	# to split table across pages
	
	# extract maximum width page and width of header and each row (in inches)
	heightPage <- getDimPage(type = "height", landscape = landscape, margin = margin)
	headerHeight <- sum(summaryTableFt$header$rowheights)
	bodyHeights <- summaryTableFt$body$rowheights
	
	# extract rows where the table should be split
	statsVar <- c("N", "Mean", "SD", "SE", "Median", "Min", "Max")
	idxEndSection <- which(summaryTableLong$Statistic == statsVar[length(statsVar)]) # cut by section
	bodyHeightsCumsum <- cumsum(bodyHeights)
	heightPageForTable <- heightPage - headerHeight
	breaks <- c(seq(from = 0, to = max(bodyHeightsCumsum), by = heightPageForTable), Inf)
	idxSectionByPage <- findInterval(bodyHeightsCumsum[idxEndSection], breaks)
	
	# build the list of tables
	summaryTableFtList <- lapply(unique(idxSectionByPage), function(i){
		iRowStart <- ifelse(i == 1, 1, max(idxEndSection[which(idxSectionByPage == (i - 1))]) + 1)
		iRowEnd <-  max(idxEndSection[which(idxSectionByPage == i)])
		table <- summaryTableLong[seq.int(from = iRowStart, to = iRowEnd), ]
		convertSummaryStatisticsTableToFlextableCustom(data = table)
	})

	# include the tables in a Word document
	if(!is.null(file)){	
		
		doc <- read_docx()
		if(landscape)	doc <- doc %>% body_end_section_landscape()
		
		for(i in seq_along(summaryTableFtList)){
			doc <- doc %>% body_add_flextable(value = summaryTableFtList[[i]])
			if(i != length(summaryTableFtList))
				doc <- doc %>% body_add_break()
		}
		if(landscape){
			doc <- doc %>%
				# a paragraph needs to be included after the table otherwise the layout is not landscape
				body_add_par(value = "", style = "Normal") %>%
				body_end_section_landscape()
		}
		print(doc, target = file)
		
	}
	
	return(summaryTableFt)
	
}

#' Format summary statistics table for export
#' @inheritParams subjectProfileSummaryPlot
#' @param byWithin string with variable of \code{data} used for grouping in column.
#' @param byAcross character vector with variable(s) of \code{data}
#' used for grouping in rows.
#' @param byAcrossLab label for each variable of \code{byAcross}.
#' @inheritParams subjectProfileSummaryPlot
#' @return data reformatted in long format
#' @author Laure Cougnaud
#' @importFrom patientVisUtility getLabelVar
#' @importFrom reshape2 melt dcast
#' @importFrom stats as.formula
formatSummaryStatisticsForExport <- function(data,
	byAcross = NULL, 
	byAcrossLab = getLabelVar(byAcross, labelVars = labelVars),
	byWithin = NULL,
	labelVars = NULL
	){
	
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

#' Convert summary statistics table to flextable
#' @param data summary statistics table in long format,
#' as returned by \code{\link{formatSummaryStatisticsForExport}}
#' @param title string with title for the table.
#' Set to NULL if no title should be included.
#' @param subtitle string with subtitle for the table,
#' empty (NULL) by default.
#' @inheritParams getDimPage
#' @inheritParams formatSummaryStatisticsForExport
#' @return \code{\link[flextable]{flextable}} object
#' @import flextable
#' @importFrom officer fp_border
#' @importFrom stats setNames
#' @author Laure Cougnaud
convertSummaryStatisticsTableToFlextable <- function(data, 
	landscape = TRUE, margin = 1,
	title = "Table: Descriptive statistics",
	byAcross = NULL,
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
		ft <- merge_v(ft, j = getNewCol(byAcross)) # merge rows
	
	# set correct alignments
	colsAlignLeft <- getNewCol(c("Statistic", byAcross))
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
	widthPage <- getDimPage(type = "width", landscape = landscape, margin = margin)
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

#' Return page dimension of interest
#' @param type string dimension of interest, 'width' or 'height'
#' @param landscape logical, if TRUE (by defaut) the table is presented in landscape
#' format
#' @param margin margin in the document in inches
#' @return integer with dimension of interest
#' @author Laure Cougnaud
getDimPage <- function(type = c("width", "height"), landscape = TRUE, margin = 1){
	# landscape: 29.7 * 21 cm ~ 11 * 8 inches ~ 2138.4 * 1512 ptx
	type <- match.arg(type)
	a4Dim <- c(21, 29.7)/2.54 # inches
	typeDim <- switch(type,
		'width' = ifelse(landscape, a4Dim[2], a4Dim[1]),
		'height' = ifelse(landscape, a4Dim[1], a4Dim[2])
	)
	dimPage <- typeDim - 2 * margin
	return(dimPage)
}