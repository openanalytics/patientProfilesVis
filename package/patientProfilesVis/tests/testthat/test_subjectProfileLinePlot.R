context("Visualize subject profile as a line")

library(ggplot2)

test_that("subject variable is specified", {
			
	data <- data.frame(
		TEST = c("A", "B", "C"),
		AVAL = rnorm(3),
		DY = c(1, 2, 3),
		SUBJID = factor(c("a", "b", "a"), levels = c("b", "a"))
	)
			
	plots <- subjectProfileLinePlot(
		data = data, 
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
		subjectVar = "SUBJID"
	)
			
	# plots are sorted based on factor levels:
	expect_named(plots, levels(data$SUBJID))
			
})

test_that("error if subject variable is not present in the data", {
			
	data <- data.frame(
		TEST = c("A", "B", "C"),
		AVAL = rnorm(3),
		DY = c(1, 2, 3)
	)
	expect_error(
		subjectProfileLinePlot(
			data = data, 
			timeVar = "DY",
			paramNameVar = "TEST",
			paramValueVar = "AVAL",
		),
		"Variable.*not available in the data"
	)
			
})

test_that("correct data are displayed for each subject", {
			
	data <- data.frame(
		TEST = factor(rep(c("A", "B"), each = 5), levels = c("B", "A")),
		AVAL = rnorm(10),
		DY = sample.int(10),
		USUBJID = sample(c("a", "b"), 10, replace = TRUE)
	)
			
	plots <- subjectProfileLinePlot(
		data = data, 
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
	)
			
	# test data is retained
	for(subjID in unique(data$USUBJID)){
				
		# check that the sublist is a list of ggplot object
		expect_type(plots[[!!subjID]], "list")
		expect_length(plots[[!!subjID]], 1)
		expect_s3_class(plots[[!!subjID]][[1]], c("subjectProfileLinePlot", "ggplot"))
				
		expect_equal(
			object = {		
						
				gg <- plots[[!!subjID]][[1]]
				
				# extract data behind the text
				isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
				ggDataPoint <- layer_data(gg, which(isGeomPoint))
				ggDataPoint$PANEL <- as.character(ggDataPoint$PANEL)
				ggDataPoint[, c("PANEL", "x", "y")]
							
			},
			expected = {
				dataReference <- subset(data, USUBJID == !!subjID)
				dataReference$PANEL <- as.character(as.numeric(dataReference$TEST))
				test <- setNames(
					dataReference[, c("PANEL", "DY", "AVAL")], 
					c("PANEL", "x", "y")
				)
			},
			check.attributes = FALSE # (rownames differ)
		)		
				
	}
			
})

test_that("multiple parameter variables are correctly combined and ordered", {
			
	# example where variables are specified as factor
	# in this case variables are ordered based on factor levels
	dataFactor <- data.frame(
		CAT = factor(c("A", "A", "A", "B"), levels = c("B", "A")),
		TEST = factor(c("a1", "a2", "a3", "b1"), levels = c("a2", "a3", "a1", "b1")),
		DY = sample.int(4),
		USUBJID = "1",
		AVAL = rnorm(4)
	)
			
	# example with character vector
	# in this case standard R ordering (alphabetical) is used
	dataCharacter <- dataFactor
	dataCharacter[, c("CAT", "TEST")] <- lapply(dataCharacter[, c("CAT", "TEST")], as.character)
			
	dataList <- list(factor = dataFactor, character = dataCharacter)
			
	for(type in names(dataList)){
				
		expect_equal(
				
			object = {
					
				plots <- subjectProfileLinePlot(
					data = dataList[[!!type]],
					paramNameVar = c("CAT", "TEST"),
					paramValueVar = "AVAL",
					timeVar = "DY"
				)
							
				gg <- plots[[1]][[1]]
							
				# extract data behind the text
				isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
				ggDataPoint <- layer_data(gg, which(isGeomPoint))
				ggDataPoint$PANEL <- as.character(ggDataPoint$PANEL)
				ggDataPoint <- ggDataPoint[order(ggDataPoint$PANEL), ]
				ggDataPoint[, c("PANEL", "x", "y")]
				
			}, expected = {
							
				data <- dataList[[!!type]]
				data <- data[with(data, order(CAT, TEST)), ]
				data$PANEL <- as.character(seq.int(nrow(data)))
				
				data[, c("PANEL", "DY", "AVAL")]
							
			},
			check.attributes = FALSE
		)
		
	}
			
})


#
#library(glpgUtilityFct)
#library(ggplot2)
#library(plyr)
#data(SDTMDataPelican)
#data(labelVarsSDTMPelican)
#
## prepare data for plots:
#dataLB <- SDTMDataPelican$LB
## sort the categories (empty values '' becomes NA)
#dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH"))
#
#dataLB$TEST <- with(dataLB, reorder(TEST, LBTESTCD, unique))		
#
#dataPlot <- subset(dataLB, USUBJID == "study-4902-02")
#dataPlot <- subset(dataPlot, TEST %in% unique(dataPlot[, "TEST"])[1:5])
#rownames(dataPlot) <- NULL
#
#test_that("subjectProfileLinePlot - basic plot", {
#					
#	title <- "Laboratory test measurements: actual value"
#	expect_error(
#		lbLinePlots <- subjectProfileLinePlot(
#			data = dataPlot,
#			paramNameVar = "TEST", 
#			paramValueVar = "AVAL",
#			paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
#			timeVar = "DY",
#			title = title,
#			labelVars = labelVarsSDTMPelican
#		),
#		NA
#	)
#	
#	gg <- lbLinePlots[[1]][[1]]
#	ggData <- gg$data[, colnames(dataPlot)]
#	expect_equal(ggData, dataPlot)
#
#	expect_identical(gg$labels$title, title)
#	
#})
#
#test_that("subjectProfileLinePlot - color/shape variable and palette specification", {
#			
#	expect_error(
#		lbLinePlotsColorShape <- subjectProfileLinePlot(
#			data = dataPlot,
#			paramNameVar = "TEST", 
#			paramValueVar = "AVAL",
#			colorVar = "LBNRIND",
#			shapeVar = "LBNRIND",
#			shapePalette = c('LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24),
#			paramGroupVar = "LBSCAT",
#			paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
#			timeVar = "DY",
#			title = "Laboratory test measurements: actual value",
#			labelVars = labelVarsSDTMPelican
#		),
#		NA
#	)
#	
#	gg <- lbLinePlotsColorShape[[1]][[1]]
#	ggData <- gg$data[, colnames(dataPlot)]
#	expect_equal(colwise(as.character)(ggData), colwise(as.character)(dataPlot))
#	
#})
#
#test_that("subjectProfileLinePlot - yLimFrom - value", {
#			
#	expect_error(
#		lbLinePlots <- subjectProfileLinePlot(
#			data = dataPlot,
#			paramNameVar = "TEST", 
#			paramValueVar = "AVAL",
#			paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
#			timeVar = "DY",
#			title = "Laboratory test measurements: actual value",
#			labelVars = labelVarsSDTMPelican,
#			yLimFrom = "value"
#		),
#		NA
#	)
#	
#	# output data from ggplot2
##	gg <- lbLinePlots[[1]][[1]]
##	ggBuildData <- ggplot_build(gg)$data
##	dataPlot <- ggBuildData[[which.max(sapply(ggBuildData, nrow))]]
##	dataPlot <- dataPlot[, c("PANEL", "x", "y")]
#			
#})
#
#test_that("subjectProfileLinePlot - correct order of paramGroupVar", {
#		
#	# create the plot:
#	dataPatient <- subset(dataLB, USUBJID == "study-4902-02")
#	paramGroupVar <- c("LBCAT", "LBSCAT")
#	xVar <- "DY"
#	yVar <- "AVAL"
#	paramVar <- "TEST"
#	lbLinePlots <- subjectProfileLinePlot(
#		data = dataPatient,
#		paramNameVar = paramVar, 
#		paramValueVar = yVar,
#		paramGroupVar = paramGroupVar,
#		timeVar = xVar,
#		title = "Laboratory test measurements: actual value",
#		labelVars = labelVarsSDTMPelican
#	)
#	
#	# output data from ggplot2
#	dataPlotsList <- lapply(seq_along(lbLinePlots[[1]]), function(i){
#		gg <- lbLinePlots[[1]][[i]]
#		ggBuildData <- ggplot_build(gg)$data
#		dataPlot <- ggBuildData[[which.max(sapply(ggBuildData, nrow))]]
#		dataPlot <- dataPlot[, c("PANEL", "x", "y")]
#		dataPlot$PANEL <- as.numeric(dataPlot$PANEL) + i * 1000
#		dataPlot
#	})
#	dataPlots <- do.call(rbind, dataPlotsList)
#	dataPlotsOrd <- dataPlots[do.call(order, dataPlots), ]
#	rownames(dataPlotsOrd) <- NULL
#	
#	# input data for plot:
#	dataInputPlot <- dataPatient[, c(paramGroupVar, paramVar, xVar, yVar)]
#	idxComplete <- which(!is.na(dataInputPlot[, xVar]) & !is.na(dataInputPlot[, yVar]))
#	dataInputPlot <- dataInputPlot[idxComplete, ]
#	idxOrderParam <- do.call(order, dataInputPlot[, c(paramGroupVar, paramVar)])	
#	paramOrder <- unique(dataInputPlot[idxOrderParam, paramVar])
#	dataInputPlot$PANEL <- match(dataInputPlot[, paramVar], paramOrder)
#	dataInputPlotOrd <- dataInputPlot[do.call(order, dataInputPlot[, c("PANEL", xVar, yVar)]), ]
#	
#	# compare the two
#	expect_equivalent(
#		object = dataPlotsOrd[, c("x", "y")],
#		expected = dataInputPlotOrd[, c(xVar, yVar)]
#	)
#	
#})