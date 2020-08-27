context("Compare 'subjectProfileLinePlot' with previous version")

library(glpgUtilityFct)
library(ggplot2)
library(plyr)
data(SDTMDataPelican)
data(labelVarsSDTMPelican)

# prepare data for plots:
dataLB <- SDTMDataPelican$LB
# sort the categories (empty values '' becomes NA)
dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH"))

dataLB$LBTEST <- with(dataLB, reorder(LBTEST, LBTESTCD, unique))		

dataPlot <- subset(dataLB, USUBJID == "study-4902-02")
dataPlot <- subset(dataPlot, LBTEST %in% unique(dataPlot[, "LBTEST"])[1:5])
rownames(dataPlot) <- NULL

test_that("subjectProfileLinePlot - basic plot", {
					
	title <- "Laboratory test measurements: actual value"
	expect_error(
		lbLinePlots <- subjectProfileLinePlot(
			data = dataPlot,
			paramNameVar = "LBTEST", 
			paramValueVar = "LBSTRESN",
			paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
			timeVar = "LBDY",
			title = title,
			labelVars = labelVarsSDTMPelican
		),
		NA
	)
	
	gg <- lbLinePlots[[1]][[1]]
	ggData <- gg$data[, colnames(dataPlot)]
	expect_equal(ggData, dataPlot)

	expect_identical(gg$labels$title, title)
	
})

test_that("subjectProfileLinePlot - color/shape variable and palette specification", {
			
	expect_error(
		lbLinePlotsColorShape <- subjectProfileLinePlot(
			data = dataPlot,
			paramNameVar = "LBTEST", 
			paramValueVar = "LBSTRESN",
			colorVar = "LBNRIND",
			shapeVar = "LBNRIND",
			shapePalette = c('LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24),
			paramGroupVar = "LBSCAT",
			paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
			timeVar = "LBDY",
			title = "Laboratory test measurements: actual value",
			labelVars = labelVarsSDTMPelican
		),
		NA
	)
	
	gg <- lbLinePlotsColorShape[[1]][[1]]
	ggData <- gg$data[, colnames(dataPlot)]
	expect_equal(colwise(as.character)(ggData), colwise(as.character)(dataPlot))
	
})

test_that("subjectProfileLinePlot - yLimFrom - value", {
			
	expect_error(
		lbLinePlots <- subjectProfileLinePlot(
			data = dataPlot,
			paramNameVar = "LBTEST", 
			paramValueVar = "LBSTRESN",
			paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
			timeVar = "LBDY",
			title = "Laboratory test measurements: actual value",
			labelVars = labelVarsSDTMPelican,
			yLimFrom = "value"
		),
		NA
	)
	
	# output data from ggplot2
#	gg <- lbLinePlots[[1]][[1]]
#	ggBuildData <- ggplot_build(gg)$data
#	dataPlot <- ggBuildData[[which.max(sapply(ggBuildData, nrow))]]
#	dataPlot <- dataPlot[, c("PANEL", "x", "y")]
			
})

test_that("subjectProfileLinePlot - correct order of paramGroupVar", {
		
	# create the plot:
	dataPatient <- subset(dataLB, USUBJID == "study-4902-02")
	paramGroupVar <- c("LBCAT", "LBSCAT")
	xVar <- "LBDY"
	yVar <- "LBSTRESN"
	paramVar <- "LBTEST"
	lbLinePlots <- subjectProfileLinePlot(
		data = dataPatient,
		paramNameVar = paramVar, 
		paramValueVar = yVar,
		paramGroupVar = paramGroupVar,
		timeVar = xVar,
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTMPelican
	)
	
	# output data from ggplot2
	dataPlotsList <- lapply(seq_along(lbLinePlots[[1]]), function(i){
		gg <- lbLinePlots[[1]][[i]]
		ggBuildData <- ggplot_build(gg)$data
		dataPlot <- ggBuildData[[which.max(sapply(ggBuildData, nrow))]]
		dataPlot <- dataPlot[, c("PANEL", "x", "y")]
		dataPlot$PANEL <- as.numeric(dataPlot$PANEL) + i * 1000
		dataPlot
	})
	dataPlots <- do.call(rbind, dataPlotsList)
	dataPlotsOrd <- dataPlots[do.call(order, dataPlots), ]
	rownames(dataPlotsOrd) <- NULL
	
	# input data for plot:
	dataInputPlot <- dataPatient[, c(paramGroupVar, paramVar, xVar, yVar)]
	idxComplete <- which(!is.na(dataInputPlot[, xVar]) & !is.na(dataInputPlot[, yVar]))
	dataInputPlot <- dataInputPlot[idxComplete, ]
	idxOrderParam <- do.call(order, dataInputPlot[, c(paramGroupVar, paramVar)])	
	paramOrder <- unique(dataInputPlot[idxOrderParam, paramVar])
	dataInputPlot$PANEL <- match(dataInputPlot[, paramVar], paramOrder)
	dataInputPlotOrd <- dataInputPlot[do.call(order, dataInputPlot[, c("PANEL", xVar, yVar)]), ]
	
	# compare the two
	expect_equivalent(
		object = dataPlotsOrd[, c("x", "y")],
		expected = dataInputPlotOrd[, c(xVar, yVar)]
	)
	
})