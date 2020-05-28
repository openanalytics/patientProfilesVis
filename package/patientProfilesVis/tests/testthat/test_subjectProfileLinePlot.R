context("Compare 'subjectProfileLinePlot' with previous version")

library(glpgUtilityFct)
data(SDTMDataPelican)
data(labelVarsSDTMPelican)

# prepare data for plots:
dataLB <- SDTMDataPelican$LB
# sort the categories (empty values '' becomes NA)
dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH"))

test_that("subjectProfileLinePlot - basic plot", {
					
	lbLinePlots <- subjectProfileLinePlot(
		data = subset(dataLB, USUBJID == "study-4902-02"),
		paramNameVar = "LBTEST", 
		paramValueVar = "LBSTRESN",
		paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
		timeVar = "LBDY",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTMPelican
	)
			
	vdiffr::expect_doppelganger(
		title = "basic", 
		fig = lbLinePlots[[1]][[1]],
		path = "subjectProfileLinePlot",
		verbose = TRUE
	)
	
})

test_that("subjectProfileLinePlot - color/shape variable and palette specification", {
			
	lbLinePlotsColorShape <- subjectProfileLinePlot(
		data = dataLB,
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
	)
	
	vdiffr::expect_doppelganger(
		title = "colorShapeVariablePalette", 
		fig = lbLinePlotsColorShape[[1]][[1]],
		path = "subjectProfileLinePlot",
		verbose = TRUE
	)
	
})

test_that("subjectProfileLinePlot - yLimFrom - value", {
			
	lbLinePlots <- subjectProfileLinePlot(
		data = subset(dataLB, USUBJID == "study-4902-02"),
		paramNameVar = "LBTEST", 
		paramValueVar = "LBSTRESN",
		paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
		timeVar = "LBDY",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTMPelican,
		yLimFrom = "value"
	)
			
	vdiffr::expect_doppelganger(
		title = "yLimFrom-value", 
		fig = lbLinePlots[[1]][[1]],
		path = "subjectProfileLinePlot",
		verbose = TRUE
	)
			
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