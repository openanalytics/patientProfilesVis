context("Compare 'subjectProfileLinePlot' with previous version")

# prepare data for plots:
dataLB <- SDTMDataPelican$LB
# sort the categories (empty values '' becomes NA)
dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH"))

test_that("subjectProfileLinePlot - basic plot", {
					
	lbLinePlots <- subjectProfileLinePlot(
		data = dataLB,
		paramNameVar = "LBTEST", 
		paramValueVar = "LBSTRESN",
		paramGroupVar = "LBSCAT",
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