context("Compare 'subjectProfileEventPlot' with previous version")

# prepare data for plots:
dataLB <- SDTMDataPelican$LB
# sort the categories (empty values '' becomes NA)
dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH"))

test_that("subjectProfileEventPlot - basic plot", {
					
	# create plot
	lbPlots <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = c("LBSCAT", "LBTEST"),
		paramGroupVar = "LBSCAT",
		timeVar = "LBDY",
		labelVars = labelVarsSDTMPelican,
		title = "Laboratory test measurements"
	)
			
	vdiffr::expect_doppelganger(
		title = "basic", 
		fig = lbPlots[[1]][[1]],
		path = "subjectProfileEventPlot",
		verbose = TRUE
	)
	
})

test_that("subjectProfileEventPlot - coloring variable", {
			
	lbPlotsColor <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		paramGroupVar = "LBSCAT",
		timeVar = "LBDY",
		colorVar = "LBNRIND",
		labelVars = labelVarsSDTMPelican,
		shapeVar = "LBNRIND",
		shapePalette = c('LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24, 'NA' = 3),
		title = "Laboratory test measurements: reference range indicator"
	)
	
	vdiffr::expect_doppelganger(
		title = "colorVariable", 
		fig = lbPlotsColor[[1]][[1]],
		path = "subjectProfileEventPlot",
		verbose = TRUE
	)
	
})

test_that("subjectProfileEventPlot - subset variable", {
	
	lbPlotsSubset <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		subsetVar = "LBCAT", subsetValue = "HEMATOLOGY",
		timeVar = "LBDY",
		colorVar = "LBNRIND",
		labelVars = labelVarsSDTMPelican,
		shapeVar = "LBNRIND",
		shapePalette = c('LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24, 'NA' = 3),
		title = "Hematology test measurements: reference range indicator"
	)
	
	vdiffr::expect_doppelganger(
		title = "subsetVariable", 
		fig = lbPlotsSubset[[1]][[1]],
		path = "subjectProfileEventPlot",
		verbose = TRUE
	)
	
})