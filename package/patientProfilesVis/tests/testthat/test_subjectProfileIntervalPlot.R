context("Compare 'subjectProfileIntervalPlot' with previous version")

library(glpgUtilityFct)
data(SDTMDataPelican)
data(labelVarsSDTMPelican)

# AEPTCD: preferred term code
dataPlot <- SDTMDataPelican$AE
# specify order of value in 'AESEV'
dataPlot[, "AESEV"] <- factor(dataPlot[, "AESEV"], levels = c("MILD", "MODERATE"))

test_that("subjectProfileIntervalPlot - default time limits extraction", {
			
	aePlots <- subjectProfileIntervalPlot(
		data = dataPlot,
		paramVar = "AETERM",
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		colorVar = "AESEV",
		labelVars = labelVarsSDTMPelican,
		title = "Adverse events"
	)
			
	vdiffr::expect_doppelganger(
		title = "timeLimitsDefault", 
		fig = aePlots[[1]][[1]],
		path = "subjectProfileIntervalPlot",
		verbose = TRUE
	)
			
})

test_that("subjectProfileIntervalPlot - time limits extracted from a dataset", {
			
	aePlotsTimLimFromSV <- subjectProfileIntervalPlot(
		data = dataPlot,
		paramVar = "AETERM",
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		colorVar = "AESEV",
		labelVars = labelVarsSDTMPelican,
		title = "Adverse events",
		timeLimData = SDTMDataPelican$SV,
		timeLimStartVar = "SVSTDY", 
		timeLimEndVar = "SVENDY"
	)
			
	vdiffr::expect_doppelganger(
		title = "timeLimitsFromDataSpecification", 
		fig = aePlotsTimLimFromSV[[1]][[1]],
		path = "subjectProfileIntervalPlot",
		verbose = TRUE
	)
			
})

test_that("subjectProfileIntervalPlot - multiple parameter variables", {
			
	# AEPTCD: preferred term codes
	exPlots <- subjectProfileIntervalPlot(
		data = SDTMDataPelican$EX,
		paramVar = c("EXTRT", "EXDOSE", "EXDOSU"),
		timeStartVar = "EXSTDY",
		timeEndVar = "EXENDY",
		colorVar = "EXDOSFRM",
		labelVars = labelVarsSDTMPelican,
		title = "Treatment exposure"
	)
	
	vdiffr::expect_doppelganger(
		title = "paramVarMultiple", 
		fig = exPlots[[1]][[1]],
		path = "subjectProfileIntervalPlot",
		verbose = TRUE
	)
			
})

test_that("subjectProfileIntervalPlot - time limits fixed", {
			
	timeLim <- c(-28, 53)
	cmPlotsTimeSpec <- subjectProfileIntervalPlot(
		data = SDTMDataPelican$CM,
		paramVar = c("CMINDC", "CMDECOD", "CMDOSTXT", "CMDOSU", "CMROUTE", "CMDOSFRQ"),
		timeStartVar = "CMSTDY",
		timeEndVar = "CMENDY",
		paramGroupVar = "CMINDC",
		colorVar = "CMINDC",
		labelVars = labelVarsSDTMPelican,
		title = "Concomitant medications",
		timeLim = timeLim
	)
	
	vdiffr::expect_doppelganger(
		title = "timeLimitsFixed", 
		fig = cmPlotsTimeSpec[[1]][[1]],
		path = "subjectProfileIntervalPlot",
		verbose = TRUE
	)

})