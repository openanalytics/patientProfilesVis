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
		fig = aePlots[["study-4903-01"]][[1]],
		path = "subjectProfileIntervalPlot",
		verbose = TRUE
	)
			
})

test_that("subjectProfileIntervalPlot - default time limits extraction - all missing values", {
			
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
		title = "timeLimitsDefaults-Missing", 
		fig = aePlots[["study-4902-01"]][[1]],
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
	dataCM <- SDTMDataPelican$CM
	cmPlotsTimeSpec <- subjectProfileIntervalPlot(
		data = dataCM,
		paramVar = "CMDECOD",
		timeStartVar = "CMSTDY",
		timeEndVar = "CMENDY",
		paramGroupVar = "CMINDC",
		colorVar = "CMINDC",
		labelVars = labelVarsSDTMPelican,
		title = "Concomitant medications",
		timeLim = timeLim
	)
	
	# consider subject with start and end date and with max non missing end date
	subjectWithStartEndTime <- by(dataCM, dataCM$USUBJID, function(x){
		if(any(!is.na(x$CMSTDY) & !is.na(x$CMENDY)))	sum(!is.na(x$CMENDY))
	})
	subject <- names(which.max(unlist(subjectWithStartEndTime)))
	
	vdiffr::expect_doppelganger(
		title = "timeLimitsFixed", 
		fig = cmPlotsTimeSpec[["study-4907-04"]][[1]],
		path = "subjectProfileIntervalPlot",
		verbose = TRUE
	)

})