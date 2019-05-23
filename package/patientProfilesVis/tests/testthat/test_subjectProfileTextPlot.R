context("Compare 'subjectProfileTextPlot' with previous version")

library(glpgUtilityFct)
data(SDTMDataPelican)
data(labelVarsSDTMPelican)

test_that("subjectProfileTextPlot - specification of parameter value only", {
			
	dmPlots <- subjectProfileTextPlot(
		data = SDTMDataPelican$DM,
		paramValueVar = c("SEX|AGE", "RACE|COUNTRY", "ARM"),
		labelVars = labelVarsSDTMPelican
	)
			
	vdiffr::expect_doppelganger(
		title = "paramValueVar", 
		fig = dmPlots[[1]][[1]],
		path = "subjectProfileTextPlot",
		verbose = TRUE
	)
	
})

test_that("subjectProfileTextPlot - specification of combination of parameter value and name", {
			
	mhPlots <- subjectProfileTextPlot(
		data = SDTMDataPelican$MH,
		paramNameVar = "MHTERM",
		paramValueVar = c("MHCAT", "MHSTDTC", "MHENDTC"),
		paramGroupVar = "MHCAT",
		title = "Medical History: status",
		labelVars = labelVarsSDTMPelican
	)
			
	vdiffr::expect_doppelganger(
		title = "paramValue/NameVar", 
		fig = mhPlots[[1]][[1]],
		path = "subjectProfileTextPlot",
		verbose = TRUE
	)
			
})

test_that("subjectProfileTextPlot - specification of a function to format parameter", {
			
	paramValueVarFct <- function(data)
		with(data, paste0(MHENRTPT, " (start = ", MHSTDTC, 
			ifelse(MHENDTC != "", paste0(", end = ", MHENDTC, ")"), ")")
		)
	)
	mhPlotsMultipleVars <- subjectProfileTextPlot(
		data = SDTMDataPelican$MH,
		paramNameVar = "MHDECOD",
		paramValueVar = paramValueVarFct,
		title = "Medical History: status with dates",
		labelVars = labelVarsSDTMPelican
	)
		
	vdiffr::expect_doppelganger(
		title = "paramValueVarAsFunction", 
		fig = mhPlotsMultipleVars[[1]][[1]],
		path = "subjectProfileTextPlot",
		verbose = TRUE
	)
			
})