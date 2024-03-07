context("Set time limits")

test_that("Plots are correctly aligned across modules and subjects", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = c("1", "2")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
			
	timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
		listPlots = listPlots,
		timeAlign = "all", timeAlignPerSubject = "none"
	)
			
	expect_is(timeLim, "list")
	expect_named(timeLim, c("A", "B"))
	expect_equal(timeLim$A, c(1, 4))
	expect_equal(timeLim$B, c(1, 4))
	
})

test_that("Time limits are not set in case no alignment is requested", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = c("1", "2")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
			
	timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
		listPlots = listPlots,
		timeAlign = "none"
	)
			
	expect_null(timeLim)
		
})

test_that("A set of plots are correctly time-aligned", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = c("1", "2")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	dataC <- data.frame(
		TEST = "1",
		DY = c(5, 6),
		USUBJID = c("1", "2")
	)
	listPlotsC <- subjectProfileEventPlot(
		data = dataC,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB, C = listPlotsC)	
			
	timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
		listPlots = listPlots,
		timeAlign = c("A", "B")
	)
			
	expect_is(timeLim, "list")
	expect_named(timeLim, c("A", "B"))
	expect_equal(timeLim$A, c(1, 4))
	expect_equal(timeLim$B, c(1, 4))
			
})

test_that("All plots are aligned per subject if requested", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = c("1", "2")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
			
	timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
		listPlots = listPlots,
		timeAlign = "all",
		timeAlignPerSubject = "all"
	)
			
	expect_is(timeLim, "list")
	expect_named(timeLim, c("A", "B"))
	
	for(mod in c("A", "B")){
		expect_is(timeLim[[mod]], "list")
		expect_equal(timeLim[[mod]]$`1`, c(1, 3))
		expect_equal(timeLim[[mod]]$`2`, c(2, 4))
	}
			
})

test_that("Time limits are correctly aligned across subjects, while aligning a set of modules per subject", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = c("1", "2")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	dataC <- data.frame(
		TEST = "1",
		DY = c(5, 6),
		USUBJID = c("1", "2")
	)
	listPlotsC <- subjectProfileEventPlot(
		data = dataC,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB, C = listPlotsC)	
			
	timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
		listPlots = listPlots,
		timeAlign = "all",
		timeAlignPerSubject = c("A", "B")
	)
			
	expect_is(timeLim, "list")
	expect_named(timeLim, c("A", "B", "C"))
	
	# time limits per subject for modules A and B
	expect_equal(timeLim$`A`$`1`, c(1, 5))
	expect_equal(timeLim$`B`$`1`, c(1, 5))
	expect_equal(timeLim$`A`$`2`, c(2, 6))
	expect_equal(timeLim$`B`$`2`, c(2, 6))
	
	# time limits across subjects for module C
	expect_equal(timeLim$`C`, c(1, 6))
			
})

test_that("Time limits are correctly aligned across a subset of the plots and a subset of subjects", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = c("1", "2")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	dataC <- data.frame(
		TEST = "1",
		DY = c(5, 6),
		USUBJID = c("1", "2")
	)
	listPlotsC <- subjectProfileEventPlot(
		data = dataC,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB, C = listPlotsC)	
			
	timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
		listPlots = listPlots,
		timeAlign = c("A", "B"),
		timeAlignPerSubject = "B"
	)
			
	expect_is(timeLim, "list")
	expect_named(timeLim, c("A", "B"))
			
	# plots aligned across all subjects/modules
	expect_equal(timeLim$`A`, c(1, 4))
	
	# plots aligned per subject
	expect_equal(timeLim$`B`$`1`, c(1, 3))
	expect_equal(timeLim$`B`$`2`, c(2, 4))
			
})

test_that("An error is generated in case the list of plots to align is not named", {
			
	data <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)
	
	expect_error(
		timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
			listPlots = list(listPlots),
			timeAlign = "all"
		),
		"'listPlots' should be named"
	)
	
	
})

test_that("An error is generated in case the list of plots to align does not contain unique names", {
			
	data <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)
			
	expect_error(
		timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
			listPlots = list(A = listPlots, A = listPlots),
			timeAlign = "all"
		),
		"unique names"
	)
			
})

test_that("A warning is generated if modules to align do not contain a time variable", {
			
	data <- data.frame(
		AEDECOD = "a", 
		USUBJID = "1"
	)
	listPlots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "AEDECOD"
	)
	listPlots <- list(A = listPlots)	
			
	expect_warning(
		timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
			listPlots = listPlots,
			timeAlign = "A"
		),
		"A.*not time variant"
	)
		
})

test_that("A warning is generated in case the plot requested to align is not available", {
			
	data <- data.frame(
		AEDECOD = "a", 
		USUBJID = "1"
	)
	listPlots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "AEDECOD"
	)
	listPlots <- list(A = listPlots)	
			
	expect_warning(
		timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
			listPlots = listPlots,
			timeAlign = "C"
		),
		"C.*not available"
	)
			
})

test_that("A warning is generated in case the module to align per subject is not specified among the modules to align", {
			
	data <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- replicate(2, listPlots, simplify = FALSE)
	names(listPlots) <- c("A", "B")
			
	expect_warning(
		timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
			listPlots = listPlots,
			timeAlign = "A",
			timeAlignPerSubject = "B"
		),
		"B.*not.*available.*modules to align"
	)
			
})

test_that("A warning is generated if the inverse of a transformation is not a function", {
			
	# rare case that inverse of trans not specified as a function
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 10),
		USUBJID = "1"
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY",
		timeTrans = scales::log10_trans()
	)
	attr(listPlotsA, "metaData")$timeTrans$inverse <- "function(x) 10^x"
	
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = "1"
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
	
	expect_warning(
		try(timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(listPlots), silent = TRUE),
		"transformation.*not available as a function"
	)
	
})

test_that("Time limits are correctly set for a subset of the modules", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 10),
		USUBJID = "1"
	)
	timeLimA <- c(0, 10)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY",
		timeLim = timeLimA
	)
	
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = "1"
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
			
	expect_silent(
		timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(listPlots)
	)
	expect_length(timeLim, 2)
	expect_equal(timeLim[["A"]], timeLimA)
	expect_equal(timeLim[["B"]], timeLimA)
			
})