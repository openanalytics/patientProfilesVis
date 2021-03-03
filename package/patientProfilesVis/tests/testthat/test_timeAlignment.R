context("time limits are extracted based on time alignment policy")

test_that("plots are aligned across modules and subject", {
			
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

test_that("specification of time alignment as logical is deprecated", {
			
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
	listPlots <- list(A = listPlots)	
	
	expect_warning(
		timeLim <- patientProfilesVis:::getTimeLimSubjectProfilePlots(
			listPlots = listPlots,
			timeAlign = TRUE
		),
		"deprecated"
	)
			
})

test_that("plots are not aligned", {
			
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

test_that("only a set of modules are aligned", {
			
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

test_that("all modules are aligned per subject", {
			
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

test_that("a set of modules aligned across subjects, and a set of modules aligned per subject", {
			
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

test_that("some module aligned across subjects, modules aligned per subject, and module not aligned", {
			
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

test_that("list of plots should be named in case plots are aligned", {
			
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

test_that("list of plots should have unique names in case plots are aligned", {
			
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

test_that("warning if module to align is not time-variant", {
			
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

test_that("warning if module to align is not available", {
			
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

test_that("warning if module to align per subject not specified among modules to align", {
			
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

test_that("warning if inverse of transformation is not a function", {
			
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
		try(timeLim <- getTimeLimSubjectProfilePlots(listPlots), silent = TRUE),
		"transformation.*not available as a function"
	)
	
})

test_that("time limits are set for a subset of the modules", {
			
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
		timeLim <- getTimeLimSubjectProfilePlots(listPlots)
	)
	expect_length(timeLim, 2)
	expect_equal(timeLim[["A"]], timeLimA)
	expect_equal(timeLim[["B"]], timeLimA)
			
})