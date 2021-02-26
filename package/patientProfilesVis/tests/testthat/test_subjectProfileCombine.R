context("Subject profiles are combined")

library(ggplot2)

test_that("subject profile with empty element is displayed as plot", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = c("1", "1", "2")
	)
	listPlotsMissingSubj <- subjectProfileEventPlot(
		data = subset(data, USUBJID == "1"),
		paramVar = "TEST",
		label = "laboratory measurement",
		timeVar = "DY"
	)
	listPlotsAll <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsMissingSubj, B = listPlotsAll)	
			
	listPlotsSubj <- subjectProfileCombine(listPlots = listPlots)
			
	expect_is(listPlotsSubj, "list")
	expect_named(listPlotsSubj, c("1", "2"))
			
	ggMissingModule <- listPlotsSubj[["2"]][[1]]
	expect_is(ggMissingModule, "ggplot")
			
	# two panels are created for this subject
	expect_length(ggMissingModule$layers, 2)
			
})

test_that("error is returned if plots to align have different time transformations", {
			
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
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = "1"
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY",
		timeTrans = scales::sqrt_trans()
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
			
	expect_error(
		subjectProfileCombine(listPlots),
		"different time transformations",
		ignore.case = TRUE
	)
			
})

test_that("message if only a set plots to align are time-transformed", {
			
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
			
	expect_message(
		subjectProfileCombine(listPlots),
		"transform.*log-10"
	)
	# to add: check that second module is time transformed in the output
			
})

test_that("message if only a set plots to align has time axis expanded", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = "1"
	)
		
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY",
		timeExpand = expansion(mult = 0, add = 3)
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
			
	expect_message(
		subjectProfileCombine(listPlots),
		"module.*expanded"
	)
	# to add: check that second module is time expanded in the output
			
})