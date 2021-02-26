context("Subject profiles are combined")

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
	expect_is(ggMissingModuleg, "ggplot")
	
	# two panels are created for this subject
	expect_length(ggMissingModule$layers, 2)
			
})