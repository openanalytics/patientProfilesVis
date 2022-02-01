context("Combine subject profiles")

library(ggplot2)
library(shiny)

test_that("Subject profiles with empty elements are still displayed as plots", {
			
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

test_that("An error is generated if the subject profiles to align have different time transformations", {
			
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

test_that("A message is generated if the time axis of the subject profiles needs to be transformed to be time-aligned", {
			
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
			
})

test_that("A message is generated if the time axis of the subject profiles needs to be expanded to be time-aligned", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = "1"
	)
		
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY",
		timeExpand = ggplot2::expansion(mult = 0, add = 3)
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
			
})

test_that("Subject profiles with different time axis expansions are successfully combined", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = "1"
	)
			
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY",
		timeExpand = ggplot2::expansion(mult = 0, add = 3)
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
		timeExpand = expansion(mult = 2, add = 0)
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
			
	expect_message(
		subjectProfileCombine(listPlots),
		".+ expanded.+ to be time aligned"
	)
			
})

test_that("Subject profiles are successfully combined in a parallel framework", {
			
	data <- data.frame(
		TEST = "1",
		DY = c(1, 10),
		USUBJID = "1"
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- replicate(10, listPlots, simplify = FALSE)	
	names(listPlots) <- as.character(seq_len(10))
	
	expect_silent(
		listPlotsSubj <- subjectProfileCombine(listPlots, nCores = 2)
	)
	
})

test_that("Progress information is displayed only when requested when combining patient profiles", {

	data <- data.frame(
		TEST = "1",
		DY = c(1, 10),
		USUBJID = "1"
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlots)
			
	expect_silent(
		subjectProfileCombine(listPlots, verbose = FALSE)
	)
	
	expect_message(
		subjectProfileCombine(listPlots, verbose = TRUE)
	)

})

test_that("A warning is generated within a shiny app without progress widget", {
			
	data <- data.frame(
		TEST = "1",
		DY = c(1, 10),
		USUBJID = "1"
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlots)
	
	server <- function(id) {
		shiny::moduleServer(id, function(input, output, session) {
			output$test <- shiny::renderPlot(
				subjectProfileCombine(listPlots, shiny = TRUE)
			)
		})
	}
	expect_warning(
		shiny::testServer(server, output$test),
		"progress",
		ignore.case = TRUE
	)
	
})
	
test_that("No error is generated when subject profiles are combined inside a shiny app", {
				
	data <- data.frame(
		TEST = "1",
		DY = c(1, 10),
		USUBJID = "1"
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlots)
	
	server <- function(id) {
		shiny::moduleServer(id, function(input, output, session) {
			output$test <- shiny::renderPlot(
				shiny::withProgress(subjectProfileCombine(listPlots, shiny = TRUE))
			)
		})
	}
	# nice to have: check the progress message directlt
	expect_silent(shiny::testServer(server, output$test))
			
})

test_that("The number of lines to be displayed in the subject profile plot is correctly extracted when plots are combined", {
		
	# in the rare event that list plots are modified
	# by the user and the nLines computed in each plotting
	# function is removed
	dataA <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1",
		AVAL = rnorm(3)
	)
	listPlotsA <- subjectProfileLinePlot(
		data = dataA,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL"
	)
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = "1",
		RIND = c("Low", "High")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY",
		# to have a legend:
		colorVar = "RIND",
		shapeVar = "RIND"
	)
	dataC <- data.frame(
		AEDECOD = "a", 
		USUBJID = "1"
	)
	listPlotsC <- subjectProfileTextPlot(
		data = dataC,
		paramValueVar = "AEDECOD"
	)
	
	listPlots <- list(
		A = listPlotsA, 
		B = listPlotsB, 
		C = listPlotsC
	)	
			
	# remove 'nLines' attribute
	listPlotsWthtNLines <- sapply(listPlots, function(lMod){
		sapply(lMod, function(lSubj){
			sapply(lSubj, function(gg){
				attr(gg, "metaData")$nLines <- NULL
				gg
			}, simplify = FALSE)		
		}, simplify = FALSE)					
	}, simplify = FALSE)

	# number of lines is estimated based on the plot object
	expect_silent(
		listPlotsWthtNLineCombined <- subjectProfileCombine(listPlotsWthtNLines)
	)
	
	# number of lines is extracted from the attribute in 'listPlots'
	expect_silent(
		listPlotsCombined <- subjectProfileCombine(listPlots)
	)
	
	expect_equal(
		object = attr(listPlotsWthtNLineCombined[[1]][[1]], "metaData")$nLines,
		expected = attr(listPlotsCombined[[1]][[1]], "metaData")$nLines	
	)
			
})

test_that("The height of the combined subject profiles is correctly restricted to the specified number of lines", {
			
	data <- data.frame(
		AEDECOD = "a", 
		USUBJID = "1"
	)
	listPlots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "AEDECOD"
	)
	listPlots <- replicate(10, listPlots, simplify = FALSE)	
	names(listPlots) <- as.character(seq_len(10))
	
	maxNLines <- 15
	listPlotsSubj <- subjectProfileCombine(
		listPlots = listPlots,
		maxNLines = maxNLines
	)
	
	# check that no more than specified number of lines for each page:
	nLinesSubjPage <- sapply(listPlotsSubj[["1"]], function(x) attr(x, "metaData")$nLines)
	expect_true(all(nLinesSubjPage < maxNLines))
	
	# check that multiple 'pages' are created
	expect_gte(length(listPlotsSubj[["1"]]), 1)
			
})

test_that("No error is generated when reference lines are set from a specified dataset for combined profile plots", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	) 		
	listPlots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	listPlots <- list(A = listPlots)
			
	dataVS <- data.frame(
		DY = c(1, 2),
		visitName = c("First Visit", "Last Visit"),
		USUBJID = "1"
	)
	
	expect_silent(
		listPlotsSubj <- subjectProfileCombine(
			listPlots = listPlots,
			refLinesData = dataVS,
			refLinesTimeVar = "DY",
			refLinesLabelVar = "visitName"
		)
	)
			
})