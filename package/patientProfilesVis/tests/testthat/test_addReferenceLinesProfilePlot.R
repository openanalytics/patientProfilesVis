context("add reference lines to a subject profile plot")

library(ggplot2)

test_that("reference lines are set from specified list", {
		
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	)
	
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	gg <- plots[["1"]][[1]]
	
	refLines <- list(
		list(time = 0, label = "baseline"),
		list(time = 10, label = "end of the study")
	)
	ggWithLine <- patientProfilesVis:::addReferenceLinesProfilePlot(
		gg = gg,
		refLines = refLines
	)
	
	# extract data behind the vertical lines:
	isGeomVLine <- sapply(ggWithLine$layers, function(l) inherits(l$geom, "GeomVline"))
	ggDataVLine <- lapply(which(isGeomVLine), function(i){
		layer_data(ggWithLine, i)
	})
	ggDataVLine <- do.call(rbind, ggDataVLine)
	
	expect_equal(ggDataVLine$x, c(0, 10))
	expect_setequal(ggDataVLine$linetype, "dotted")
	expect_setequal(ggDataVLine$colour, "black")
			
})

test_that("reference lines are set from specified dataset", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = c("1", "1", "2")
	) 		
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	
	dataVS <- data.frame(
		DY = c(0, 10),
		visitName = c("First Visit", "Last Visit"),
		USUBJID = c("2", "2")
	)

	# plot with patient with reference line
	gg <- patientProfilesVis:::addReferenceLinesProfilePlot(
		gg = plots[["2"]][[1]],
		refLinesData = dataVS,
		refLinesTimeVar = "DY",
		refLinesLabelVar = "visitName"
	)
	# extract data behind the vertical lines:
	isGeomVLine <- sapply(gg$layers, function(l) inherits(l$geom, "GeomVline"))
	expect_true(any(isGeomVLine))
	
	ggDataVLine <- lapply(which(isGeomVLine), function(i){
		layer_data(gg, i)
	})
	ggDataVLine <- do.call(rbind, ggDataVLine)
	expect_equal(ggDataVLine$x, c(0, 10))
	
	# plot for patient without reference lines
	gg <- patientProfilesVis:::addReferenceLinesProfilePlot(
		gg = plots[["1"]][[1]],
		refLinesData = dataVS,
		refLinesTimeVar = "DY",
		refLinesLabelVar = "visitName"
	)
			
	# extract data behind the vertical lines:
	isGeomVLine <- sapply(gg$layers, function(l) inherits(l$geom, "GeomVline"))
	expect_false(any(isGeomVLine))
	
})

test_that("failure if reference line dataset doesn't contain the subject variable", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	) 		
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	dataVS <- data.frame(
		DY = c(0, 10),
		visitName = c("First Visit", "Last Visit")
	)
	
	expect_error(
		gg <- patientProfilesVis:::addReferenceLinesProfilePlot(
			gg = plots[["1"]][[1]],
			refLinesData = dataVS,
			refLinesTimeVar = "DY",
			refLinesLabelVar = "visitName",
			subjectVar = "USUBJID"
		)
	)
			
})

test_that("reference lines are set from specified dataset with custom subject variable", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	) 		
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	
	# specification of subject variable
	expect_equal(
		# custom subject variable
		object = {
			dataVS <- data.frame(
				DY = c(0, 10),
				visitName = c("First Visit", "Last Visit"),
				SUBJID = "1"
			)
			patientProfilesVis:::addReferenceLinesProfilePlot(
				gg = plots[["1"]][[1]],
				refLinesData = dataVS,
				refLinesTimeVar = "DY",
				refLinesLabelVar = "visitName",
				subjectVar = "SUBJID"
			)
		}, 
		# default subject variable
		expected = {
			dataVS <- data.frame(
				DY = c(0, 10),
				visitName = c("First Visit", "Last Visit"),
				USUBJID = "1"
			)
			patientProfilesVis:::addReferenceLinesProfilePlot(
				gg = plots[["1"]][[1]],
				refLinesData = dataVS,
				refLinesTimeVar = "DY",
				refLinesLabelVar = "visitName"
			)
		}
	)
			
})

test_that("reference lines are set with labels from specified list", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	)
			
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	gg <- plots[["1"]][[1]]
			
	refLines <- list(
		list(time = 0, label = "baseline"),
		list(time = 10, label = "end of the study")
	)
	ggWithLine <- patientProfilesVis:::addReferenceLinesProfilePlot(
		gg = gg,
		refLines = refLines,
		addLabel = TRUE
	)
	expect_is(ggWithLine, "list")
	expect_named(ggWithLine, c("gg", "ggRefLines"))
			
	ggLabel <- ggWithLine$ggRefLines
	
	# extract data behind the labels:
	isGeomText <- sapply(ggLabel$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(ggLabel, which(isGeomText))

	expect_equal(ggDataText$x, c(0, 10))
	expect_equal(as.character(ggDataText$label), c("baseline", "end of the study"))
			
})

test_that("reference lines are set with labels from specified dataset", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = c("1", "1", "2")
	) 		
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
			
	dataVS <- data.frame(
		DY = c(0, 10),
		visitName = c("First Visit", "Last Visit"),
		USUBJID = c("2", "2")
	)
	gg <- patientProfilesVis:::addReferenceLinesProfilePlot(
		gg = plots[["2"]][[1]],
		refLinesData = dataVS,
		refLinesTimeVar = "DY",
		refLinesLabelVar = "visitName",
		addLabel = TRUE
	)
	expect_is(gg, "list")
	expect_named(gg, c("gg", "ggRefLines"))
			
	ggLabel <- gg$ggRefLines
			
	# extract data behind the labels:
	isGeomText <- sapply(ggLabel$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(ggLabel, which(isGeomText))
			
	expect_equal(ggDataText$x, c(0, 10))
	expect_equal(as.character(ggDataText$label), c("First Visit", "Last Visit"))
			
})

test_that("reference lines are set with custom color/linetype", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	)
			
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	gg <- plots[["1"]][[1]]
			
	refLines <- list(
		list(time = 0, label = "baseline", color = "purple"),
		list(time = 10, label = "end of the study", linetype = "dashed")
	)
	ggWithLine <- patientProfilesVis:::addReferenceLinesProfilePlot(
		gg = gg,
		refLines = refLines,
		refLinesColor = "green",
		refLinesLinetype = "solid"
	)
			
	# extract data behind the vertical lines:
	isGeomVLine <- sapply(ggWithLine$layers, function(l) inherits(l$geom, "GeomVline"))
	ggDataVLine <- lapply(which(isGeomVLine), function(i){
		layer_data(ggWithLine, i)
		})
	ggDataVLine <- do.call(rbind, ggDataVLine)
	
	# set by line
	expect_equal(subset(ggDataVLine, xintercept == 0)$colour, "purple")
	expect_equal(subset(ggDataVLine, xintercept == 10)$linetype, "dashed")
	
	# set for all lines
	expect_equal(subset(ggDataVLine, xintercept == 10)$colour, "green")
	expect_equal(subset(ggDataVLine, xintercept == 0)$linetype, "solid")
	
})

test_that("reference lines are set with custom color/linetype", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	)
			
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	
	refLines <- list(
		list(time = 0, label = "baseline", color = "purple")
	)
	
	timeLim <- c(0, 4)
	gg <- patientProfilesVis:::addReferenceLinesProfilePlot(
		gg = plots[["1"]][[1]],
		refLines = refLines,
		addLabel = TRUE,
		timeLim = timeLim
	)
	ggLabel <- gg$ggRefLines
	
	expect_identical(ggLabel$coordinates$limits$x, timeLim)

})

test_that("warning if subject ID missing in case of reference lines from specified dataset", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	)
			
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	gg <- plots[["1"]][[1]]
	attr(gg, "metaData") <- NULL
			
	dataVS <- data.frame(
		DY = c(0, 10),
		visitName = c("First Visit", "Last Visit"),
		USUBJID = "1"
	)
	expect_warning(
		ggRefLine <- patientProfilesVis:::addReferenceLinesProfilePlot(
			gg = gg,
			refLinesData = dataVS,
			refLinesTimeVar = "DY",
			refLinesLabelVar = "visitName"
		),
		"no reference lines",
		ignore.case = TRUE
	)
	expect_identical(ggRefLine, gg)
		
})

