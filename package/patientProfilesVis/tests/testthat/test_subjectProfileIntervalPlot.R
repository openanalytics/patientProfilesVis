context("Visualize subject profile interval")

library(ggplot2)
library(reshape2)
library(scales)

test_that("subject variable is specified", {
			
	data <- data.frame(
		TEST = c("A", "B", "C"),
		DY = c(1, 2, 3),
		START = c(1, 2, 3),
		END = c(2, 3, 4),
		SUBJID = factor(c("a", "b", "a"), levels = c("b", "a"))
	)
			
	plots <- subjectProfileIntervalPlot(
		data = data, 
		timeStartVar = "START",
		timeEndVar = "END",
		paramVar = "TEST",
		subjectVar = "SUBJID"
	)
			
	# plots are sorted based on factor levels:
	expect_named(plots, levels(data$SUBJID))
			
})

test_that("error if subject variable is not present in the data", {
			
	data <- data.frame(
		TEST = c("A", "B", "C"),
		DY = c(1, 2, 3),
		START = c(1, 2, 3),
		END = c(2, 3, 4)
	)
	expect_error(
		subjectProfileIntervalPlot(
			data = data, 
			timeStartVar = "START",
			timeEndVar = "END",
			paramVar = "TEST"
		),
		"Variable.*not available in the data"
	)
			
})

test_that("parameter values are correctly displayed by subject", {
			
	data <- data.frame(
		TEST = c("A", "B", "C"),
		START = c(1, 2, 3),
		END = c(2, 3, 4),
		USUBJID = c("a", "b", "a"),
		stringsAsFactors = TRUE
	)
			
	plots <- subjectProfileIntervalPlot(
		data = data, 
		timeStartVar = "START",
		timeEndVar = "END",
		paramVar = "TEST"
	)
	expect_type(plots, "list")
	expect_named(plots, levels(data$USUBJID))
			
	# test data is retained
	for(subjID in unique(data$USUBJID)){
		
		# check that the sublist is a list of ggplot object
		expect_type(plots[[!!subjID]], "list")
		expect_length(plots[[!!subjID]], 1)
		expect_s3_class(plots[[!!subjID]][[1]], c("subjectProfileIntervalPlot", "ggplot"))
		
		gg <- plots[[subjID]][[1]]
		
		dataReferenceSubj <- subset(data, USUBJID == subjID)
		dataReferenceSubj$TEST <- as.character(dataReferenceSubj$TEST)
		
		# extract labels of the y-axis
		yLabel <- layer_scales(gg, 1)$y$range$range
		
		## check that data for points is retained:
				
		# extract data behind the point
		isPointAes <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
		ggDataPoint <- lapply(which(isPointAes), function(i){
			layer_data(gg, i)
		})
		ggDataPoint <- do.call(rbind, ggDataPoint)
		ggDataPoint$yLabel <- yLabel[
			as.numeric(as.factor(ggDataPoint$y))
		]
		ggDataPoint <- ggDataPoint[, c("x", "yLabel")]
		ggDataPoint <- ggDataPoint[do.call(order, ggDataPoint), ]
		
		dataReferencePoint <- reshape2::melt(
			dataReferenceSubj, 
			id.vars = "TEST", 
			measure.vars = c("START", "END")
		)
		dataReferencePoint <- dataReferencePoint[, c("value", "TEST")]
		dataReferencePoint <- dataReferencePoint[do.call(order, dataReferencePoint), ]
		
		expect_equal(
			object = ggDataPoint,
			expected = dataReferencePoint, 
			check.attributes = FALSE
		)

		## check that data for segments is retained:
		
		# extract data behind the point
		isSegmentAes <- sapply(gg$layers, function(l) inherits(l$geom, "GeomSegment"))
		ggDataSegment <- layer_data(gg, which(isSegmentAes))
		ggDataSegment$yLabel <- yLabel[
			as.numeric(as.factor(ggDataSegment$y))
		]
		ggDataSegment <- ggDataSegment[, c("x", "xend", "yLabel")]
		ggDataSegment <- ggDataSegment[do.call(order, ggDataSegment), ]
		
		dataReferenceSegment <- dataReferenceSubj[, c("START", "END", "TEST")]
		dataReferenceSegment <- dataReferenceSegment[do.call(order, dataReferenceSegment), ]
		expect_equal(
			object = ggDataSegment,
			expected = dataReferenceSegment,
			check.attributes = FALSE
		)	
		
	}
	
})

test_that("multiple parameter variables are correctly combined and ordered", {
			
	# example where variables are specified as factor
	# in this case variables are ordered based on factor levels
	dataFactor <- data.frame(
		CAT = factor(c("A", "A", "A", "B"), levels = c("B", "A")),
		TEST = factor(c("a1", "a2", "a3", "b1"), levels = c("a2", "a3", "a1", "b1")),
		START = 1:4,
		END = 2:5,
		USUBJID = "1"
	)
			
	# example with character vector
	# in this case standard R ordering (alphabetical) is used
	dataCharacter <- dataFactor
	dataCharacter[, c("CAT", "TEST")] <- lapply(dataCharacter[, c("CAT", "TEST")], as.character)
			
	dataList <- list(dataFactor, dataCharacter)
			
	for(i in seq_along(dataList)){
		
		expect_equal(
				
			object = {
		
				plots <- subjectProfileIntervalPlot(
					data = dataList[[i]],
					paramVar = c("CAT", "TEST"),
					timeStartVar = "START",
					timeEndVar = "END"
				)
						
				gg <- plots[[1]][[1]]
						
				# extract data behind the point
				isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
				ggDataPoint <- lapply(which(isGeomPoint), function(i){
					layer_data(gg, i)
				})
				ggDataPoint <- do.call(rbind, ggDataPoint)
				
				# extract labels of the y-axis
				yLabel <- layer_scales(gg, 1)$y$range$range
				ggDataPoint$yLabel <- yLabel[
					as.numeric(as.factor(ggDataPoint$y))
				]
				ggDataPoint <- ggDataPoint[with(ggDataPoint, order(y, decreasing = TRUE)), ]
				ggDataPoint <- ggDataPoint[, c("x", "yLabel")]
				
			}, expected = {
						
				# extract input data
				dataReference <- dataList[[i]]
				dataReference <- reshape2::melt(
					dataReference, 
					id.vars = c("CAT", "TEST"), 
					measure.vars = c("START", "END")
				)
				
				dataReference <- dataReference[with(dataReference, order(CAT, TEST)), ]
				dataReference$yLabel <- with(dataReference, paste(CAT, TEST, sep = " - "))
				
				dataReference <- dataReference[, c("value", "yLabel")]
							
			},
			check.attributes = FALSE
		)
	}
	
})

test_that("variable(s) of parameters are combined with specified separator", {
			
	data <- data.frame(
		CAT = c("A", "A", "A", "B"),
		TEST = c("a1", "a2", "a3", "b1"), 
		START = 1:4,
		END = 2:5,
		USUBJID = "1"
	)
	plots <- subjectProfileIntervalPlot(
		data = data,
		paramVar = c("CAT", "TEST"),
		paramVarSep = " and ",
		timeStartVar = "START",
		timeEndVar = "END"
	)
			
	gg <- plots[["1"]][[1]]
			
	# extract data behind the point
	yLabel <- layer_scales(gg, 1)$y$range$range
	yLabel <- rev(yLabel)
	
	dataReference <- data[with(data, order(CAT, TEST)), ]
	dataReference$yLabel <- with(dataReference, paste(CAT, TEST, sep = " and "))
	
	expect_equal(yLabel, dataReference$yLabel)
	
})

test_that("label(s) for parameter variable(s) are specified", {
			
	data <- data.frame(
		CAT = "A", TEST = "a1",
		START = 1:4,
		END = 2:5,
		USUBJID = "1",
		AVAL = 1
	)
			
	expect_equal({
		plots <- subjectProfileIntervalPlot(
			data = data,
			paramVar = c("CAT", "TEST"),
			timeStartVar = "START",
			timeEndVar = "END"
		)
		gg <- plots[[1]][[1]]
		gg$labels$title
	}, expected = "CAT, TEST")
		
	expect_equal({
		plots <- subjectProfileIntervalPlot(
			data = data,
			paramVar = c("CAT", "TEST"),
			timeStartVar = "START",
			timeEndVar = "END",
			paramLab = c(TEST = "Laboratory parameter")
		)
		gg <- plots[[1]][[1]]
		gg$labels$title
	}, expected = "CAT, Laboratory parameter")
	
})

test_that("parameters are grouped based on grouping variable(s)", {
			
	# example where data is first sorted based on multiple
	# grouping variables (factor and character),
	# then param name variable (for a2 vs a1)
	data <- data.frame(
		CAT1 = factor(c("I", "I", "II", "II"), levels = c("II", "I")),
		CAT2 = c("A", "A", "A", "B"), 
		TEST = factor(c("a1", "a2", "a3", "b1"), levels = c("a2", "a3", "a1", "b1")),
		START = 1:4, END = 2:5,
		USUBJID = "1"
	)
	
	plots <- subjectProfileIntervalPlot(
		data = data,
		paramVar = "TEST",
		paramGroupVar = c("CAT1", "CAT2"),
		timeStartVar = "START",
		timeEndVar = "END"
	)
	
	gg <- plots[["1"]][[1]]
	
	# extract labels of the y-axis
	yLabel <- layer_scales(gg, 1)$y$range$range
	# labels are indicated from the bottom to the top of the plot
	yLabel <- rev(yLabel)
	
	dataReference <- data[with(data, order(CAT1, CAT2, TEST)), ]
	dataReference$TEST <- as.character(dataReference$TEST)
	
	expect_equal(yLabel, dataReference$TEST)
	
})

test_that("points are colored based on a variable", {
			
	data <- data.frame(
		TEST = c(1, 1, 2),
		START = c(1, 3, 5),
		END = c(2, 4, 6),
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		USUBJID = "1"
	)
			
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		paramVar = "TEST",
		colorVar = "RIND"
	)
			
	gg <- plots[["1"]][[1]]
	
	## extract color palette of the plot
	ggScales <- gg$scales$scales
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	colorScale <- ggScales[[which(isColorAes)]]
	colorScalePlot <- colorScale$palette(2)
		
	## point
	
	# extract data behind the point
	isPointAes <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- lapply(which(isPointAes), function(i){
		layer_data(gg, i)
	})
	ggDataPoint <- do.call(rbind, ggDataPoint)
	ggDataPoint$y <- as.numeric(as.factor(ggDataPoint$y))
	
	# format reference data
	dataReferencePoint <- reshape2::melt(
		data, 
		id.vars = c("TEST", "RIND"), 
		measure.vars = c("START", "END")
	)
	
	# parameter as sorted from top to the bottom
	dataReferencePoint$y <- with(dataReferencePoint, max(TEST)-TEST)+1
	# missing levels are not displayed
	dataReferencePoint$RIND <- droplevels(dataReferencePoint$RIND)
	
	ggDataPointWithInput <- merge(
		x = ggDataPoint, by.x = c("x", "y"),
		y = dataReferencePoint, by.y = c("value", "y"),
		all = TRUE
	)
			
	# all data is represented
	expect_equal(nrow(ggDataPointWithInput), nrow(dataReferencePoint))
	# color scale based on data
	colorScalePointData <- c(with(ggDataPointWithInput, tapply(colour, RIND, unique)))

	expect_equal(colorScalePointData, colorScalePlot)
	
	## segment
	
	# extract data behind the point
	isSegmentAes <- sapply(gg$layers, function(l) inherits(l$geom, "GeomSegment"))
	ggDataSegment <- layer_data(gg, which(isSegmentAes))

	dataReferenceSegment <- data
	# parameter as sorted from top to the bottom
	dataReferenceSegment$y <- with(dataReferenceSegment, max(TEST)-TEST)+1
	# missing levels are not displayed
	dataReferenceSegment$RIND <- droplevels(dataReferenceSegment$RIND)
	
	ggDataSegmentWithInput <- merge(
		x = ggDataSegment, by.x = c("x", "xend", "y"),
		y = dataReferenceSegment, by.y = c("START", "END", "y"),
		all = TRUE
	)
	
	# all data is represented
	expect_equal(nrow(ggDataSegmentWithInput), nrow(dataReferenceSegment))
	# color scale based on data
	colorScaleSegmentData <- c(with(ggDataSegmentWithInput, tapply(colour, RIND, unique)))
	
	expect_equal(colorScaleSegmentData, colorScalePlot)
	
})

test_that("points are colored with specified palette", {
			
	data <- data.frame(
		TEST = c(1, 1, 2),
		START = c(1, 3, 5),
		END = c(2, 4, 6),
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		USUBJID = "1"
	)
			
	colorPalette <- c(Low = "green", Normal = "blue", High = "red")
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		paramVar = "TEST",
		colorVar = "RIND",
		colorPalette = colorPalette
	)
	gg <- plots[["1"]][[1]]
			
	# extract color palette of the plot
	ggScales <- gg$scales$scales
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	colorScale <- ggScales[[which(isColorAes)]]
	colorScalePlot <- colorScale$palette(3)
	expect_equal(colorScalePlot, colorPalette)
			
})

test_that("color label is specified", {
			
	data <- data.frame(
		TEST = c(1, 1, 2),
		START = c(1, 3, 5),
		END = c(2, 4, 6),
		RIND = c("High", "Normal", "High"),
		USUBJID = "1"
	)
			
	colorLab <- "Reference indicator"
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		paramVar = "TEST",
		colorVar = "RIND",
		colorLab = colorLab
	)
			
	gg <- plots[["1"]][[1]]
	ggScales <- gg$scales$scales
			
	# extract color scale
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	colorScale <- ggScales[[which(isColorAes)]]
	expect_equal(colorScale$name, colorLab)
			
})

test_that("missing time values are not imputed", {
	
	# TEST 1: missing start, missing end
	# TEST 2: complete interval
	# TEST 3: missing start and end date
	data <- data.frame(
		TEST = c(1, 1, 2, 3),
		START = c(1, NA_real_, 5, NA_real_),
		END = c(NA_real_, 4, 6, NA_real_),
		USUBJID = "1"
	)
			
	expect_message(
		plots <- subjectProfileIntervalPlot(
			data = data,
			timeStartVar = "START",
			timeEndVar = "END",
			paramVar = "TEST",
			timeImpType = "none"
		),
		"2 record(s) with missing START and 2 record(s) with missing END are not considered.",
		fixed = TRUE
	)
	
	gg <- plots[[1]][[1]]
	
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- lapply(which(isGeomPoint), function(i){
		layer_data(gg, i)
	})
	ggDataPoint <- do.call(rbind, ggDataPoint)
	ggDataPoint$y <- as.numeric(as.factor(ggDataPoint$y))
	# filter records with missing time
	ggDataPoint <- subset(ggDataPoint, !is.na(x))
	ggDataPoint <- ggDataPoint[do.call(order, ggDataPoint[, c("x", "y")]), ]
	
	# reference data
	dataReference <- data.frame(
		x = c(1, 4, 5, 6),
		y = c(3, 3, 2, 2)
	)
			
	expect_equal(
		ggDataPoint[, c("x", "y")],
		dataReference,
		check.attributes = FALSE # row.names differ
	)
	
	# and corresponding symbol is labelled: 'Complete'
	ggScales <- gg$scales$scales
	isShapeAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "shape")
	)
	shapeScale <- ggScales[[which(isShapeAes)]]
	shapeScalePlot <- shapeScale$palette(1)
	expect_setequal(ggDataPoint$shape, shapeScalePlot["Complete"])
	
	### check that record with all start/end time missing still displayed in axis
	yLabel <- layer_scales(gg, 1)$y$range$range
	expect_equal(yLabel, c("3", "2", "1"))
	
	## no caption for imputation
	expect_null(gg$labels$caption)
			
})

test_that("missing time values are imputed with 'minimal' imputation", {
			
	# TEST 1: missing start, missing end
	# TEST 2: complete interval
	# TEST 3: missing start and end date
	data <- data.frame(
		TEST = c(1, 1, 2, 3),
		START = c(1, NA_real_, 5, NA_real_),
		END = c(NA_real_, 4, 6, NA_real_),
		USUBJID = "1"
	)
			
	expect_message(
		plots <- subjectProfileIntervalPlot(
			data = data,
			timeStartVar = "START",
			timeEndVar = "END",
			paramVar = "TEST",
			timeImpType = "minimal"
		),
		"2 record(s) with missing START and 2 record(s) with missing END are imputed with minimal imputation.",
		fixed = TRUE
	)
			
	gg <- plots[[1]][[1]]
		
	### check that all records are displayed
	
	## extract data behind the point
	
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- lapply(which(isGeomPoint), function(i){
		layer_data(gg, i)
	})
	ggDataPoint <- do.call(rbind, ggDataPoint)
	ggDataPoint$y <- as.numeric(as.factor(ggDataPoint$y))
	# filter records with missing start/end time
	# as they will be displayed at corresponding end/start
	ggDataPoint <- subset(ggDataPoint, !is.na(x) & !is.na(shape))
			
	# add status
	ggScales <- gg$scales$scales
	isShapeAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "shape")
	)
	shapeScale <- ggScales[[which(isShapeAes)]]
	shapeScalePlot <- shapeScale$palette(1)
	ggDataPoint$status <- names(shapeScalePlot)[match(ggDataPoint$shape, shapeScalePlot)]
	
	ggDataPoint <- ggDataPoint[do.call(order, ggDataPoint[, c("y", "x")]), c("x", "y", "status")]
	
	# reference data
	dataReference <- data.frame(
		x = c(5, 6, 1, 4),
		y = c(2, 2, 3, 3),
		status = c("Complete", "Complete", "Missing end", "Missing start"),
		stringsAsFactors = FALSE
	)

	expect_equal(
		ggDataPoint,
		dataReference,
		check.attributes = FALSE # row.names differ
	)
	
	### check that record with all start/end time missing still displayed in axis
	yLabel <- layer_scales(gg, 1)$y$range$range
	expect_equal(yLabel, c("3", "2", "1"))

	## caption with information
	expect_false(is.null(gg$labels$caption))
			
})

test_that("missing time values are imputed based on data records", {
			
	# USUBJID 1: 
	# - TEST 1: missing start, missing end -> imputed by TEST 2
	# - TEST 2: complete interval
	# USUBJID 2: 
	# - TEST 1: missing start and end date -> imputed by data of subject 1	
	data <- data.frame(
		TEST = c(1, 1, 2, 3),
		START = c(1, NA_real_, 0, NA_real_),
		END = c(NA_real_, 4, 7, NA_real_),
		USUBJID = c("1", "1", "1", "2")
	)
			
	expect_message(
		plots <- subjectProfileIntervalPlot(
			data = data,
			timeStartVar = "START",
			timeEndVar = "END",
			paramVar = "TEST",
			timeImpType = "data-based"
		),
		"2 record(s) with missing START and 2 record(s) with missing END are imputed with data-based imputation.",
		fixed = TRUE
	)
			
	## check that all records are displayed
	
	# extract data behind the point
	extractGGData <- function(gg){
		
		isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
		ggDataPoint <- lapply(which(isGeomPoint), function(i){
			layer_data(gg, i)
		})
		ggDataPoint <- do.call(rbind, ggDataPoint)
		ggDataPoint$y <- as.numeric(as.factor(ggDataPoint$y))
		
		# add status
		ggScales <- gg$scales$scales
		isShapeAes <- sapply(ggScales, function(x) 
			all(x[["aesthetics"]] == "shape")
		)
		shapeScale <- ggScales[[which(isShapeAes)]]
		shapeScalePlot <- shapeScale$palette(1)
		ggDataPoint$status <- names(shapeScalePlot)[match(ggDataPoint$shape, shapeScalePlot)]
		
		ggDataPoint <- ggDataPoint[do.call(order, ggDataPoint[, c("y", "x")]), c("x", "y", "status")]
		
		return(ggDataPoint)
		
	}
	
	# subject 1: records imputed by subject-specific data
	ggDataPointSubj1 <- extractGGData(gg = plots[["1"]][[1]])
	dataReferenceSubj1 <- data.frame(
		x = c(1, 7, 0, 4, 0, 7),
		y = c(2, 2, 2, 2, 1, 1),
		status = c(
			# TEST 1: complete/missing end
			"Complete", "Missing end", 
			# TEST 1: missing start/complete
			"Missing start", "Complete",
			# TEST 2
			"Complete", "Complete"
		),
		stringsAsFactors = FALSE
	)
	dataReferenceSubj1 <- dataReferenceSubj1[do.call(order, dataReferenceSubj1[, c("y", "x")]), ]
	expect_equal(
		ggDataPointSubj1,
		dataReferenceSubj1,
		check.attributes = FALSE # row.names differ
	)
	
	# subject 2: records imputed data of other subjects
	ggDataPointSubj2 <- extractGGData(gg = plots[["2"]][[1]])
	dataReferenceSubj2 <- data.frame(
		x = c(0, 7),
		y = c(1, 1),
		status = c("Missing start", "Missing end"),
		stringsAsFactors = FALSE
	)
	expect_equal(
		ggDataPointSubj2,
		dataReferenceSubj2,
		check.attributes = FALSE # row.names differ
	)
	
	## caption with information
	expect_false(is.null(plots[["1"]][[1]]$labels$caption))
	
})

test_that("time values imputed from the data are set to the interval [0, Inf] when all data records are missing", {
			
	data <- data.frame(
		TEST = 1,
		START = NA_real_,
		END = NA_real_, 
		USUBJID = "1"
	)
			
	expect_message(
		plots <- subjectProfileIntervalPlot(
			data = data,
			timeStartVar = "START",
			timeEndVar = "END",
			paramVar = "TEST",
			timeImpType = "data-based"
		)
	)
	gg <- plots[["1"]][[1]]
			
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- lapply(which(isGeomPoint), function(i){
		layer_data(gg, i)
	})
	ggDataPoint <- do.call(rbind, ggDataPoint)
	expect_equal(ggDataPoint$x, c(0, Inf))
			
})


test_that("missing time values are imputed based on an external dataset", {
			
	# USUBJID 1: missing end, complete interval
	# USUBJID 2: missing start and end date
	# USUBJID 3: missing start
	data <- data.frame(
		TEST = c(1, 1, 2, 3),
		START = c(1, NA_real_, 5, NA_real_),
		END = c(NA_real_, 4, 6, NA_real_),
		USUBJID = c("1", "3", "1", "2")
	)
	
	# only specified for subjects 1 and 2:
	# USUBJID 1 and 2: imputed based on this subject-specific data
	# USUBJID 3: imputed based on other subjects specific data
	timeLimData <- data.frame(
		USUBJID = c("1", "2"),
		START_VISIT = c(-1, 0),
		END_VISIT = c(8, 10)
	)
	
	expect_message(
		plots <- subjectProfileIntervalPlot(
			data = data,
			timeStartVar = "START",
			timeEndVar = "END",
			paramVar = "TEST",
			timeLimData = timeLimData,
			timeLimStartVar = "START_VISIT",
			timeLimEndVar = "END_VISIT"
		),
		"2 record(s) with missing START and 2 record(s) with missing END are imputed with START_VISIT/END_VISIT",
		fixed = TRUE
	)
	
	## extract data behind the point
	extractGGData <- function(gg){
		
		isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
		ggDataPoint <- lapply(which(isGeomPoint), function(i){
			layer_data(gg, i)
		})
		ggDataPoint <- do.call(rbind, ggDataPoint)
		ggDataPoint$y <- as.numeric(as.factor(ggDataPoint$y))
		
		# add status
		ggScales <- gg$scales$scales
		isShapeAes <- sapply(ggScales, function(x) 
			all(x[["aesthetics"]] == "shape")
		)
		shapeScale <- ggScales[[which(isShapeAes)]]
		shapeScalePlot <- shapeScale$palette(1)
		ggDataPoint$status <- names(shapeScalePlot)[match(ggDataPoint$shape, shapeScalePlot)]
		
		ggDataPoint <- ggDataPoint[do.call(order, ggDataPoint[, c("y", "x")]), c("x", "y", "status")]
		
		return(ggDataPoint)
		
	}
	
	# subject 1: one missing start record imputed by timeLimData for this subject
	ggDataPointSubj1 <- extractGGData(gg = plots[["1"]][[1]])
	dataReferenceSubj1 <- data.frame(
		x = c(5, 6, 1, 8),
		y = c(1, 1, 2, 2),	
		status = c("Complete", "Complete", "Complete", "Missing end"),
		stringsAsFactors = FALSE
	)
	expect_equal(
		ggDataPointSubj1,
		dataReferenceSubj1,
		check.attributes = FALSE # row.names differ
	)
	
	# subject 2: two missings record imputed by timeLimData for this subject
	ggDataPointSubj2 <- extractGGData(gg = plots[["2"]][[1]])
	dataReferenceSubj2 <- data.frame(
		x = c(0, 10),
		y = c(1, 1),	
		status = c("Missing start", "Missing end"),
		stringsAsFactors = FALSE
	)
	expect_equal(
		ggDataPointSubj2,
		dataReferenceSubj2,
		check.attributes = FALSE # row.names differ
	)
	
	# subject 3: one missing end record imputed by timeLimData across other subjects
	ggDataPointSubj3 <- extractGGData(gg = plots[["3"]][[1]])
	dataReferenceSubj3 <- data.frame(
		x = c(-1, 4),
		y = c(1, 1),	
		status = c("Missing start", "Complete"),
		stringsAsFactors = FALSE
	)
	expect_equal(
		ggDataPointSubj3,
		dataReferenceSubj3,
		check.attributes = FALSE # row.names differ
	)
	
	## caption with information
	expect_false(is.null(plots[["1"]][[1]]$labels$caption))
			
})

test_that("warning in case external dataset but not time variables are specified", {
			
	data <- data.frame(
		TEST = "1",
		START = 1,
		END = 4,
		USUBJID = "1"
	)

	timeLimData <- data.frame(
		USUBJID = c("1", "2"),
		START_VISIT = c(-1, 0),
		END_VISIT = c(8, 10)
	)
			
	expect_warning(
		plots <- subjectProfileIntervalPlot(
			data = data,
			timeStartVar = "START",
			timeEndVar = "END",
			paramVar = "TEST",
			timeLimData = timeLimData
		),
		"start/end variable(s) are not specified",
		fixed = TRUE
	)
	
})
		
test_that("points are set transparent", {
			
	data <- data.frame(
		TEST = c(1, 1, 2),
		START = c(1, 3, 5),
		END = c(2, 4, 6),
		RIND = c("High", "Normal", "High"),
		USUBJID = "1"
	)
			
	alpha <- 0.3
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		paramVar = "TEST",
		alpha = alpha
	)
	gg <- plots[["1"]][[1]]
			
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- lapply(which(isGeomPoint), function(i){
		layer_data(gg, i)
	})
	ggDataPoint <- do.call(rbind, ggDataPoint)
	
	expect_setequal(ggDataPoint$alpha, alpha)
			
})


test_that("a transformation is applied on the time variable", {
			
	data <- data.frame(
		TEST = seq(3),
		START = c(1, 10, 100),
		END = c(1, 10, 100) + 5,
		USUBJID = "1"
	)
	
	timeTrans <- scales::log10_trans()
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		timeTrans = timeTrans,
		paramVar = "TEST"
	)
			
	gg <- plots[["1"]][[1]]
			
	# extract x-scale
	ggScales <- gg$scales$scales
	isXAes <- sapply(ggScales, function(x) 
		any("x" %in% x[["aesthetics"]])
	)
	xScale <- ggScales[[which(isXAes)]]
		
	expect_identical(xScale$trans, timeTrans)
			
})

test_that("time axis is expanded", {
			
	data <- data.frame(
		TEST = seq(3),
		START = c(1, 3, 5),
		END = c(2, 4, 6),
		USUBJID = "1"
	)
			
	timeExpand <- expansion(mult = 0, add = 3)
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		timeExpand = timeExpand,
		paramVar = "TEST"
	)
			
	gg <- plots[["1"]][[1]]
			
	# extract x-scale
	ggScales <- gg$scales$scales
	isXAes <- sapply(ggScales, function(x) 
		any("x" %in% x[["aesthetics"]])
	)
	xScale <- ggScales[[which(isXAes)]]
			
	expect_identical(xScale$expand, timeExpand)
			
})

test_that("time limits are specified", {
			
	data <- data.frame(
		TEST = seq(3),
		START = c(1, 3, 5),
		END = c(2, 4, 6),
		USUBJID = "1"
	)
			
	timeLim <- c(0, 10)
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		timeLim = timeLim,
		paramVar = "TEST"
	)
	gg <- plots[["1"]][[1]]
			
	expect_identical(gg$coordinates$limits$x, timeLim)
			
	expect_identical(attr(plots, "metaData")$timeLim, timeLim)
			
})

test_that("visualizations are not aligned in the time axis", {
			
	data <- data.frame(
		TEST = c("A", "B", "A", "B"),
		START = c(1, 3, 5, 7),
		END = c(2, 4, 6, 8),
		USUBJID = c("1", "1", "2", "2")
	)
			
	expect_silent(
		plots <- subjectProfileIntervalPlot(
			data = data,
			paramVar = "TEST",
			timeStartVar = "START",
			timeEndVar = "END",
			timeAlign = FALSE
		)
	)
	expect_null(plots[["1"]]$coordinates$limits$x)
	expect_null(plots[["2"]]$coordinates$limits$x)
			
})

test_that("visualizations are aligned in the time axis", {
			
	data <- data.frame(
		TEST = c("A", "B", "A", "B"),
		START = c(1, 3, 5, 7),
		END = c(2, 4, 6, 8),
		USUBJID = c("1", "1", "2", "2")
	)
			
	timeLim <- c(0, 10)
	expect_silent(
		plots <- subjectProfileIntervalPlot(
			data = data,
			paramVar = "TEST",
			timeStartVar = "START",
			timeEndVar = "END",
			timeLim = timeLim,
			timeAlign = TRUE
		)
	)
	expect_equal(plots[["1"]][[1]]$coordinates$limits$x, timeLim)
	expect_equal(plots[["2"]][[1]]$coordinates$limits$x, timeLim)
			
})


test_that("time axis alignment and specification of time limits is not compatible", {
			
	data <- data.frame(
		TEST = c("A", "B", "A", "B"),
		START = c(1, 3, 5, 7),
		END = c(2, 4, 6, 8),
		USUBJID = c("1", "1", "2", "2")
	)
			
	expect_warning(
		plots <- subjectProfileIntervalPlot(
			data = data,
			paramVar = "TEST",
			timeStartVar = "START",
			timeEndVar = "END",
			timeLim = c(0, 10),
			timeAlign = FALSE
		),
		"Time limits are not set"
	)
	expect_null(plots[["1"]][[1]]$coordinates$limits$x)
	expect_null(plots[["2"]][[1]]$coordinates$limits$x)
			
})

test_that("label is specified for the x variable", {
			
	data <- data.frame(
		TEST = seq(3),
		START = seq(3),
		END = seq(3),
		USUBJID = "1"
	)
			
	xLab <- "Relative day of the study"
	plots <- subjectProfileIntervalPlot(
		data = data,
		paramVar = "TEST",
		timeStartVar = "START",
		timeEndVar = "END",
		xLab = xLab
	)
	gg <- plots[["1"]][[1]]
			
	expect_identical(gg$labels$x, xLab)
			
})

test_that("label is specified for the y variable", {
			
	data <- data.frame(
		TEST = seq(3),
		START = seq(3),
		END = seq(3),
		USUBJID = "1"
	)
			
	yLab <- "Parameter of interest"
	plots <- subjectProfileIntervalPlot(
		data = data,
		paramVar = "TEST",
		timeStartVar = "START",
		timeEndVar = "END",
		yLab = yLab
	)
			
	gg <- plots[["1"]][[1]]
			
	expect_identical(gg$labels$y, yLab)
			
})

test_that("title is specified", {
			
	data <- data.frame(
		TEST = seq(3),
		START = seq(3),
		END = seq(3),
		USUBJID = "1"
	)
	title <- "Laboratory parameters"
			
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		paramVar = "TEST",
		title = title
	)
			
	gg <- plots[["1"]][[1]]
			
	expect_identical(
		object = gg$labels$title, 
		expected = title
	)
			
})

test_that("label is specified", {
			
	data <- data.frame(
		TEST = seq(3),
		START = seq(3),
		END = seq(3),
		USUBJID = "1"
	)
	label <- "laboratory information"
			
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		paramVar = "TEST",
		label = label
	)
			
	expect_identical(
		attr(plots, "metaData")$label,
		expected = label
	)
			
})

test_that("points at the start or end of the interval are shaped based on a variable", {
			
	data <- data.frame(
		TEST = seq(2),
		START = c(1, -10),
		END = c(2, 0),
		START_STATUS = c("Just started", "Long ago"),
		END_STATUS = c("Ongoing", "Just finished"),
		USUBJID = "1",
		stringsAsFactors = FALSE
	)
	
	shapeVars <- list(timeStartShapeVar = "START_STATUS", timeEndShapeVar = "END_STATUS")
	for(varName in names(shapeVars)){
			
		args <- list(
			data = data,
			timeStartVar = "START",
			timeEndVar = "END",
			paramVar = "TEST"
		)
		args <- c(args, shapeVars[varName])

		plots <- do.call(subjectProfileIntervalPlot, args)
		gg <- plots[["1"]][[1]]	
		
		# extract data behind the point
		isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
		ggDataPoint <- lapply(which(isGeomPoint), function(i){
			layer_data(gg, i)
		})
		ggDataPoint <- do.call(rbind, ggDataPoint)
		ggDataPoint$y <- as.numeric(as.factor(ggDataPoint$y))
		
		# format reference data
		shapeVar <- shapeVars[[varName]]
		dataReference <- data
		# parameter as sorted from top to the bottom
		dataReferencePoint <- reshape2::melt(
			dataReference, 
			id.vars = c("TEST", shapeVar),
			measure.vars = sub("_STATUS$", "", shapeVar)
		)	
		dataReferencePoint$y <- with(dataReference, max(TEST)-TEST)+1
		
		ggDataPointWithInput <- merge(
			x = dataReferencePoint, by.x = c("value", "y"),
			y = ggDataPoint, by.y = c("x", "y"),
			all.x = TRUE
		)
		
		# check that all data is in the plot
		expect_setequal(!is.na(ggDataPointWithInput$shape), TRUE)
		
		# check that symbols are correctly set
		ggScales <- gg$scales$scales
		isShapeAes <- sapply(ggScales, function(x) 
			all(x[["aesthetics"]] == "shape")
		)
		shapeScale <- ggScales[[which(isShapeAes)]]
		shapeScalePlot <- shapeScale$palette(1)
		ggDataPointWithInput$shapeLabel <- names(shapeScalePlot)[match(ggDataPointWithInput$shape, shapeScalePlot)]
		expect_equal(ggDataPointWithInput$shapeLabel, ggDataPointWithInput[[shapeVar]])
		
	}
	
})

test_that("points are shaped with specified palette", {
			
	data <- data.frame(
		TEST = seq(2),
		START = c(1, -10),
		END = c(2, 0),
		START_STATUS = c("Just started", "Long ago"),
		END_STATUS = c("Ongoing", "Just finished"),
		USUBJID = "1",
		stringsAsFactors = FALSE
	)
			
	shapePalette <- c(
		`Just started` = 'diamond', `Long ago` = 'cross', 
		`Ongoing` = 'square', `Just finished` = 'triangle'
	)
	
	shapeVars <- list(timeStartShapeVar = "START_STATUS", timeEndShapeVar = "END_STATUS")
	for(varName in names(shapeVars)){
		
		args <- list(
			data = data,
			timeStartVar = "START",
			timeEndVar = "END",
			paramVar = "TEST",
			shapePalette = shapePalette
		)
		args <- c(args, shapeVars[varName])
		
		plots <- do.call(subjectProfileIntervalPlot, args)
		gg <- plots[["1"]][[1]]	
		
		# extract shape palette of the plot
		ggScales <- gg$scales$scales
		isShapeAes <- sapply(ggScales, function(x) 
			all(x[["aesthetics"]] == "shape")
		)
		shapeScale <- ggScales[[which(isShapeAes)]]
		shapeScalePlot <- shapeScale$palette(1)
		
		# check that shape palette contains the correct symbols
		# for the specified labels
		expect_equal(
			shapeScalePlot[names(shapePalette)],
			shapePalette
		)
		
	}
})

test_that("shape label is specified", {
			
	data <- data.frame(
		TEST = seq(2),
		START = c(1, -10),
		END = c(2, 0),
		START_STATUS = c("Just started", "Long ago"),
		END_STATUS = c("Ongoing", "Just finished"),
		USUBJID = "1",
		stringsAsFactors = FALSE
	)
			
	shapeLab <- "Time status"
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		timeStartShapeVar = "START_STATUS", 
		timeEndShapeVar = "END_STATUS",
		paramVar = "TEST",
		shapeLab = shapeLab
	)
			
	gg <- plots[["1"]][[1]]
			
	# extract shape scale
	ggScales <- gg$scales$scales
	isShapeAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "shape")
	)
	shapeScale <- ggScales[[which(isShapeAes)]]
	expect_equal(shapeScale$name, shapeLab)
		
})

test_that("symbols are set to a specific size", {
			
	data <- data.frame(
		TEST = seq(2),
		START = c(1, -10),
		END = c(2, 0),
		START_STATUS = c("Just started", "Long ago"),
		END_STATUS = c("Ongoing", "Just finished"),
		USUBJID = "1",
		stringsAsFactors = FALSE
	)
			
	shapeSize <- 10
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		timeStartShapeVar = "START_STATUS", 
		timeEndShapeVar = "END_STATUS",
		paramVar = "TEST",
		shapeSize = shapeSize
	)
	gg <- plots[["1"]][[1]]
			
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- lapply(which(isGeomPoint), function(i){
		layer_data(gg, i)
	})
	ggDataPoint <- do.call(rbind, ggDataPoint)
	expect_setequal(ggDataPoint$size, shapeSize)
			
})

test_that("variable labels are specified", {
			
	data <- data.frame(
		TEST = seq(3),
		START = seq(3),
		START_STATUS = c("Just started", "Long ago", "Started"),
		END_STATUS = c("Ongoing", "Just finished", "Finished"),
		END = seq(3),
		NRIND = c("High", "Normal", "High"),
		USUBJID = "1"
	)
	timeLimData <- data.frame(
		USUBJID = "1",
		START_VISIT = 0,
		END_VISIT = 10
	)
			
	labelVars <- c(
		START =  "Start relative day",
		END = "End relative day",
		START_STATUS = "Start status",
		END_STATUS = "End status",
		START_VISIT = "First visit",
		END_VISIT = "End visit",
		TEST = "Parameter",
		NRIND = "Reference indicator"
	)
	plots <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		timeStartShapeVar = "START_STATUS", 
		timeEndShapeVar = "END_STATUS",
		timeLimData = timeLimData,
		timeLimStartVar = "START_VISIT",
		timeLimEndVar = "END_VISIT",
		colorVar = "NRIND",
		paramVar = "TEST",
		labelVars = labelVars
	)
	gg <- plots[["1"]][[1]]
			
	expect_identical(gg$labels$title, "Parameter")
	expect_identical(gg$labels$x, "Start relative day, End relative day")
	
	expect_match(gg$labels$caption, "First visit")
	expect_match(gg$labels$caption, "End visit")
	
	ggScales <- gg$scales$scales
	
	# title for shape legend
	isShapeAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "shape")
	)
	shapeScale <- ggScales[[which(isShapeAes)]]
	expect_equal(shapeScale$name, "Start status, End status")
	
	# title for color legend
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	colorScale <- ggScales[[which(isColorAes)]]
	expect_equal(unname(colorScale$name), "Reference indicator")
	
})