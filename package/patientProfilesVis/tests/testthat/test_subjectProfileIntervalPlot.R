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
		USUBJID = c("a", "b", "a")
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
	
	data <- data.frame(
		TEST = c(1, 1, 2, 2),
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
	
	# format reference data
	dataReferencePoint <- reshape2::melt(
		data, 
		id.vars = "TEST", 
		measure.vars = c("START", "END")
	)
	# parameter as sorted from top to the bottom
	dataReferencePoint$y <- with(dataReferencePoint, max(TEST)-TEST)+1
	dataReferencePoint <- dataReferencePoint[, c("value", "y")]
	# filter records with missing time
	dataReferencePoint <- subset(dataReferencePoint, !is.na(value))
	dataReferencePoint <- dataReferencePoint[do.call(order, dataReferencePoint), ]
	
	expect_equal(
		ggDataPoint[, c("x", "y")],
		dataReferencePoint[, c("value", "y")],
		check.attributes = FALSE
	)
	
	# and corresponding symbol is labelled: 'Complete'
	ggScales <- gg$scales$scales
	isShapeAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "shape")
	)
	shapeScale <- ggScales[[which(isShapeAes)]]
	shapeScalePlot <- shapeScale$palette(1)
	expect_setequal(ggDataPoint$shape, shapeScalePlot["Complete"])
			
})

test_that("missing time values are imputed with 'minimal' imputation", {
			
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
	ggDataPoint <- ggDataPoint[do.call(order, ggDataPoint[, c("x", "y")]), ]
			
	# add status
	ggScales <- gg$scales$scales
	isShapeAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "shape")
	)
	shapeScale <- ggScales[[which(isShapeAes)]]
	shapeScalePlot <- shapeScale$palette(1)
	ggDataPoint$status <- names(shapeScalePlot)[match(ggDataPoint$shape, shapeScalePlot)]
			
	## format reference data
	data$id <- seq_len(nrow(data))
	dataReferencePoint <- reshape2::melt(
		data, 
		id.vars = c("id", "TEST"),
		measure.vars = c("START", "END")
	)
	# parameter as sorted from top to the bottom
	dataReferencePoint$y <- with(dataReferencePoint, max(TEST)-TEST)+1
	# filter records with missing start/end time
	# as they will be displayed at corresponding end/start
	dataReferencePoint <- subset(dataReferencePoint, !is.na(value))
	dataReferencePoint <- dataReferencePoint[do.call(order, dataReferencePoint[, c("value", "y")]), ]
			
	dataReferencePoint <- plyr::ddply(dataReferencePoint, "id", function(x){
		status <- if(all(c("START", "END") %in% x$variable)){
			"Complete"
		}else	c(START = "Missing end", END = "Missing start")[x$variable]
		cbind.data.frame(x, status = status, stringsAsFactors = FALSE)
	})
	
	expect_equal(
		ggDataPoint[, c("x", "y", "status")],
		dataReferencePoint[, c("value", "y", "status")],
		check.attributes = FALSE
	)
	
	### check that record with all start/end time missing still displayed in axis
	yLabel <- layer_scales(gg, 1)$y$range$range
	expect_true("3" %in% yLabel)
			
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



#context("Compare 'subjectProfileIntervalPlot' with previous version")
#
#library(glpgUtilityFct)
#data(SDTMDataPelican)
#data(labelVarsSDTMPelican)
#
## AEPTCD: preferred term code
#dataPlot <- SDTMDataPelican$AE
## specify order of value in 'AESEV'
#dataPlot[, "AESEV"] <- factor(dataPlot[, "AESEV"], levels = c("MILD", "MODERATE"))
#
#test_that("subjectProfileIntervalPlot - default time limits extraction", {
#			
#	aePlots <- subjectProfileIntervalPlot(
#		data = dataPlot,
#		paramVar = "AETERM",
#		timeStartVar = "AESTDY",
#		timeEndVar = "AEENDY",
#		colorVar = "AESEV",
#		labelVars = labelVarsSDTMPelican,
#		title = "Adverse events"
#	)
#			
#	vdiffr::expect_doppelganger(
#		title = "timeLimitsDefault", 
#		fig = aePlots[["study-4903-01"]][[1]],
#		path = "subjectProfileIntervalPlot",
#		verbose = TRUE
#	)
#			
#})
#
#test_that("subjectProfileIntervalPlot - default time limits extraction - all missing values", {
#			
#	aePlots <- subjectProfileIntervalPlot(
#		data = dataPlot,
#		paramVar = "AETERM",
#		timeStartVar = "AESTDY",
#		timeEndVar = "AEENDY",
#		colorVar = "AESEV",
#		labelVars = labelVarsSDTMPelican,
#		title = "Adverse events"
#	)
#	
#	vdiffr::expect_doppelganger(
#		title = "timeLimitsDefaults-Missing", 
#		fig = aePlots[["study-4902-01"]][[1]],
#		path = "subjectProfileIntervalPlot",
#		verbose = TRUE
#	)
#			
#			
#})			
#
#test_that("subjectProfileIntervalPlot - time limits extracted from a dataset", {
#			
#	aePlotsTimLimFromSV <- subjectProfileIntervalPlot(
#		data = dataPlot,
#		paramVar = "AETERM",
#		timeStartVar = "AESTDY",
#		timeEndVar = "AEENDY",
#		colorVar = "AESEV",
#		labelVars = labelVarsSDTMPelican,
#		title = "Adverse events",
#		timeLimData = SDTMDataPelican$SV,
#		timeLimStartVar = "SVSTDY", 
#		timeLimEndVar = "SVENDY"
#	)
#			
#	vdiffr::expect_doppelganger(
#		title = "timeLimitsFromDataSpecification", 
#		fig = aePlotsTimLimFromSV[[1]][[1]],
#		path = "subjectProfileIntervalPlot",
#		verbose = TRUE
#	)
#			
#})
#
#test_that("subjectProfileIntervalPlot - multiple parameter variables", {
#			
#	# AEPTCD: preferred term codes
#	exPlots <- subjectProfileIntervalPlot(
#		data = SDTMDataPelican$EX,
#		paramVar = c("EXTRT", "EXDOSE", "EXDOSU"),
#		timeStartVar = "EXSTDY",
#		timeEndVar = "EXENDY",
#		colorVar = "EXDOSFRM",
#		labelVars = labelVarsSDTMPelican,
#		title = "Treatment exposure"
#	)
#	
#	vdiffr::expect_doppelganger(
#		title = "paramVarMultiple", 
#		fig = exPlots[[1]][[1]],
#		path = "subjectProfileIntervalPlot",
#		verbose = TRUE
#	)
#			
#})
#
#test_that("subjectProfileIntervalPlot - time limits fixed", {
#			
#	timeLim <- c(-28, 53)
#	dataCM <- SDTMDataPelican$CM
#	cmPlotsTimeSpec <- subjectProfileIntervalPlot(
#		data = dataCM,
#		paramVar = "CMDECOD",
#		timeStartVar = "CMSTDY",
#		timeEndVar = "CMENDY",
#		paramGroupVar = "CMINDC",
#		colorVar = "CMINDC",
#		labelVars = labelVarsSDTMPelican,
#		title = "Concomitant medications"#,
##		timeLim = timeLim
#	)
#	
##	# consider subject with start and end date and with max non missing end date
##	subjectWithStartEndTime <- by(dataCM, dataCM$USUBJID, function(x){
##		if(any(!is.na(x$CMSTDY) & !is.na(x$CMENDY)))	sum(!is.na(x$CMENDY))
##	})
##	subject <- names(which.max(unlist(subjectWithStartEndTime))) #"study-4905-02"
#	
#	vdiffr::expect_doppelganger(
#		title = "timeLimitsFixed", 
#		fig = cmPlotsTimeSpec[["study-4905-02"]][[1]],
#		path = "subjectProfileIntervalPlot",
#		verbose = TRUE
#	)
#
#})
#
#test_that("subjectProfileIntervalPlot - mix of Unicode and standard ggplot2 shape palette", {
#
#	cmPlots <- subjectProfileIntervalPlot(
#		data = SDTMDataPelican$CM,
#		paramVar ="CMDECOD",
#		timeStartVar = "CMSTDY",
#		timeEndVar = "CMENDY",
#		timeEndShapeVar = "CMENRTPT"
#	)
#	
#	# Error: Can't find shape name: '19' (with version < 1.2.0)
#	print(cmPlots[["study-4902-02"]][[1]])
#	
#})