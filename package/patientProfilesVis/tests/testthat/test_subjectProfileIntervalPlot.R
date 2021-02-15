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