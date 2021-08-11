context("Visualize subject profile event")

library(ggplot2)
library(scales)

test_that("Subject profile plots are correctly sorted in the output based on the levels of the subject ID variable", {
			
	data <- data.frame(
		TEST = c("A", "B", "C"),
		DY = c(1, 2, 3),
		SUBJID = factor(c("a", "b", "a"), levels = c("b", "a"))
	)
	
	plots <- subjectProfileEventPlot(
		data = data, 
		timeVar = "DY",
		paramVar = "TEST",
		subjectVar = "SUBJID"
	)
	
	# plots are sorted based on factor levels:
	expect_named(plots, levels(data$SUBJID))
			
})

test_that("An error is generated if the subject variable is not present in the data", {
			
	data <- data.frame(
		TEST = c("A", "B", "C"),
		DY = c(1, 2, 3)
	)
	expect_error(
		subjectProfileEventPlot(
			data = data, 
			timeVar = "DY",
			paramVar = "TEST"
		),
		"Variable.*not available in the data"
	)
			
})

test_that("Parameter variables are correctly displayed for each subject", {
			
	data <- data.frame(
		TEST = factor(c("A", "B", "C"), levels = c("B", "C", "A")),
		DY = c(1, 2, 3),
		USUBJID = factor(c("1", "2", "1"), levels = c("2", "1"))
	)
	
	plots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)
	
	# test data is retained
	for(subjID in unique(data$USUBJID)){
		
		# check that the sublist is a list of ggplot object
		expect_type(plots[[!!subjID]], "list")
		expect_length(plots[[!!subjID]], 1)
		expect_s3_class(plots[[!!subjID]][[1]], c("subjectProfileEventPlot", "ggplot"))
		
		expect_equal(
			object = {		
				
				gg <- plots[[!!subjID]][[1]]	
				# extract data behind the point
				isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
				ggDataPoint <- layer_data(gg, which(isGeomPoint))
				xCoord <- ggDataPoint[order(ggDataPoint$y), "x"]
					
				# extract labels of the y-axis
				yLabel <- layer_scales(gg, which(isGeomPoint))$y$range$range
					
				# variables are order from the bottom to the top in the data
				# so use revert order
				dataPlot <- data.frame(x = xCoord, y = yLabel, stringsAsFactors = FALSE)
				
			},
			expected = {
				dataReference <- subset(data, USUBJID == !!subjID)[, c("DY", "TEST")]
				dataReference$TEST <- as.character(dataReference$TEST)
				setNames(dataReference, c("x", "y"))
			},
			check.attributes = FALSE # (rownames differ)
		)		
		
	}
			
})

test_that("Multiple parameter variables are correctly combined and ordered", {
		
	# example where variables are specified as factor
	# in this case variables are ordered based on factor levels
	dataFactor <- data.frame(
		CAT = factor(c("A", "A", "A", "B"), levels = c("B", "A")),
		TEST = factor(c("a1", "a2", "a3", "b1"), levels = c("a2", "a3", "a1", "b1")),
		DY = c(1, 2, 3, 4),
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
	
				plots <- subjectProfileEventPlot(
					data = dataList[[!!i]],
					paramVar = c("CAT", "TEST"),
					timeVar = "DY"
				)
					
				gg <- plots[[1]][[1]]
				
				# extract data behind the point
				isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
				ggDataPoint <- layer_data(gg, which(isGeomPoint))
				ggDataPoint <- ggDataPoint[order(ggDataPoint$y), ]
										
				# extract labels of the y-axis
				ggDataPoint$yLabel <- layer_scales(gg, which(isGeomPoint))$y$range$range
				
				# variables are order from the bottom to the top in the data
				# so use revert order
				ggDataPointOrder <- ggDataPoint[order(ggDataPoint$y, decreasing = TRUE), ]					
										
				ggDataPointOrder[, c("x", "yLabel")]
			
			}, expected = {
				
				data <- dataList[[!!i]]
				dataReference <- data[with(data, order(CAT, TEST)), ]
				dataReference$yLabel <- with(dataReference, paste(CAT, TEST, sep = " - "))
			
				dataReference[, c("DY", "yLabel")]
			
			},
			check.attributes = FALSE
		)
	}
	
})

test_that("Parameter values are correctly combined with a specified separator", {
			
	data <- data.frame(
		CAT = c("A", "A", "A", "B"),
		TEST = c("a1", "a2", "a3", "b1"), 
		DY = c(1, 2, 3, 4),
		USUBJID = "1"
	)
	plots <- subjectProfileEventPlot(
		data = data,
		paramVar = c("CAT", "TEST"),
		paramVarSep = " and ",
		timeVar = "DY"
	)
	gg <- plots[["1"]][[1]]
			
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	yLabel <- layer_scales(gg, which(isGeomPoint))$y$range$range
	yLabel <- rev(yLabel)
	
	dataReference <- data[with(data, order(CAT, TEST)), ]
	dataReference$yLabel <- with(dataReference, paste(CAT, TEST, sep = " and "))
	
	expect_equal(yLabel, dataReference$yLabel)
			
})

test_that("Specified labels for parameter variables are correctly set", {
			
	data <- data.frame(
		CAT = "A", TEST = "a1",
		DY = 1,
		USUBJID = "1"
	)
		
	expect_equal({
		plots <- subjectProfileEventPlot(
			data = data,
			paramVar = c("CAT", "TEST"),
			timeVar = "DY"
		)
		gg <- plots[[1]][[1]]
		gg$labels$title
	}, expected = "CAT, TEST")

	expect_equal({
		plots <- subjectProfileEventPlot(
			data = data,
			paramVar = c("CAT", "TEST"),
			timeVar = "DY",
			paramLab = c(TEST = "Laboratory parameter")
		)
		gg <- plots[[1]][[1]]
		gg$labels$title
	}, expected = "CAT, Laboratory parameter")
			
})

test_that("Parameter values are correctly ordered/grouped based on grouping variables", {
			
	# example where data is first sorted based on multiple
	# grouping variables (factor and character),
	# then param name variable (for a2 vs a1)
	data <- data.frame(
		CAT1 = factor(c("I", "I", "II", "II"), levels = c("II", "I")),
		CAT2 = c("A", "A", "A", "B"), 
		TEST = factor(c("a1", "a2", "a3", "b1"), levels = c("a2", "a3", "a1", "b1")),
		DY = c(1, 2, 3, 4),
		USUBJID = "1"
	)
			
	plots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		paramGroupVar = c("CAT1", "CAT2"),
		timeVar = "DY"
	)
			
	gg <- plots[["1"]][[1]]
	
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	
	# extract labels of the y-axis
	yLabel <- layer_scales(gg, which(isGeomPoint))$y$range$range
	# labels are indicated from the bottom to the top of the plot
	yLabel <- rev(yLabel)
	
	dataReference <- data[with(data, order(CAT1, CAT2, TEST)), ]
	dataReference$TEST <- as.character(dataReference$TEST)
			
	expect_equal(yLabel, dataReference$TEST)
			
})

test_that("Data points are correctly colored based on a specified variable", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		USUBJID = "1"
	)
	
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST",
		colorVar = "RIND"
	)
	
	gg <- plots[["1"]][[1]]
	
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	
	# format reference data
	dataReference <- data
	# parameter as sorted from top to the bottom
	dataReference$y <- with(dataReference, max(TEST)-TEST)+1
	# missing levels are not displayed
	dataReference$RIND <- droplevels(dataReference$RIND)
	
	ggDataPointWithInput <- merge(
		x = ggDataPoint, by.x = c("x", "y"),
		y = dataReference, by.y = c("DY", "y"),
		all = TRUE
	)
	
	# all data is represented
	expect_equal(nrow(ggDataPointWithInput), nrow(data))
	# color scale based on data
	colorScaleData <- c(with(ggDataPointWithInput, tapply(colour, RIND, unique)))

	# extract color palette of the plot
	ggScales <- gg$scales$scales
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	colorScale <- ggScales[[which(isColorAes)]]
	colorScalePlot <- colorScale$palette(2)
	expect_equal(colorScaleData, colorScalePlot)
	
})

test_that("Data points are correctly colored with a specified palette", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		USUBJID = "1"
	)
			
	colorPalette <- c(Low = "green", Normal = "blue", High = "red")
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
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

test_that("A specified label for the color variable is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = c("High", "Normal", "High"),
		USUBJID = "1"
	)
	
	colorLab <- "Reference indicator"
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
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
	
	# extract shape scale
	# by default, shape label also set to color label
	isShapeAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "shape")
	)
	shapeScale <- ggScales[[which(isShapeAes)]]
	expect_equal(shapeScale$name, colorLab)
	
})

test_that("Data point shapes are based on the color variable by default", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		USUBJID = "1"
	)
			
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST",
		colorVar = "RIND"
	)
			
	gg <- plots[["1"]][[1]]
		
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	
	shapes <- c(with(ggDataPoint, tapply(shape, colour, unique)))
	expect_type(shapes, "character")
	expect_length(shapes, 2)
	expect_length(unique(shapes), 2)
			
})

test_that("Data points are correctly shaped based on a specified variable", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		USUBJID = "1"
	)
	
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST",
		shapeVar = "RIND"
	)
	
	gg <- plots[["1"]][[1]]
	
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	
	# format reference data
	dataReference <- data
	# parameter as sorted from top to the bottom
	dataReference$y <- with(dataReference, max(TEST)-TEST)+1
	# missing levels are not displayed
	dataReference$RIND <- droplevels(dataReference$RIND)
	
	ggDataPointWithInput <- merge(
		x = ggDataPoint, by.x = c("x", "y"),
		y = dataReference, by.y = c("DY", "y"),
		all = TRUE
	)
	
	# all data is represented
	expect_equal(nrow(ggDataPointWithInput), nrow(data))
	# shape scale based on data
	shapeScaleData <- c(with(ggDataPointWithInput, tapply(shape, RIND, unique)))
	
	# extract shape palette of the plot
	ggScales <- gg$scales$scales
	isShapeAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "shape")
	)
	shapeScale <- ggScales[[which(isShapeAes)]]
	shapeScalePlot <- shapeScale$palette(2)
	expect_equal(shapeScalePlot, shapeScaleData)
	
})

test_that("Data points are correctly shaped with a specified palette", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		USUBJID = "1"
	)
	
	shapePalette <- c(Low = 25, Normal = 19, High = 24)
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST",
		shapeVar = "RIND",
		shapePalette = shapePalette
	)
	gg <- plots[["1"]][[1]]
	
	# extract color palette of the plot
	ggScales <- gg$scales$scales
	isShapeAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "shape")
	)
	shapeScale <- ggScales[[which(isShapeAes)]]
	shapeScalePlot <- shapeScale$palette(3)
	expect_equal(shapeScalePlot, shapePalette)
	
})

test_that("A specified label for the shape variable is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = c("High", "Normal", "High"),
		USUBJID = "1"
	)
			
	shapeLab <- "Reference indicator"
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST",
		shapeVar = "RIND",
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

test_that("Data points are correctly set transparent", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1"
	)
			
	alpha <- 0.3
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST",
		alpha = alpha
	)
	gg <- plots[["1"]][[1]]
	
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	
	expect_setequal(ggDataPoint$alpha, alpha)
	
})

test_that("Records with missing time points in the time variable are discarded with a message", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(4.5, NA_real_, NA_real_),
		USUBJID = "1"
	)
	expect_message(
		plots <- subjectProfileEventPlot(
			data = data,
			timeVar = "DY",
			paramVar = "TEST"
		),
		"2 record(s) with missing DY are not considered.",
		fixed = TRUE
	)
	gg <- plots[["1"]][[1]]
	
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	ggDataPoint$y <- as.numeric(ggDataPoint$y)
	
	expect_equal(
		ggDataPoint[, c("x", "y")],
		subset(data, !is.na(DY), select = c("DY", "TEST")),
		check.attributes = FALSE
	)
	
})

test_that("A specified label for the time variable is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1"
	)
			
	timeLab <- "Relative day of the study"
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		timeLab = timeLab,
		paramVar = "TEST"
	)
			
	gg <- plots[["1"]][[1]]
	
	# by default used as label for the x-axis
	expect_equal(gg$labels$x, timeLab)

})

test_that("A transformation is correctly applied on the time variable", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 10, 100),
		USUBJID = "1"
	)
			
	timeTrans <- scales::log10_trans()
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
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

test_that("The time axis is correctly expanded if requested", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	)
			
	timeExpand <- expansion(mult = 0, add = 3)
	
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
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

test_that("Limits for the time axis are correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 2, 3),
		USUBJID = "1"
	)
			
	timeLim <- c(2, 3)
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		timeLim = timeLim,
		paramVar = "TEST"
	)
			
	gg <- plots[["1"]][[1]]
			
	expect_identical(gg$coordinates$limits$x, timeLim)
	
	expect_identical(attr(plots, "metaData")$timeLim, timeLim)
			
})

test_that("A label for the variable on the x-axis is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1"
	)
			
	xLab <- "Relative day of the study"
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		xLab = xLab,
		paramVar = "TEST"
	)
			
	gg <- plots[["1"]][[1]]
			
	expect_identical(gg$labels$x, xLab)
			
})

test_that("A label for the variable on the y-axis is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1"
	)
			
	yLab <- "Parameter of interest"
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		yLab = yLab,
		paramVar = "TEST"
	)
			
	gg <- plots[["1"]][[1]]
			
	expect_identical(gg$labels$y, yLab)
			
})

test_that("A title is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1"
	)
	title <- "Laboratory parameters"
	
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		title = title,
		paramVar = "TEST"
	)
			
	gg <- plots[["1"]][[1]]
	
	expect_identical(
		object = gg$labels$title, 
		expected = title
	)
			
})

test_that("A label for the metadata of the subject profile plots is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1"
	)
	label <- "laboratory information"
			
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		label = label,
		paramVar = "TEST"
	)
						
	expect_identical(
		attr(plots, "metaData")$label,
		expected = label
	)
			
})

test_that("Labels for aesthetic, plot or axis title are correctly extracted from the specified variable labels", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = c("High", "Normal", "High"),
		USUBJID = "1"
	)
			
	# label specified for a subset of the variable(s)
	labelVars <- c(TEST = "Parameter", RIND = "Reference range")
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST",
		colorVar = "RIND",
		labelVars = labelVars
	)
	
	gg <- plots[["1"]][[1]]
	
	expect_identical(gg$labels$title, "Parameter")
	expect_identical(unname(gg$labels$x), "DY")

	ggScales <- gg$scales$scales
	for(aes in c("colour", "fill", "shape")){
		
		expect_equal({
					
			isAes <- sapply(ggScales, function(x) 
				all(x[["aesthetics"]] == !!aes)
			)
			aesScale <- ggScales[[which(isAes)]]
			unname(aesScale$name)
					
		}, expected = "Reference range")
		
	}
			
})