context("Visualize subject profile as a line")

library(ggplot2)

test_that("Subject profile plots are correctly sorted in the output based on the levels of the subject ID variable", {
			
	data <- data.frame(
		TEST = c("A", "B", "C"),
		AVAL = rnorm(3),
		DY = c(1, 2, 3),
		SUBJID = factor(c("a", "b", "a"), levels = c("b", "a"))
	)
			
	plots <- subjectProfileLinePlot(
		data = data, 
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
		subjectVar = "SUBJID"
	)
			
	# plots are sorted based on factor levels:
	expect_named(plots, levels(data$SUBJID))
			
})

test_that("An error is generated if the subject variable is not present in the data", {
			
	data <- data.frame(
		TEST = c("A", "B", "C"),
		AVAL = rnorm(3),
		DY = c(1, 2, 3)
	)
	expect_error(
		subjectProfileLinePlot(
			data = data, 
			timeVar = "DY",
			paramNameVar = "TEST",
			paramValueVar = "AVAL",
		),
		"Variable.*not available in the data"
	)
			
})

test_that("Parameter variables and values are correctly displayed for each subject", {
			
	data <- data.frame(
		TEST = factor(rep(c("A", "B"), each = 5), levels = c("B", "A")),
		AVAL = rnorm(10),
		DY = sample.int(10),
		USUBJID = factor(rep(c("a", "b"), length = 10, replace = TRUE))
	)
			
	plots <- subjectProfileLinePlot(
		data = data, 
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
	)
			
	# test data is retained
	for(subjID in unique(data$USUBJID)){
				
		# check that the sublist is a list of ggplot object
		expect_type(plots[[subjID]], "list")
		expect_length(plots[[subjID]], 1)
		expect_s3_class(plots[[subjID]][[1]], c("subjectProfileLinePlot", "ggplot"))
			
		for(aes in c("Point", "Line")){
		
			expect_equal(
				object = {		
							
					gg <- plots[[subjID]][[1]]
					
					# extract data behind the aesthetic
					geomAes <- paste0("Geom", aes)
					isGeomAes <- sapply(gg$layers, function(l) inherits(l$geom, geomAes))
					ggDataAes <- layer_data(gg, which(isGeomAes))
					ggDataAes$PANEL <- as.character(ggDataAes$PANEL)
					ggDataAes <- ggDataAes[, c("PANEL", "x", "y")]
					ggDataAes[do.call(order, ggDataAes), ]
								
				},
				expected = {
					dataReference <- subset(data, USUBJID == subjID)
					dataReference$PANEL <- as.character(as.numeric(dataReference$TEST))
					dataReference <- setNames(
						dataReference[, c("PANEL", "DY", "AVAL")], 
						c("PANEL", "x", "y")
					)
					dataReference[do.call(order, dataReference), ]
				},
				check.attributes = FALSE # (rownames differ),
			)
			
		}
				
	}
			
})

test_that("Multiple parameter variables are correctly combined and ordered", {
			
	# example where variables are specified as factor
	# in this case variables are ordered based on factor levels
	dataFactor <- data.frame(
		CAT = factor(c("A", "A", "A", "B"), levels = c("B", "A")),
		TEST = factor(c("a1", "a2", "a3", "b1"), levels = c("a2", "a3", "a1", "b1")),
		DY = sample.int(4),
		USUBJID = "1",
		AVAL = rnorm(4)
	)
			
	# example with character vector
	# in this case standard R ordering (alphabetical) is used
	dataCharacter <- dataFactor
	dataCharacter[, c("CAT", "TEST")] <- lapply(dataCharacter[, c("CAT", "TEST")], as.character)
			
	dataList <- list(factor = dataFactor, character = dataCharacter)
			
	for(type in names(dataList)){
				
		expect_equal(
				
			object = {
					
				plots <- subjectProfileLinePlot(
					data = dataList[[!!type]],
					paramNameVar = c("CAT", "TEST"),
					paramValueVar = "AVAL",
					timeVar = "DY"
				)
							
				gg <- plots[[1]][[1]]
							
				# extract data behind the point
				isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
				ggDataPoint <- layer_data(gg, which(isGeomPoint))
				ggDataPoint$PANEL <- as.character(ggDataPoint$PANEL)
				ggDataPoint <- ggDataPoint[order(ggDataPoint$PANEL), ]
				ggDataPoint[, c("PANEL", "x", "y")]
				
			}, expected = {
							
				data <- dataList[[!!type]]
				data <- data[with(data, order(CAT, TEST)), ]
				data$PANEL <- as.character(seq.int(nrow(data)))
				
				data[, c("PANEL", "DY", "AVAL")]
							
			},
			check.attributes = FALSE
		)
		
	}
			
})

test_that("Parameter values are correctly combined with a specified separator", {
			
	data <- data.frame(
		CAT = factor(c("A", "A", "A", "B"), levels = c("B", "A")),
		TEST = factor(c("a1", "a2", "a3", "b1"), levels = c("a2", "a3", "a1", "b1")),
		DY = c(1, 2, 3, 4),
		USUBJID = "1",
		AVAL = rnorm(4)
	)
	plots <- subjectProfileLinePlot(
		data = data,
		paramNameVar = c("CAT", "TEST"),
		paramVarSep = " and ",
		paramValueVar = "AVAL",
		timeVar = "DY"
	)
	gg <- plots[["1"]][[1]]
	
	# extract labels for the different facets
	ggGrob <- ggplotGrob(gg)
	ggGrobFacets <- ggGrob$grobs[grep("^strip", ggGrob$layout$name)]
	facetLabs <- sapply(ggGrobFacets, function(ggGrob) {
		ggGrobFacetChild <- ggGrob$grobs[[1]]$children
		ggGrobFacetTitle <- ggGrobFacetChild[[which(sapply(ggGrobFacetChild, inherits, "titleGrob"))]]
		sapply(ggGrobFacetTitle$children, "[[", "label")	
	})
	facetLabs <- unname(facetLabs)
	
	# build parameter labels from data
	dataReference <- data[with(data, order(CAT, TEST)), ]
	dataReference$yLabel <- with(dataReference, paste(CAT, TEST, sep = " and "))
	
	expect_equal(facetLabs, dataReference$yLabel)
	
})



test_that("Specified labels for parameter variables are correctly set", {
			
	data <- data.frame(
		CAT = "A", TEST = "a1",
		DY = 1,
		USUBJID = "1",
		AVAL = 1
	)
			
	expect_equal({
		plots <- subjectProfileLinePlot(
			data = data,
			paramNameVar = c("CAT", "TEST"),
			paramValueVar = "AVAL",
			timeVar = "DY",
		)
		gg <- plots[[1]][[1]]
		gg$labels$title
	}, expected = "AVAL")
			
	expect_equal({
		plots <- subjectProfileLinePlot(
			data = data,
			paramNameVar = c("CAT", "TEST"),
			paramValueVar = "AVAL",
			timeVar = "DY",
			paramLab = c("Laboratory parameter")
		)
		gg <- plots[[1]][[1]]
		gg$labels$title
	}, expected = "Laboratory parameter")
			
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
		AVAL = rnorm(4),
		USUBJID = "1"
	)
			
	plots <- subjectProfileLinePlot(
		data = data,
		paramNameVar = "TEST",
		paramGroupVar = c("CAT1", "CAT2"),
		paramValueVar = c("AVAL"),
		timeVar = "DY"
	)
			
	gg <- plots[["1"]][[1]]
			
	# extract labels for the different facets
	ggGrob <- ggplotGrob(gg)
	ggGrobFacets <- ggGrob$grobs[grep("^strip", ggGrob$layout$name)]
	facetLabs <- sapply(ggGrobFacets, function(ggGrob) {
		ggGrobFacetChild <- ggGrob$grobs[[1]]$children
		ggGrobFacetTitle <- ggGrobFacetChild[[which(sapply(ggGrobFacetChild, inherits, "titleGrob"))]]
		sapply(ggGrobFacetTitle$children, "[[", "label")	
	})
	facetLabs <- unname(facetLabs)
		
	dataReference <- data[with(data, order(CAT1, CAT2, TEST)), ]
	dataReference$TEST <- as.character(dataReference$TEST)
			
	expect_equal(facetLabs, dataReference$TEST)
			
})

test_that("The reference ranges are correctly displayed", {
	
	# Parameter A: reference range outside data range
	# Parameter B: reference range inside data range
	# Parameter C: missing reference range
	# Parameter D: missing y-values
	data <- data.frame(
		TEST = c("A", "A", "B", "B", "C", "C", "D"),
		DY = seq(7),
		USUBJID = "1",
		AVAL = c(1, 2, 3, 4, 5, 7, NA_real_),
		LOW = c(0, 0, 3.5, 3.5, NA_real_, NA_real_, NA_real_),
		HIGH = c(4, 4, 4, 4, NA_real_, NA_real_, NA_real_)
	)
	
	# error if only one variable is specified:
	expect_error(
		plots <- subjectProfileLinePlot(
			data = data,
			timeVar = "DY",
			paramNameVar = "TEST",
			paramValueVar = "AVAL",
			paramValueRangeVar = c("LOW")
		),
		"'paramValueRangeVar' should be of length 2."
	)
	
	# error if some variable(s) are not in the data
	expect_error(
		subjectProfileLinePlot(
			data = data,
			timeVar = "DY",
			paramNameVar = "TEST",
			paramValueVar = "AVAL",
			paramValueRangeVar = c("LOW", "HIGH2")
		),
		"HIGH2.* are not available in the data"
	)
	
	# correct specification
	expect_silent(
		plots <- subjectProfileLinePlot(
			data = data,
			timeVar = "DY",
			paramNameVar = "TEST",
			paramValueVar = "AVAL",
			paramValueRangeVar = c("LOW", "HIGH")
		)
	)
	gg <- plots[[1]][[1]]
	
	isGeomRibbon <- sapply(gg$layers, function(l) inherits(l$geom, "GeomRibbon"))
	ggDataRibbon <- layer_data(gg, which(isGeomRibbon))
	ggDataRibbon$PANEL <- as.character(ggDataRibbon$PANEL)
	
	dataRefRibbon <- subset(data, !is.na(LOW) & !is.na(HIGH))
	dataRefRibbon$PANEL <- as.character(as.numeric(as.factor(dataRefRibbon$TEST)))
	expect_equal(
		ggDataRibbon[, c("PANEL", "x", "ymin", "ymax")],
		dataRefRibbon[, c("PANEL", "DY", "LOW", "HIGH")], 
		check.attributes = FALSE # colnames differ
	)
	
	expect_setequal(ggDataRibbon$colour, NA)
	expect_false(any(is.na(ggDataRibbon$fill)))
	
})

test_that("Limits for the y-axis are correctly restricted to the observation range", {
			
	# Parameter A: reference range outside data range
	# Parameter B: reference range inside data range
	# Parameter C: missing reference range
	# Parameter D: missing y-values
	data <- data.frame(
		TEST = c("A", "A", "B", "B", "C", "C", "D"),
		DY = seq(7),
		USUBJID = "1",
		AVAL = c(1, 2, 3, 4, 5, 7, NA_real_),
		LOW = c(0, 0, 3.5, 3.5, NA_real_, NA_real_, NA_real_),
		HIGH = c(4, 4, 4, 4, NA_real_, NA_real_, NA_real_)
	)
			
	expect_silent(
		plots <- subjectProfileLinePlot(
			data = data,
			timeVar = "DY",
			paramNameVar = "TEST",
			paramValueVar = "AVAL",
			paramValueRangeVar = c("LOW", "HIGH"),
			yLimFrom = c("value")
		)
	)
	gg <- plots[[1]][[1]]
			
	isGeomRibbon <- sapply(gg$layers, function(l) inherits(l$geom, "GeomRibbon"))
	ggDataRibbon <- layer_data(gg, which(isGeomRibbon))
	ggDataRibbon$PANEL <- as.character(ggDataRibbon$PANEL)
			
	dataRefRibbon <- data.frame(
		PANEL = c("1", "1", "2", "2"),
		x = c(1, 2, 3, 4),
		ymin = c(1, 1, 3.5, 3.5),
		ymax = c(2, 2, 4, 4),
		stringsAsFactors = FALSE
	)
	expect_equal(
		ggDataRibbon[, c("PANEL", "x", "ymin", "ymax")],
		dataRefRibbon, 
		check.attributes = FALSE # colnames differ
	)
			
})

test_that("A custom color is correctly set for the reference range", {
			
	data <- data.frame(
		TEST = c("A", "A"),
		DY = seq(2),
		USUBJID = "1",
		AVAL = c(1, 2),
		LOW = c(0, 0),
		HIGH = c(4, 4)
	)
			
	colorValueRange <- "orange"
	expect_silent(
		plots <- subjectProfileLinePlot(
			data = data,
			timeVar = "DY",
			paramNameVar = "TEST",
			paramValueVar = "AVAL",
			paramValueRangeVar = c("LOW", "HIGH"),
			colorValueRange = colorValueRange
		)
	)
	
	gg <- plots[[1]][[1]]
	
	isGeomRibbon <- sapply(gg$layers, function(l) inherits(l$geom, "GeomRibbon"))
	ggDataRibbon <- layer_data(gg, which(isGeomRibbon))

	expect_setequal(ggDataRibbon$fill, "orange")
	expect_setequal(ggDataRibbon$colour, NA)
	
})

test_that("Data points are correctly colored based on a specified variable", {
			
	data <- data.frame(
		TEST = c("A", "A", "B"),
		DY = seq(3),
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		AVAL = rnorm(3),
		USUBJID = "1"
	)
			
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
		colorVar = "RIND"
	)
			
	gg <- plots[["1"]][[1]]
	
	## point
			
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	ggDataPoint$PANEL <- as.character(ggDataPoint$PANEL)
	
	# format reference data
	dataReference <- data
	dataReference$PANEL <- as.character(as.numeric(as.factor(dataReference$TEST)))
	# missing levels are not displayed
	dataReference$RIND <- droplevels(dataReference$RIND)
	
	ggDataPointWithInput <- merge(
		x = ggDataPoint, by.x = c("PANEL", "x", "y"),
		y = dataReference, by.y = c("PANEL", "DY", "AVAL"),
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
	
	## line: colour only used for the points, not the line
	
	# extract data behind the line
	isGeomLine <- sapply(gg$layers, function(l) inherits(l$geom, "GeomLine"))
	ggDataLine <- layer_data(gg, which(isGeomLine))
	expect_setequal(ggDataLine$colour, "black")
	
})

test_that("Data points are correctly colored with a specified palette", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		AVAL = rnorm(3),
		USUBJID = "1"
	)
			
	colorPalette <- c(Low = "green", Normal = "blue", High = "red")
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
		colorVar = "RIND", colorPalette = colorPalette
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
		AVAL = rnorm(3),
		USUBJID = "1"
	)
	
	colorLab <- "Reference indicator"
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
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
		AVAL = rnorm(3),
		USUBJID = "1"
	)

	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
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
		AVAL = rnorm(3),
		USUBJID = "1"
	)
			
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
		shapeVar = "RIND"
	)
			
	gg <- plots[["1"]][[1]]
			
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
			
	# format reference data
	dataReference <- data
	dataReference$PANEL <- as.character(dataReference$TEST)
	# missing levels are not displayed
	dataReference$RIND <- droplevels(dataReference$RIND)
			
	ggDataPointWithInput <- merge(
		x = ggDataPoint, by.x = c("PANEL", "x", "y"),
		y = dataReference, by.y = c("PANEL", "DY", "AVAL"),
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
		AVAL = rnorm(3),
		USUBJID = "1"
	)
			
	shapePalette <- c(Low = 25, Normal = 19, High = 24)
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
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
		RIND = factor(
			c("High", "Normal", "High"), 
			levels = c("Low", "Normal", "High")
		),
		AVAL = rnorm(3),
		USUBJID = "1"
	)
			
	shapeLab <- "Reference indicator"
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
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

test_that("The shape symbols are correctly set to a specific size", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		RIND = c("High", "Normal", "High"),
		AVAL = rnorm(3),
		USUBJID = "1"
	)
	
	shapeSize <- 10
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
		shapeSize = shapeSize
	)
	gg <- plots[["1"]][[1]]
	
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	expect_setequal(ggDataPoint$size, shapeSize)
	
})

test_that("Data points are correctly set transparent", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1",
		AVAL = rnorm(3)
	)
	
	alpha <- 0.3
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
		alpha = alpha
	)
	gg <- plots[["1"]][[1]]
	
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	
	expect_setequal(ggDataPoint$alpha, alpha)
	
})

test_that("A title is correctly set", {
	
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1",
		AVAL = rnorm(3)
	)
	
	timeLab <- "Relative day of the study"
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		timeLab = timeLab,
		paramNameVar = "TEST",
		paramValueVar = "AVAL"
	)
	
	gg <- plots[["1"]][[1]]
	
	# by default used as label for the x-axis
	expect_equal(gg$labels$x, timeLab)
	
})

test_that("A transformation is correctly applied on the time variable", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = c(1, 10, 100),
		USUBJID = "1",
		AVAL = rnorm(3)
	)
			
	timeTrans <- scales::log10_trans()
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		timeTrans = timeTrans,
		paramNameVar = "TEST",
		paramValueVar = "AVAL"
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
		USUBJID = "1",
		AVAL = rnorm(3)
	)
			
	timeExpand <- expansion(mult = 0, add = 3)
			
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		timeExpand = timeExpand,
		paramNameVar = "TEST",
		paramValueVar = "AVAL"
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
		USUBJID = "1",
		AVAL = rnorm(3)
	)
			
	timeLim <- c(2, 3)
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		timeLim = timeLim,
		paramNameVar = "TEST",
		paramValueVar = "AVAL"
	)
			
	gg <- plots[["1"]][[1]]
			
	expect_identical(gg$coordinates$limits$x, timeLim)
	
	expect_identical(attr(plots, "metaData")$timeLim, timeLim)
		
})

test_that("A label for the variable on the x-axis is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1",
		AVAL = rnorm(3)
	)
			
	xLab <- "Relative day of the study"
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		xLab = xLab,
		paramNameVar = "TEST",
		paramValueVar = "AVAL"
	)
			
	gg <- plots[["1"]][[1]]
	
	expect_identical(gg$labels$x, xLab)
	
})

test_that("A label for the variable on the y-axis is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1",
		AVAL = rnorm(3)
	)
	
	yLab <- "Parameter of interest"
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		yLab = yLab,
		paramNameVar = "TEST",
		paramValueVar = "AVAL"
	)
	
	gg <- plots[["1"]][[1]]
	
	expect_identical(gg$labels$y, yLab)
	
})

test_that("A title is correctly set", {
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1",
		AVAL = rnorm(3)
	)
	title <- "Laboratory parameters"
	
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		title = title,
		paramNameVar = "TEST",
		paramValueVar = "AVAL"
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
		USUBJID = "1",
		AVAL = rnorm(3)
	)
	label <- "laboratory information"
	
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		label = label,
		paramNameVar = "TEST",
		paramValueVar = "AVAL"
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
		USUBJID = "1",
		AVAL = rnorm(3)
	)
	
	# label specified for a subset of the variable(s)
	labelVars <- c(DY = "Relative time", RIND = "Reference range")
	plots <- subjectProfileLinePlot(
		data = data,
		timeVar = "DY",
		paramNameVar = "TEST",
		paramValueVar = "AVAL",
		colorVar = "RIND",
		labelVars = labelVars
	)
	
	gg <- plots[["1"]][[1]]
	
	expect_identical(gg$labels$title, "AVAL")
	expect_identical(unname(gg$labels$x), "Relative time")
	
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