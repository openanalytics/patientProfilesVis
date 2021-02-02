context("Visualize subject profile event")

library(ggplot2)

test_that("subject variable is not present in the data", {
			
	data <- data.frame(
		LBTEST = c("A", "B", "C"),
		LBDY = c(1, 2, 3)
	)
	expect_error(
		subjectProfileEventPlot(
			data = data, 
			timeVar = "LBDY",
			paramVar = "LBTEST"
		),
		"Variable.*not available in the data"
	)
			
})

test_that("subject variable order is retained", {
			
	data <- data.frame(
		LBTEST = c("A", "B", "C"),
		LBDY = c(1, 2, 3),
		USUBJID = factor(c("1", "2", "1"), levels = c("2", "1"))
	)
			
	expect_silent(
		plots <- subjectProfileEventPlot(
			data = data,
			paramVar = "LBTEST",
			timeVar = "LBDY"
		)
	)
			
	expect_named(plots, levels(data$USUBJID))
		
})


test_that("parameter variables are correctly displayed for each subject", {
			
	data <- data.frame(
		LBTEST = factor(c("A", "B", "C"), levels = c("B", "C", "A")),
		LBDY = c(1, 2, 3),
		USUBJID = factor(c("1", "2", "1"), levels = c("2", "1"))
	)
	
	plots <- subjectProfileEventPlot(
		data = data,
		paramVar = "LBTEST",
		timeVar = "LBDY"
	)
	
	# test data is retained
	for(subjID in names(plots)){
		
		# check that the sublist is a list of ggplot object
		expect_type(plots[[!!subjID]], "list")
		expect_length(plots[[!!subjID]], 1)
		expect_s3_class(plots[[!!subjID]][[1]], c("subjectProfileEventPlot", "ggplot"))
		
		expect_equal(
			object = {		
				
				gg <- plots[[!!subjID]][[1]]	
				# extract data behind the text
				isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
				ggDataPoint <- layer_data(gg, which(isGeomPoint))
				xCoord <- ggDataPoint[order(ggDataPoint$y), "x"]
					
				# extract labels of the y-axis
				yLabel <- layer_scales(gg, which(isGeomPoint))$y$range$range
					
				# variables are order from the bottom to the top in the data
				# so use revert order
				dataPlot <- data.frame(x = xCoord, y = yLabel)
				
			},
			expected = {
				dataReference <- subset(data, USUBJID == !!subjID)
				setNames(dataReference[, c("LBDY", "LBTEST")], c("x", "y"))
			},
			check.attributes = FALSE # (rownames differ)
		)		
		
	}
			
})

test_that("multiple parameter variables are correctly combined and ordered", {
		
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
		
	dataList <- list(data, data2)
	
	for(i in seq_along(dataList)){
		
		expect_equal(
				
			object = {
	
				plots <- subjectProfileEventPlot(
					data = dataList[[!!i]],
					paramVar = c("CAT", "TEST"),
					timeVar = "DY"
				)
					
				gg <- plots[[1]][[1]]
				
				# extract data behind the text
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

test_that("variable(s) of parameters are combined with specified separator", {
			
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
			
	# extract data behind the text
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	yLabel <- layer_scales(gg, which(isGeomPoint))$y$range$range
	yLabel <- rev(yLabel)
	
	dataReference <- data[with(data, order(CAT, TEST)), ]
	dataReference$yLabel <- with(dataReference, paste(CAT, TEST, sep = " and "))
	
	expect_equal(yLabel, dataReference$yLabel)
			
})

test_that("label(s) for parameter variable(s) are specified", {
			
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

test_that("parameters are grouped based on grouping variable(s)", {
			
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
	
	# extract data behind the point
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	ggDataPoint <- ggDataPoint[order(ggDataPoint$y), ]
	
	# extract labels of the y-axis
	ggDataPoint$yLabel <- layer_scales(gg, which(isGeomPoint))$y$range$range
	
	# variables are order from the bottom to the top in the data
	# so use revert order
	ggDataPointOrder <- ggDataPoint[order(ggDataPoint$y, decreasing = TRUE), ]					
	yLabel <- ggDataPointOrder$yLabel
	
	dataOrder <- data[with(data, order(CAT1, CAT2, TEST)), ]
			
	expect_equal(yLabel, as.character(dataOrder$TEST))
			
})