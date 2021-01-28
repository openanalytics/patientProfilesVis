context("Visualize subject profile as a text")

library(ggplot2)

test_that("subject variable is not present in the data", {
			
	data <- data.frame(SEX = c("F", "M", "F"))
	expect_error(
		subjectProfileTextPlot(data = data, paramValueVar = "SEX"),
		"Variable.*not available in the data"
	)
			
})

test_that("subject variable order is retained", {
			
	data <- data.frame(
		SEX = c("F", "M", "F"),
		USUBJID = factor(c("3", "2", "1"), levels = c("2", "3", "1"))
	)
			
	expect_silent(
		plots <- subjectProfileTextPlot(data = data, paramValueVar = "SEX")
	)
	
	expect_named(plots, levels(data$USUBJID))
			
})

test_that("parameter values are displayed", {
			
	data <- data.frame(
		SEX = c("F", "M", "F"),
		AGE = c(40, 46, NA_real_),
		ARM = factor(c("A", "B", "A")),
		USUBJID = factor(c("3", "2", "1"), levels = c("2", "3", "1"))
	)
	paramValueVar <- c("SEX", "AGE", "ARM")
	expect_silent(
		plots <- subjectProfileTextPlot(
			data = data,
			paramValueVar = paramValueVar
		)
	)
	expect_type(plots, "list")
	expect_named(plots, levels(data$USUBJID))
	
	# test data is retained
	for(subjID in names(plots)){
		
		# check that the sublist is a list of ggplot object
		expect_type(plots[[!!subjID]], "list")
		expect_length(plots[[!!subjID]], 1)
		expect_s3_class(plots[[!!subjID]][[1]], c("subjectProfileTextPlot", "ggplot"))
		
		expect_equal(
			object = {		
				
				gg <- plots[[!!subjID]][[1]]	
				# extract data behind the text
				isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
				ggDataText <- layer_data(gg, which(isGeomText))
				ggDataText <- ggDataText[order(ggDataText$y), ]
				yValue <- as.character(ggDataText[, "label"])
				
				# extract labels of the y-axis
				yLabel <- layer_scales(gg, which(isGeomText))$y$range$range
				
				# variables are order from the bottom to the top in the data
				# so use revert order
				setNames(rev(yValue), rev(yLabel))
				
			},
			expected = {
				dataReference <- subset(data, USUBJID == !!subjID)[, paramValueVar]
				setNames(as.character(paste(t(dataReference))), paramValueVar)
			}
		)		
		
	}
		
})

test_that("parameter values are combined", {
			
	data <- data.frame(
		SEX = "M", AGE = NA_character_,
		WEIGHT = 40, USUBJID = "1"
	)
	plots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "WEIGHT|AGE|SEX"
	)
	gg <- plots[["1"]][[1]]
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	ggDataText <- ggDataText[order(ggDataText$y), ]
	
	yValue <- as.character(ggDataText[, "label"])
	expect_equal(yValue, c("40 - NA - M"))
	
	yLabel <- layer_scales(gg, which(isGeomText))$y$range$range
	expect_equal(yLabel, c("WEIGHT|AGE|SEX"))
		
})

test_that("label for parameters are specified", {
			
	data <- data.frame(
		SEX = "M", AGE = NA_character_,
		WEIGHT = 40, USUBJID = "1"
	)
	paramValueLab <- c(AGE = "Age (years)", WEIGHT = "Weight (kg)")
	plots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "WEIGHT|AGE|SEX",
		paramValueLab = paramValueLab
	)
	
	gg <- plots[["1"]][[1]]
			
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	ggDataText <- ggDataText[order(ggDataText$y), ]
			
	yValue <- as.character(ggDataText[, "label"])
	expect_equal(yValue, c("40 - NA - M"))
			
	yLabel <- layer_scales(gg, which(isGeomText))$y$range$range
	expect_equal(yLabel, c("Weight (kg), Age (years), SEX"))
			
})