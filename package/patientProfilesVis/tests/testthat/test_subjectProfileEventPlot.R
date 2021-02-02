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