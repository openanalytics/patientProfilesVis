context("Visualize subject profile as a text")

library(ggplot2)
library(gtable)

test_that("Subject profile plots are correctly sorted in the output based on the levels of the subject ID variable", {
			
	data <- data.frame(
		SEX = c("F", "M", "F"),
		SUBJID = factor(c("a", "b", "c"), levels = c("b", "a", "c"))
	)
			
	plots <- subjectProfileTextPlot(
		data = data, 
		paramValueVar = "SEX",
		subjectVar = "SUBJID"
	)
			
	# plots are sorted based on factor levels:
	expect_named(plots, levels(data$SUBJID))
			
})

test_that("An error is generated if the subject variable is not present in the data", {
			
	data <- data.frame(SEX = c("F", "M", "F"))
	expect_error(
		subjectProfileTextPlot(data = data, paramValueVar = "SEX"),
		"Variable.*not available in the data"
	)
			
})

test_that("Parameter variables are correctly displayed for each subject", {
			
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
	for(subjID in unique(data$USUBJID)){
		
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

test_that("Parameter variables are correctly combined", {
			
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
	expect_equal(yLabel, c("WEIGHT, AGE, SEX"))
		
})

test_that("Parameter values are correctly combined with a specified separator", {
			
	data <- data.frame(
		SEX = "M", AGE = NA_character_,
		WEIGHT = 40, USUBJID = "1"
	)
	plots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "WEIGHT|AGE|SEX",
		paramVarSep = " and "
	)
	gg <- plots[["1"]][[1]]
			
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	ggDataText <- ggDataText[order(ggDataText$y), ]
			
	yValue <- as.character(ggDataText[, "label"])
	expect_equal(yValue, c("40 and NA and M"))
			
})

test_that("Specified labels for parameter variables are correctly set", {
			
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

test_that("Parameter variables and names are correctly displayed", {
			
	# example with multiple records
	# for the same label
	data <- data.frame(
		CAT = "A",
		TERM = c("a", "b"),
		END = c(NA_character_, "03/2020"),
		START = c("01/2020", "02/2020"),
		USUBJID = "1"
	)
			
	plots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = c("TERM", "START", "END"),
		paramNameVar = "CAT"
	)
	
	gg <- plots[["1"]][[1]]
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	ggDataText <- ggDataText[order(ggDataText$y), ]
	
	yValue <- as.character(ggDataText[, "label"])
	expect_equal(yValue, c("a - 01/2020 - NA, b - 02/2020 - 03/2020"))
	
	yLabel <- layer_scales(gg, which(isGeomText))$y$range$range
	expect_equal(yLabel, "A")
	
})

test_that("An error is generated if there is more than one parameter name", {
			
	data <- data.frame(
		CAT = "A",
		TERM = c("a", "b"),
		START = c("01/2020", "02/2020"),
		USUBJID = "1"
	)
			
	expect_error(
		plots <- subjectProfileTextPlot(
			data = data,
			paramValueVar = "START",
			paramNameVar = c("CAT", "TERM")
		),
		"'paramNameVar' should be of length 1"
	)		
})

test_that("A specified parameter value function correctly returns a new variable with parameter values", {
			
	# example with multiple records
	# for the same label
	data <- data.frame(
		TERM = factor(c("a", "b"), levels = c("b", "a")),
		END = c(NA_character_, "03/2020"),
		START = c("01/2020", "02/2020"),
		USUBJID = "1"
	)
			
	paramValueVarFct <- function(data){
		with(data, paste0("[", START, ", ", END, "]"))
	}
	plots <- subjectProfileTextPlot(
		data = data,
		paramNameVar = "TERM",
		paramValueVar = paramValueVarFct
	)
			
	gg <- plots[["1"]][[1]]
			
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	ggDataText <- ggDataText[order(ggDataText$y), ]
			
	yValue <- as.character(ggDataText[, "label"])
	expect_equal(
		rev(yValue), 
		paramValueVarFct(data[order(data$TERM), ])
	)
			
	yLabel <- layer_scales(gg, which(isGeomText))$y$range$range
	expect_equal(rev(yLabel), levels(data$TERM))
			
})

test_that("Parameter variables are correctly represented in a table format", {
			
	# example with multiple records for the same subject
	data <- data.frame(
		CAT = "A",
		TERM = factor(c("a", "b"), levels = c("b", "a")),
		END = c(NA_character_, "03/2020"),
		START = c("01/2020", "02/2020"),
		USUBJID = "1"
	)
	
	paramValueVar <- c("CAT", "TERM", "START", "END")
	expect_silent(
		plots <- subjectProfileTextPlot(
			data = data,
			paramValueVar = paramValueVar,
			table = TRUE
		)
	)
	gg <- plots[["1"]][[1]]
	
	# extract table defined with: 'annotation_custom'
	isGeomTable <- sapply(gg$layers, function(l) inherits(l$geom, "GeomCustomAnn"))
	
	# table is defined as gtable
	gTable <- layer_grob(gg, which(isGeomTable))[[1]]
	gTable <- gtable::gtable_filter(gTable, "fg")# filter background(=bg) elements
	gTableLabels <- sapply(gTable$grobs, "[[", "label") # plot labels
	gTableCoord <- gTable$layout[, c("t", "l")] # coordinates
	
	# format plot data
	ggDataLong <- cbind.data.frame(gTableCoord, label = gTableLabels)
	ggDataLong <- ggDataLong[with(ggDataLong, order(t, l)), ]
	ggData <- matrix(
		data = subset(ggDataLong, t != 1)$label, 
		nrow = max(ggDataLong$t)-1, ncol = max(ggDataLong$l),
		byrow = TRUE,
		dimnames = list(NULL, subset(ggDataLong, t == 1)$label) # header
	)
	ggData <- as.data.frame(ggData, stringsAsFactors = FALSE)
	
	dataOrder <- data[with(data, order(CAT, TERM, START, END)), ]
	dataOrder[, ] <- lapply(dataOrder, as.character)
	
	expect_equal(
		object = ggData,
		expected = dataOrder[, paramValueVar],
		check.attributes = FALSE
	)
	
})

test_that("Parameter values are correctly ordered/grouped based on grouping variables", {
			
	# example where data is first sorted based on multiple
	# grouping variables (factor and character),
	# then param name variable (for a2 vs a1)
	data <- data.frame(
		CAT1 = c("I", "I", "I", "II"),
		CAT2 = factor(c("A", "A", "B", "A"), levels = c("B", "A")),
		TERM = factor(c("a1", "a2", "b", "a3"), levels = c("a2", "a1", "a3", "b")),
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = "1",
		stringsAsFactors = FALSE
	)
			
	plots <- subjectProfileTextPlot(
		data = data,
		paramNameVar = "TERM",
		paramValueVar = "START",
		paramGroupVar = c("CAT1", "CAT2")
	)
	
	gg <- plots[["1"]][[1]]
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	ggDataText <- ggDataText[order(ggDataText$y), ]
	
	dataReference <- data[with(data, order(CAT1, CAT2, TERM)), ]
	dataReference$TERM <- as.character(dataReference$TERM)
	
	yValue <- as.character(ggDataText[, "label"])
	expect_equal(rev(yValue), dataReference$START)
	
	yLabel <- layer_scales(gg, which(isGeomText))$y$range$range
	# labels are indicated from the bottom to the top of the plot
	yLabel <- rev(yLabel)
	expect_equal(yLabel, dataReference$TERM)
			
})

test_that("Parameter values in a table format are correctly ordered/grouped based on grouping variables", {

	data <- data.frame(
		CAT1 = c("I", "I", "I", "II"),
		CAT2 = factor(c("A", "A", "B", "A"), levels = c("B", "A")),
		TERM = factor(c("a1", "a2", "b", "a3"), levels = c("a2", "a1", "a3", "b")),
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = "1",
		stringsAsFactors = FALSE
	)
			
	plots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = c("TERM", "START"),
		paramGroupVar = c("CAT1", "CAT2"),
		table = TRUE
	)
	gg <- plots[["1"]][[1]]
			
	# extract table defined with: 'annotation_custom'
	isGeomTable <- sapply(gg$layers, function(l) inherits(l$geom, "GeomCustomAnn"))
			
	# table is defined as gtable
	gTable <- layer_grob(gg, which(isGeomTable))[[1]]
	gTable <- gtable::gtable_filter(gTable, "fg")# filter background(=bg) elements
	gTableLabels <- sapply(gTable$grobs, "[[", "label") # plot labels
	gTableCoord <- gTable$layout[, c("t", "l")] # coordinates
			
	# format plot data
	ggDataLong <- cbind.data.frame(gTableCoord, label = gTableLabels)
	ggDataLong <- ggDataLong[with(ggDataLong, order(t, l)), ]
	ggData <- matrix(
		data = subset(ggDataLong, t != 1)$label, 
		nrow = max(ggDataLong$t)-1, ncol = max(ggDataLong$l),
		byrow = TRUE,
		dimnames = list(NULL, subset(ggDataLong, t == 1)$label) # header
	)
	
	dataOrder <- data[with(data, order(CAT1, CAT2, TERM, START)), ]
	dataOrder <- as.matrix(dataOrder)
	
	expect_equal(
		object = ggData,
		expected = dataOrder[, c("TERM", "START")],
		check.attributes = FALSE
	)
	
})

test_that("A label for the variable on the x-axis is correctly set", {
			
	data <- data.frame(ARM = "A", USUBJID = "1")
	xLab <- "ARM: Planned treatment arm"
	
	plots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "ARM",
		xLab = xLab
	)
	
	expect_identical(
		object = plots[[1]][[1]]$labels$x, 
		expected = xLab
	)
			
})

test_that("A label for the variable on the y-axis is correctly set", {
			
	data <- data.frame(ARM = "A", USUBJID = "1")
	yLab <- "Demographic variable"
	
	plots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "ARM",
		yLab = yLab
	)
	
	expect_identical(
		object = plots[[1]][[1]]$labels$y, 
		expected = yLab
	)
			
})

test_that("A title is correctly set", {
			
	data <- data.frame(ARM = "A", USUBJID = "1")
	title <- "Demographic information"
	
	plots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "ARM",
		title = title
	)
	
	expect_identical(
		object = plots[[1]][[1]]$labels$title, 
		expected = title
	)
			
})

test_that("Labels for aesthetic, plot or axis title are correctly extracted from the specified variable labels", {
			
	data <- data.frame(
		SEX = "F",
		AGE = 40,
		ARM = "A",
		USUBJID = "1"
	)
	
	# label specified for a subset of the variable(s)
	labelVars <- c(AGE = "Age (years)", ARM = "Treatment")
	expect_equal(
		object = {
			plots <- subjectProfileTextPlot(
				data = data,
				paramValueVar = c("SEX", "AGE", "ARM"),
				labelVars = labelVars
			)
			gg <- plots[[1]][[1]]
			isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
			yLabel <- layer_scales(gg, which(isGeomText))$y$range$range
			rev(yLabel) # labels returned from bottom to the top of the plot
		},
		expected = c("SEX", "Age (years)", "Treatment")
	)		
	
	# in case variables are combined
	expect_equal(
		object = {
			plots <- subjectProfileTextPlot(
				data = data,
				paramValueVar = "SEX|AGE|ARM",
				labelVars = labelVars
			)
			gg <- plots[[1]][[1]]
			isGeomText <- sapply(gg$layers, function(l) 
				inherits(l$geom, "GeomText"))
			layer_scales(gg, which(isGeomText))$y$range$range
		},
		expected = c("SEX, Age (years), Treatment")
	)
			
})

test_that("A label for the metadata of the subject profile plots is correctly set", {
			
	data <- data.frame(ARM = "A", USUBJID = "1")
	label <- "demographic information"
	expect_identical(
		object = {
			plots <- subjectProfileTextPlot(
				data = data,
				paramValueVar = "ARM",
				label = label
			)
			attr(plots, "metaData")$label
		}, 
		expected = label
	)
			
})

test_that("Variables that are too long to fit in one table column will span multiple lines", {

	data <- data.frame(
		AEDECOD = paste(sample(LETTERS, 500, replace = TRUE), collapse = " "),
		USUBJID = "1"
	)

	listPlots <- subjectProfileTextPlot(
		data = data,
		paramValueVar = "AEDECOD",
		table = TRUE
	)
	
	gg <- listPlots[["1"]][[1]]
	isGeomTable <- sapply(gg$layers, function(l) inherits(l$geom, "GeomCustomAnn"))
	
	# table is defined as gtable
	gTable <- layer_grob(gg, which(isGeomTable))[[1]]
	gTable <- gtable::gtable_filter(gTable, "fg")# filter background(=bg) elements
	gTableLabels <- sapply(gTable$grobs, "[[", "label") # plot labels
	
	# remove table header with variable name:
	gTableLabelsCnt <- setdiff(gTableLabels, "AEDECOD")
	
	# check that values span multiple lines
	expect_match(gTableLabelsCnt, regexp = "\n", fixed = TRUE)
			
})

test_that("Visualizations correctly span multiple pages", {
			
	data <- data.frame(
		AEDECOD = sample(LETTERS, 100, replace = TRUE), 
		USUBJID = "1"
	)
	
	expect_gte(
		object = {
			plots <- subjectProfileTextPlot(
				data = data,
				paramValueVar = "AEDECOD",
				table = TRUE,
				paging = TRUE
			)
			length(plots[[1]])
		}, 
		1
	)
	
	expect_equal(
		object = {
			plots <- subjectProfileTextPlot(
				data = data,
				paramValueVar = "AEDECOD",
				table = TRUE,
				paging = FALSE
			)
			length(plots[[1]])
		}, 
		1
	)
			
})

test_that("The widths of the table columns are correctly set", {
			
	data <- data.frame(
		CAT = paste(as.character(seq_len(100)), collapse = " "),
		TERM = paste(as.character(seq_len(40)), collapse = " "),
		USUBJID = "1"
	)
	
	paramValueVar <- c("CAT", "TERM")
	colWidth <- c(0.9, 0.1)
	expect_silent(
		plots <- subjectProfileTextPlot(
			data = data,
			paramValueVar = paramValueVar,
			table = TRUE, colWidth = colWidth
		)
	)
	gg <- plots[["1"]][[1]]
	
	# extract table defined with: 'annotation_custom'
	isGeomTable <- sapply(gg$layers, function(l) inherits(l$geom, "GeomCustomAnn"))

	# table is defined as gtable
	gTable <- layer_grob(gg, which(isGeomTable))[[1]]
	gTable <- gtable::gtable_filter(gTable, "fg")# filter background(=bg) elements
	gTableLabels <- sapply(gTable$grobs, "[[", "label") # plot labels
	gTableLabels <- setdiff(gTableLabels, paramValueVar)
	
	# extract max number of characters per line for each column:
	gTableLabelsByLine <- strsplit(gTableLabels, split = "\n")
	gTableMaxNChar <- sapply(gTableLabelsByLine, function(x) max(nchar(x)))
	
	# check that column proportion in plot is as specified:
	expect_equal(
		round(gTableMaxNChar[2]/gTableMaxNChar[1], 1), 
		round(colWidth[2]/colWidth[1], 1)
	)
	
})