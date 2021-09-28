context("Test miscellaneous functions")

test_that("The path of the subject profiles is correctly retrieved", {

	template <- "subjectProfile.Rnw"
	pathTemplate <- patientProfilesVis:::getPathTemplate(template)
	expect_true(file.exists(pathTemplate))
	expect_equal(basename(pathTemplate), template)
			
	expect_false(file.exists(patientProfilesVis:::getPathTemplate("blabla")))

})

test_that("The default report format for subject profiles is correctly extracted", {
			
	reportFormat <- subjectProfileReportFormat()		
	expect_type(reportFormat, "list")
	expect_named(
		reportFormat,
		c("heightLineIn", "margin", "landscape", 
		"aspectRatio", "yLabelWidth")
	)
	expect_equal(reportFormat$heightLineIn, 0.2)
	expect_equal(reportFormat$margin, 0.75)
	expect_equal(reportFormat$landscape, FALSE)
	expect_equal(reportFormat$aspectRatio, 0.5)
	expect_equal(reportFormat$yLabelWidth, 30)
			
})

test_that("The combined variable contains a missing value only if all variables to combine are missing", {
			
	data <- data.frame(
		A = c(NA_character_, "1", NA_character_),
		B = c(1, NA_real_, NA_real_)
	)	
	
	res <- patientProfilesVis:::interactionWithMissing(
		data = data,
		vars = c("A", "B")
	)
	resReference <- factor(
		c('NA - 1', '1 - NA', 'NA - NA'),
		levels = c("1 - NA", "NA - 1", "NA - NA")
	)
	expect_identical(res, resReference)
			
})

test_that("When factor variables are combined, the order of their levels is retained", {
			
	data <- data.frame(
		var1 = factor(
			c("A", "A", "B", NA_character_),
			levels = c("B", "A")
		),
		var2 = factor(
			c("a1", "a2", "b1", NA_character_),
			levels = c("b1", "a2", "a1")
		)
	)
			
	res <- patientProfilesVis:::interactionWithMissing(
		data = data,
		vars = c("var1", "var2")
	)
	resReference <- factor(
		x = c("A - a1", "A - a2", "B - b1", "NA - NA"),
		levels = c("B - b1", "A - a2", "A - a1", "NA - NA")			
	)
	expect_identical(res, resReference)
			
})

test_that("When character variables are combined, their levels are ordered alphabetically", {
			
	data <- data.frame(
		var1 = factor(c("B", "A", "A", NA_character_)),
		var2 = factor(c("b1", "a2", "a1", NA_character_))
	)
			
	res <- patientProfilesVis:::interactionWithMissing(
		data = data,
		vars = c("var1", "var2")
	)
	resReference <- factor(
		x = c("B - b1", "A - a2", "A - a1", "NA - NA"),
		levels = c("A - a1", "A - a2", "B - b1", "NA - NA")			
	)
	expect_identical(res, resReference)
			
})

test_that("A variable used for a plot aesthetic is formatted correctly", {
			
	data <- data.frame(
		TEST = c("B", "A", "A", "", NA_character_),
		stringsAsFactors = FALSE
	)
	expect_message(
		varAes <- convertAesVar(data = data, var = "TEST"),
		"Empty records .* converted to NA."
	)
	expect_is(varAes, "factor")
	expect_equal(
		as.character(varAes),
		c("B", "A", "A", NA_character_, NA_character_)
	)
	expect_equal(levels(varAes), c("A", "B", NA_character_))
			
})
