context("Miscellaneous utility functions for the patient profiles")

test_that("path of patient profiles template is retrieved", {

	template <- "subjectProfile.Rnw"
	pathTemplate <- patientProfilesVis:::getPathTemplate(template)
	expect_true(file.exists(pathTemplate))
	expect_equal(basename(pathTemplate), template)
			
	expect_false(file.exists(patientProfilesVis:::getPathTemplate("blabla")))

})

test_that("default report format for subject profile is extracted", {
			
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

test_that("missing values in one of the parameter variable don't propagate", {
			
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

test_that("parameter factor variable(s) have their order retained when combined", {
			
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
		c("A - a1", "A - a2", "B - b1", "NA - NA"),
		c("B - b1", "A - a2", "A - a1", "NA - NA")			
	)
	expect_identical(res, resReference)
			
})

test_that("parameter character variable(s) are order alphabetically when combined", {
			
	data <- data.frame(
		var1 = factor(c("A", "A", "B", NA_character_)),
		var2 = factor(c("a1", "a2", "b1", NA_character_))
	)
			
	res <- patientProfilesVis:::interactionWithMissing(
		data = data,
		vars = c("var1", "var2")
	)
	resReference <- factor(
		c("A - a1", "A - a2", "B - b1", "NA - NA"),
		c("A - a1", "A - a2", "B - b1", "NA - NA")			
	)
	expect_identical(res, resReference)
			
})

test_that("a variable is formatted to be used as an aesthetic in a plot", {
			
	data <- data.frame(
		TEST = c("A", "B", "A", "", NA_character_),
		stringsAsFactors = FALSE
	)
	expect_message(
		varAes <- convertAesVar(data = data, var = "TEST"),
		"Empty records .* converted to NA."
	)
	expect_is(varAes, "factor")
	expect_equal(
		as.character(varAes),
		c("A", "B", "A", NA_character_, NA_character_)
	)
	expect_equal(levels(varAes), c("A", "B", NA_character_))
			
})
