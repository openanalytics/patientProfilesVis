context("Miscellaneous utility functions for the patient profiles")

test_that("path of patient profiles template is retrieved", {

	template <- "subjectProfile.Rnw"
	pathTemplate <- getPathTemplate(template)
	expect_true(file.exists(pathTemplate))
	expect_equal(basename(pathTemplate), template)
			
	expect_false(file.exists(getPathTemplate("blabla")))

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
