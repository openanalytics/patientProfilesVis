context("Create of subject profile report")

test_that("report is created without errors in case of unicode symbols", {
			
	data <- data.frame(
		TEST = seq(2),
		START = c(1, -10),
		END = c(2, 0),
		START_STATUS = c("missing start", "complete"),
		END_STATUS = c("missing end", "partial"),
		USUBJID = "1",
		stringsAsFactors = FALSE
	)
	shapePalette <- c(
		complete = '\u25A0', partial = '\u25CB', 
		`missing start` = "\u25C4", `missing end` = "\u25BA"
	)
	listPlotsA <- subjectProfileIntervalPlot(
		data = data,
		timeStartVar = "START",
		timeEndVar = "END",
		paramVar = "TEST",
		timeStartShapeVar = "START_STATUS", 
		timeEndShapeVar = "END_STATUS",
		shapePalette = shapePalette
	)
	listPlots <- list(A = listPlotsA)
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	
	# check that creation proceeds without errors
	expect_silent(
		createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			verbose = FALSE
		)
	)
	expect_true(file.exists(reportFile))
	expect_gte(file.info(reportFile)$size, 0)
			
})

test_that("report is created if only one time limit is specified", {

	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1"
	)
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST",
		timeLim = c(0, NA)
	)
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	
	# check that creation proceeds without errors
	expect_silent(
		createSubjectProfileReport(
			listPlots = list(plots),
			outputFile = reportFile,
			verbose = FALSE
		)
	)
	expect_true(file.exists(reportFile))
	expect_gte(file.info(reportFile)$size, 0)
			
})

test_that("a Rmd document can be rendered after creation of patient profiles", {
	
	# this was a bug with patientProfilesVis < 1.2.0
			
	data <- data.frame(
		TEST = seq(3),
		DY = seq(3),
		USUBJID = "1"
	)
	plots <- subjectProfileEventPlot(
		data = data,
		timeVar = "DY",
		paramVar = "TEST"
	)
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	
	# check that creation proceeds without errors
	createSubjectProfileReport(
		listPlots = list(plots),
		outputFile = reportFile,
		verbose = FALSE
	)
	
	# test with dummy Rmd
	rmdFile <- tempfile(pattern = "dummy", fileext = ".Rmd")
	cat('---',
		'title: "Test document"',
		'subtitle: "Study: X, Batch X"',
		'---\n',
		'### Test section  \n',
		'This is a test paragraph',
		sep = "\n",
		file = rmdFile
	)
	# with version < 1.2.0: 
	# Error in gsub(inline.code, "\\1", input[idx]) : invalid 'pattern' argument
	expect_silent(rmarkdown::render(rmdFile, quiet = TRUE))
	
})