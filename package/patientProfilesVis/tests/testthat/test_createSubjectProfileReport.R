context("Create subject profile report")

library(pdftools)

test_that("report is created with custom file name", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = "1"
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)			
			
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = c("1", "2")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
			
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_silent(
		createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile
		)
	)
	expect_true(file.exists(reportFile))
			
})

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


test_that("report is created per subject", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = "subject-I",
		stringsAsFactors = FALSE
	)
			
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)			
			
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID = c("subject-II", "subject-I"),
		stringsAsFactors = FALSE
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
			
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			reportPerSubject = TRUE
		)
	)
	
	# check that output paths for each subject
	subjectIDs <- unique(c(dataA$USUBJID, dataB$USUBJID))
	expect_length(paths, length(subjectIDs))
	
	for(i in seq_along(subjectIDs)){
		expect_match(paths[i], paste0(subjectIDs[[i]], "\\.pdf$"))
	}
	
	# and that the files exist
	expect_true(all(file.exists(paths)))
	
	# report across subjects doesn't exist
	expect_false(file.exists(reportFile))
			
})

test_that("report is created with bookmark", {
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = "subject-I"
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)			
			
	dataB <- data.frame(
		TEST = "1",
		DY = c(3, 4),
		USUBJID =  c("subject-II", "subject-I")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
	
	bookmarkData <- data.frame(
		USUBJID = c("subject-I", "subject-II"),
		SEX = c("Male", "Female"),
		AGE = c("25 years", "58 years")
	)
	bookmarkVars <- c("SEX", "AGE")
			
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_silent(
		createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			bookmarkData = bookmarkData,
			bookmarkVar = bookmarkVars,
			# by default, subjectSortData == bookmarkData
			subjectSortData = NULL
		)
	)
	expect_true(file.exists(reportFile))
	
	# check that there is an index based on SEX and AGE in the Table of Content
	tocCntList <- pdftools::pdf_toc(reportFile)
	tocCnt <- unlist(tocCntList, recursive = TRUE)
	expect_true(any(grepl("Index based on SEX", tocCnt)))
	expect_true(any(grepl("Index based on AGE", tocCnt)))
	
	reportCnt <- pdftools::pdf_data(reportFile)
	reportCntIndex <- reportCnt[sapply(reportCnt, function(x) grepl("Index", x[, "text"]))]
	reportIndexTxt <- sapply(reportCntIndex, function(x) paste(x[["text"]], collapse = " "))
	
	# by default, subjects are sorted based on alphabetical order
	# (because subjectSortData is not specified)
	expect_true(any(grepl("Index.*SEX.*Female.*2.*Male.*1", reportIndexTxt)))
	expect_true(any(grepl("Index.*AGE.*25 years.*1.*58 years.*2", reportIndexTxt)))
	
			
})