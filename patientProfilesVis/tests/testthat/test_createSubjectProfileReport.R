context("Create subject profile report")

library(pdftools)

# Note: a few tests are skipped on CRAN because the overall suite is too
# time-consuming to execute in CRAN

test_that("A report is correctly created with a custom file name", {
			
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

test_that("Progress messages are printed when requested during report creation", {
			
	skip_on_cran() 
			
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
			
	expect_message(
		createSubjectProfileReport(
			listPlots = list(plots),
			outputFile = reportFile,
			verbose = TRUE
		)
	)
	
	expect_silent(
		createSubjectProfileReport(
			listPlots = list(plots),
			outputFile = reportFile,
			verbose = FALSE
		)
	)
	
})

test_that("The report is created without errors when unicode symbols are used", {
			
	skip_on_cran() 
			
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
			outputFile = reportFile
		)
	)
	expect_true(file.exists(reportFile))
	expect_gte(file.info(reportFile)$size, 0)
			
})

test_that("A report is successfully created if only one time limit is specified", {

	skip_on_cran() 
			
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
			outputFile = reportFile
		)
	)
	expect_true(file.exists(reportFile))
	expect_gte(file.info(reportFile)$size, 0)
			
})

test_that("A Rmd document is rendered correctly after the creation of the subject profiles", {
	
	skip_on_cran() 		
			
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
		outputFile = reportFile
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


test_that("The report is successfully created per subject", {
	
	skip_on_cran() 			
			
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

test_that("A report is correctly created with bookmark", {

	skip_on_cran() 	
			
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

test_that("A report is correctly created with bookmark, containig variable labels", {

	skip_on_cran() 
			
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
		AGE = c("25", "58")
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
			subjectSortData = NULL,
			labelVars = c(AGE = "Age (years)")
		)
	)
	expect_true(file.exists(reportFile))
			
	# check that there is an index based on SEX and AGE in the Table of Content
	tocCntList <- pdftools::pdf_toc(reportFile)
	tocCnt <- unlist(tocCntList, recursive = TRUE)
	expect_true(any(grepl("Index based on SEX", tocCnt)))
	expect_true(any(grepl("Index based on Age (years)", tocCnt, fixed = TRUE)))
	
	reportCnt <- pdftools::pdf_data(reportFile)
	reportCntIndex <- reportCnt[sapply(reportCnt, function(x) grepl("Index", x[, "text"]))]
	reportIndexTxt <- sapply(reportCntIndex, function(x) paste(x[["text"]], collapse = " "))
	
	# by default, subjects are sorted based on alphabetical order
	# (because subjectSortData is not specified)
	expect_true(any(grepl("Index.*SEX", reportIndexTxt)))
	expect_true(any(grepl("Index.*Age \\(years\\)", reportIndexTxt)))
	
})

test_that("Warnings are generated in case the bookmark data does not contain the bookmark or the subject variables", {

	skip_on_cran() 
			
	data <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = "subject-I"
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)	
	listPlots <- list(A = listPlots)	
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_warning(
		createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			bookmarkData = data.frame(SEX = "Female"),
			bookmarkVar = "SEX"
		),
		"USUBJID.*not available"
	)
	
	expect_warning(
		createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			bookmarkData = data.frame(USUBJID = "subject-I"),
			bookmarkVar = "SEX"
		),
		"SEX.*not available"
	)
	
})

test_that("Subjects are correctly sorted in the report in ascending order of a specified variable", {

	skip_on_cran() 
			
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
			
	subjectSortData <- data.frame(
		USUBJID = c("subject-I", "subject-II"),
		AGE = c(58, 25)
	)
	subjectSortVar <- "AGE"
			
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			subjectSortData = subjectSortData,
			subjectSortVar = subjectSortVar
		)
	)
	expect_true(file.exists(reportFile))
			
	reportCnt <- pdftools::pdf_data(reportFile)
	reportCntTxt <- sapply(reportCnt, function(x) paste(x[["text"]], collapse = " "))
	
	# first subject II, then subject I
	expect_lte(
		grep(" subject-II ", reportCntTxt),
		grep(" subject-I ", reportCntTxt)
	)
	
})

test_that("Subjects are correctly sorted in the report in descending order of a specified variable", {

	skip_on_cran() 
			
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
	
	subjectSortData <- data.frame(
		USUBJID = c("subject-I", "subject-II"),
		AGE = c(58, 25)
	)
	subjectSortVar <- "AGE"
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			subjectSortData = subjectSortData,
			subjectSortVar = subjectSortVar,
			subjectSortDecreasing = TRUE
		)
	)
	expect_true(file.exists(reportFile))
	
	reportCnt <- pdftools::pdf_data(reportFile)
	reportCntTxt <- sapply(reportCnt, function(x) paste(x[["text"]], collapse = " "))
	
	# first subject I, then subject II
	expect_gte(
		grep(" subject-II ", reportCntTxt),
		grep(" subject-I ", reportCntTxt)
	)
	
})

test_that("A warning is generated if the dataset to sort the subjects does not contain the sorting or subject variables", {

	skip_on_cran() 
			
	data <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = "subject-I"
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)	
	listPlots <- list(A = listPlots)	
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_warning(
		createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			subjectSortData = data.frame(SEX = "Female"),
			subjectSortVar = "SEX"
		),
		"USUBJID.*not available"
	)
	
	expect_warning(
		createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			subjectSortData = data.frame(USUBJID = "subject-I"),
			subjectSortVar = "SEX"
		),
		"SEX.*not available"
	)
	
})

test_that("Reports are created successfully by subject based on a sorting variable", {

	skip_on_cran() 
			
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
			
	subjectSortData <- data.frame(
		USUBJID = c("subject-I", "subject-II"),
		AGE = c(58, 25)
	)
	subjectSortVar <- "AGE"
			
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			subjectSortData = subjectSortData,
			subjectSortVar = subjectSortVar,
			reportPerSubject = TRUE
		)
	)

	# first subject II, then subject I
	expect_lte(
		grep("subject-II.pdf$", paths),
		grep("subject-I.pdf$", paths)
	)
			
})

test_that("A report is correctly created for a subset of the subjects based on a dataset", {

	skip_on_cran() 
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("subject-A", "subject-F")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)			
			
	dataB <- data.frame(
		TEST = "1",
		DY = seq.int(4),
		USUBJID =  c("subject-B", "subject-A", "subject-C", "subject-D")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
			
	subjectSubsetData <- data.frame(
		USUBJID = c("subject-A", "subject-C")
	)
			
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			subjectSubsetData = subjectSubsetData
		)
	)
	
	reportCnt <- pdftools::pdf_data(reportFile)
	reportCntTxt <- sapply(reportCnt, function(x) paste(x[["text"]], collapse = " "))
	reportCntTxtWithSubj <- grep("subject-\\w", reportCntTxt, value = TRUE)
	reportSubjects <- gsub(".+ (subject\\-\\w) .+", "\\1", reportCntTxtWithSubj)
	
	expect_setequal(reportSubjects, c("subject-A", "subject-C"))
	
})

test_that("A report is correctly created for a subset of the subjects, based on a specified value of a variable in a dataset", {

	skip_on_cran() 
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("subject-A", "subject-F")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)			
	
	dataB <- data.frame(
		TEST = "1",
		DY = seq.int(4),
		USUBJID =  c("subject-B", "subject-A", "subject-C", "subject-D")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
	
	subjectSubsetData <- data.frame(
		USUBJID = c("subject-A", "subject-C", "subject-B", "subject-F"),
		TRT = c(NA_character_, "A", "B", "B")
	)
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			subjectSubsetData = subjectSubsetData,
			subjectSubsetVar = "TRT",
			subjectSubsetValue = "B"
		)
	)
	
	reportCnt <- pdftools::pdf_data(reportFile)
	reportCntTxt <- sapply(reportCnt, function(x) paste(x[["text"]], collapse = " "))
	reportCntTxtWithSubj <- grep("subject-\\w", reportCntTxt, value = TRUE)
	reportSubjects <- gsub(".+ (subject\\-\\w) .+", "\\1", reportCntTxtWithSubj)
	
	expect_setequal(reportSubjects, c("subject-F", "subject-B"))
			
})

test_that("A report is correctly created for a random sample of the subjects", {

	skip_on_cran() 
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("subject-A", "subject-F")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)			
	
	dataB <- data.frame(
		TEST = "1",
		DY = seq.int(4),
		USUBJID =  c("subject-B", "subject-A", "subject-C", "subject-D")
	)
	listPlotsB <- subjectProfileEventPlot(
		data = dataB,
		paramVar = "TEST",
		timeVar = "DY"
	)
	listPlots <- list(A = listPlotsA, B = listPlotsB)	
	
	subjectSubsetData <- data.frame(
		USUBJID = c("subject-A", "subject-C", "subject-D")
	)
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			subjectSubsetData = subjectSubsetData,
			subjectSample = 2
		)
	)
	
	reportCnt <- pdftools::pdf_data(reportFile)
	reportCntTxt <- sapply(reportCnt, function(x) paste(x[["text"]], collapse = " "))
	reportCntTxtWithSubj <- grep("subject-\\w", reportCntTxt, value = TRUE)
	reportSubjects <- gsub(".+ (subject\\-\\w) .+", "\\1", reportCntTxtWithSubj)
	
	expect_length(reportSubjects, 2)	
	expect_true(all(reportSubjects %in% subjectSubsetData$USUBJID))
	
})


test_that("A report is correctly created for a set of specified subjects by specifying subject IDs", {
			
	skip_on_cran() 
			
	dataA <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("subject-A", "subject-F")
	)
	listPlotsA <- subjectProfileEventPlot(
		data = dataA,
		paramVar = "TEST",
		timeVar = "DY"
	)			
	
	dataB <- data.frame(
		TEST = "1",
		DY = seq.int(4),
		USUBJID =  c("subject-B", "subject-A", "subject-C", "subject-D")
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
			subset = c("subject-A", "subject-C", "subject-Y")
		)
	)
	
	reportCnt <- pdftools::pdf_data(reportFile)
	reportCntTxt <- sapply(reportCnt, function(x) paste(x[["text"]], collapse = " "))
	reportCntTxtWithSubj <- grep("subject-\\w", reportCntTxt, value = TRUE)
	reportSubjects <- gsub(".+ (subject\\-\\w) .+", "\\1", reportCntTxtWithSubj)
	
	expect_setequal(reportSubjects, c("subject-A", "subject-C"))	
	
})

test_that("Patient profile figures are successfully exported", {

	skip_on_cran() 
			
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
			
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	figDir <- file.path(dirname(reportFile), "figures")
	if(dir.exists(figDir)) unlink(figDir, recursive = TRUE)
	
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			exportFigures = FALSE
		)
	)
	# does the figure dir exist?
	expect_false(dir.exists(figDir))
	
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			exportFigures = TRUE
		)
	)
	
	# does the figure dir exist?
	expect_true(dir.exists(figDir))
	
	# do the figure files exist?
	figFiles <- list.files(figDir)
	expect_true(all(c("subject-I-1.pdf", "subject-II-1.pdf") %in% figFiles))
	
})

test_that("A warning is generated if the report is requested to be exported by batch when the report is created across subjects", {

	skip_on_cran() 
			
	data <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)		
	listPlots <- list(A = listPlots)
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	
	expect_warning(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			exportBatchSize = 5
		),
		"creation.*per batch not possible",
		ignore.case = TRUE
	)
	
})

test_that("A warning is generated in case a report is requested to be exported when plots are not aligned across subjects", {

	skip_on_cran() 
			
	data <- data.frame(
		TEST = "1",
		DY = c(1, 2),
		USUBJID = c("1", "2")
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)		
	listPlots <- list(A = listPlots)
	
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
	
	expect_warning(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			exportBatchSize = 5,
			reportPerSubject = TRUE
		),
		"creation.*per batch not possible",
		ignore.case = TRUE
	)
	
})

test_that("Subject profiles are successfully exported by batch", {

	skip_on_cran() 
			
	data <- data.frame(
		TEST = "1",
		DY = 1,
		USUBJID = as.character(seq.int(7))
	)
	listPlots <- subjectProfileEventPlot(
		data = data,
		paramVar = "TEST",
		timeVar = "DY"
	)		
	listPlots <- list(A = listPlots)
		
	reportFile <- tempfile(pattern = "report", fileext = ".pdf")
			
	expect_silent(
		paths <- createSubjectProfileReport(
			listPlots = listPlots,
			outputFile = reportFile,
			exportBatchSize = 3,
			reportPerSubject = TRUE,
			timeAlignPerSubject = "all"
		)
	)
	
	expect_length(paths, length(unique(data$USUBJID)))
	
})
			
