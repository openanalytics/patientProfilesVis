context("Creation of subject profile report")
# load data
library(glpgUtilityFct)
data(SDTMDataPelican)
data(labelVarsSDTMPelican)

# AEPTCD: preferred term code
dataAE <- SDTMDataPelican$AE
# specify order of value in 'AESEV'
dataAE[, "AESEV"] <- factor(dataAE[, "AESEV"], levels = c("MILD", "MODERATE"))


test_that("createSubjectProfileReport - unicode symbols", {
			
	# add status for dates:
	set.seed(1234) 
	dataAE[, paste0(c("AESTDY", "AEENDY"), "ST")] <- 
		sample(x = c("missing start", "missing end", "partial", "complete"), 
			size = nrow(dataAE)*2, replace = TRUE)
	
	aePlotsShape <- subjectProfileIntervalPlot(
		data = dataAE,
		paramVar = "AETERM",
		timeStartVar = "AESTDY", timeEndVar = "AEENDY",
		timeStartShapeVar = "AESTDYST", timeEndShapeVar = "AEENDYST",
		# left/right triangle not available in default ggplot palette
		# so use unicode character (hexadecimal notation)
		shapePalette = c(
			complete = '\u25A0', partial = '\u25EF', 
			`missing start` = "\u25C4", `missing end` = "\u25BA"
		),
		shapeLab = "Study date status", shapeSize = 4,
		colorVar = "AESEV",
		labelVars = labelVarsSDTMPelican,
		title = "Adverse events"
	)
	
	createSubjectProfileReport(
		listPlots = list(aePlotsShape),
		outputFile = "subjectProfile_SDTM_Unicode.pdf",
		exportFigures = TRUE,
		verbose = TRUE
	)
			
})


test_that("createSubjectProfileReport - text module - long axis label and table format", {
			
	dataPlot <- SDTMDataPelican$AE		
	dataPlot[1, "AEDECOD"] <- paste(sample(LETTERS, size = 200, replace = TRUE), collapse = " ")
			
	aeListingPlotsTable <- subjectProfileTextPlot(
		data = dataPlot, #subset(dataPlot, USUBJID == "study-4903-07"),
		paramValueVar = c("AEDECOD", "AESTDTC", "AEENDTC", "AESEV", "AESER", "AEACN"),
		paramGroupVar = "AESTDTC",
		labelVars = labelVarsSDTMPelican,
		table = TRUE
	)
	aeListingPlots <- subjectProfileTextPlot(
		data = dataPlot, #subset(dataPlot, USUBJID == "study-4903-07"),
		paramNameVar = "AEDECOD",
		paramValueVar = c("AESTDTC", "AEENDTC", "AESEV", "AESER", "AEACN"),
		labelVars = labelVarsSDTMPelican,
		table = FALSE
	)
#	aeListingPlots[["study-4903-07"]]
	
	createSubjectProfileReport(
		listPlots = list(aeListingPlotsTable, aeListingPlots),
		outputFile = "subjectProfile_longVariable.pdf",
#		subjectSubset = "study-4909-01",
		exportFigures = TRUE,
		verbose = TRUE
	)
	
})


test_that("createSubjectProfileReport - text module - long axis label and table format", {
			
	# AEPTCD: preferred term code
	dataAE <- SDTMDataPelican$AE
	
	aePlots <- subjectProfileIntervalPlot(
		data = dataAE,
		paramVar = "AETERM",
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		colorVar = "AESOC",
		labelVars = labelVarsSDTMPelican,
		title = "Adverse events"
	)
	attr(aePlots[["study-4903-04"]][[1]], "metaData")$nLines
	
	createSubjectProfileReport(
		listPlots = list(aePlots),
		outputFile = "subjectProfile_legend.pdf",
		subset = "study-4903-04",
		verbose = TRUE
	)
			
})

test_that("createSubjectProfileReport - specification of only one time limit", {
			
	# AEPTCD: preferred term code
	dataAE <- SDTMDataPelican$AE
	
	aePlots <- subjectProfileIntervalPlot(
		data = dataAE,
		paramVar = "AETERM",
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		colorVar = "AESEV",
		labelVars = labelVarsSDTMPelican,
		title = "Adverse events",
		timeLim = c(0, NA)
	)
	
	library(glpgUtilityFct)
	data(SDTMDataPelican)
	data(labelVarsSDTMPelican)
	
	# prepare data for plots:
	dataLB <- SDTMDataPelican$LB
	# sort the categories (empty values '' becomes NA)
	dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH"))
	lbLinePlots <- subjectProfileLinePlot(
		data = dataLB,
		paramNameVar = "LBTEST", 
		paramValueVar = "LBSTRESN",
		paramGroupVar = "LBSCAT",
		paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
		timeVar = "LBDY",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTMPelican
	)
	
	lPlots <- list(AE = aePlots, LB = lbLinePlots)
	createSubjectProfileReport(
		listPlots = lPlots,
		outputFile = "subjectProfile_timeLim_oneMissing.pdf",
		subset = names(aePlots)[1:2],
		verbose = TRUE
	)
	
	createSubjectProfileReport(
		listPlots = lPlots,
		outputFile = "subjectProfile_timeLim_oneMissing.pdf",
		subset = names(aePlots)[1:2],
		verbose = TRUE,
		timeAlign = "AE"
	)
	
})

test_that("createSubjectProfileReport - missing start/end", {
			
	# AEPTCD: preferred term code
	dataAE <- SDTMDataPelican$AE
	
	aePlots <- subjectProfileIntervalPlot(
		data = dataAE,
		paramVar = "AETERM",
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		colorVar = "AESEV",
		labelVars = labelVarsSDTMPelican,
		title = "Adverse events"
	)
	
	# prepare data for plots:
	dataLB <- SDTMDataPelican$LB
	# sort the categories (empty values '' becomes NA)
	dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH"))
	lbLinePlots <- subjectProfileLinePlot(
		data = dataLB,
		paramNameVar = "LBTEST", 
		paramValueVar = "LBSTRESN",
		paramGroupVar = "LBSCAT",
		paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
		timeVar = "LBDY",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTMPelican
	)
			
	lPlots <- list(AE = aePlots, LB = lbLinePlots)
	createSubjectProfileReport(
		listPlots = list(AE = aePlots),
		outputFile = "subjectProfile_missingStartEnd.pdf",
		verbose = TRUE,
		subset = "study-4902-01",
		timeAlign = "AE"
	)
	
	subjectWithLBAE <- intersect(names(aePlots), names(lbLinePlots))[1:2]
	subjects <- c("study-4902-01", subjectWithLBAE)
	createSubjectProfileReport(
		listPlots = list(AE = aePlots, LB = lbLinePlots),
		outputFile = "subjectProfile_missingStartEnd.pdf",
		verbose = TRUE,
		subset = subjects,
		timeAlign = "AE"
	)

})

test_that("Rendering of Rmd document after creation of Sweave patient profiles", {
	
	# AEPTCD: preferred term code
	dataAE <- SDTMDataPelican$AE
	
	aePlots <- subjectProfileIntervalPlot(
		data = dataAE,
		paramVar = "AETERM",
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		colorVar = "AESEV",
		labelVars = labelVarsSDTMPelican,
		title = "Adverse events"
	)
	createSubjectProfileReport(
		listPlots = list(AE = aePlots),
		outputFile = "subjectProfile_missingStartEnd.pdf",
		verbose = TRUE,
		subset = names(aePlots)[1],
		timeAlign = "AE"
	)
	
	# Note: test with dummy Rmd, vignette cannot be used because
	# not yet created 
	pathTestRmd <- "test.Rmd"
	cat('---',
		'title: "Test document"',
		'subtitle: "Study: X, Batch X"',
		'---\n',
		'### Test section  \n',
		'This is a test paragraph',
		sep = "\n",
		file = "test.Rmd"
	)
	# version < 1.2.0: 
	# Error in gsub(inline.code, "\\1", input[idx]) : invalid 'pattern' argument
	expect_silent(test <- rmarkdown::render("test.Rmd", quiet = TRUE))
	# clean
	tmp <- file.remove(c("test.Rmd", "test.html")) 
	
})


