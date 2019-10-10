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
			`missing start` = "\u25C0", `missing end` = "\u25B6"
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

