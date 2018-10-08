context("Compare 'subjectProfile[Function]Plot' with previous version")

#library(graphicsQC)
library(vdiffr)

# example dataset(s) formatted as a list of data.frame
data(SDTMDataPelican)
# and corresponding labels
data(labelVarsSDTMPelican)

# subjectProfileTextPlot

paramValueVarFct <- function(data)
	with(data, paste0(MHENRTPT, " (start = ", MHSTDTC, 
		ifelse(MHENDTC != "", paste0(", end = ", MHENDTC, ")"), ")")
	)
)
pl <- subjectProfileTextPlot(
	data = SDTMDataPelican$MH,
	paramNameVar = "MHDECOD",
	paramValueVar = paramValueVarFct,
	title = "Medical History 2: status with dates",
	labelVars = labelVarsSDTMPelican
)
#ggsave(filename = "test.png", plot = pl[[1]][[1]])
#plotFile(filename = "test.png", prefix = "testSavedGgplot2", clear  =TRUE)

test_that("subjectProfileTextPlot", 
	vdiffr::expect_doppelganger(
		title = "subjectProfileTextPlot1", 
		fig = pl[[1]][[1]],
		verbose = TRUE
	)
)

# with the graphicsQC package
#library(graphicsQC)
#output <- plotExpr(
#	expr = "print(pl[[1]][[1]] + ggtitle('test'))", "pdf", 
#	path = "testGgplot2", 
#	prefix = "example1",
#	clear = TRUE
#)
#output2 <- plotExpr(
#	expr = "print(pl[[1]][[1]] + ggtitle('Medical History: status with dates'))", 
#	"pdf", 
#	path = "testGgplot2", 
#	prefix = "example1bis",
#	clear = TRUE
#)
#compare(test = output, control = output2)

