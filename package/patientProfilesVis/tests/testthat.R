library(testthat)
library(patientProfilesVis)
library(glpgUtilityFct)

# The same collate statement should be used than the R CMD check
# at the creation of the reference figures with 'manage_cases'
# this affect the order of the elements with 'reorder'
# if different, the order of the parameters in the y-axis of the plot might differ
Sys.setlocale(category = "LC_COLLATE", locale = "C")

# Load example dataset(s) (formatted as a list of data.frame)
data(SDTMDataPelican)
# and corresponding labels
data(labelVarsSDTMPelican)

reporters <- testthat::MultiReporter$new(
	reporters = list(
		# default check
		CheckReporter$new(), 
		# Reporter for Jenkins
		JunitReporter$new(file = file.path(getwd(), "results.xml"))
	)
)

test_check(
	"patientProfilesVis",
	reporter = reporters
)

