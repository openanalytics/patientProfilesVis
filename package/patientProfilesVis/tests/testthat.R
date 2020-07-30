library(testthat)
library(patientProfilesVis)
library(vdiffr)
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

# to have failed tests with vdiffr not skipped
Sys.setenv("CI" = "true")

if (Sys.getenv("TESTTHAT_OUTPUT_FILE") != "")
	options(testthat.output_file = Sys.getenv("TESTTHAT_OUTPUT_FILE", stdout()))
test_check(
		"patientProfilesVis",
		reporter = Sys.getenv("TESTTHAT_DEFAULT_CHECK_REPORTER", "check"))


# create figures for new test:
# manage_cases()
# run tests for the package:
# devtools::test()
# run all R CMD CHECK
# devtools::check(pkg = ".")

