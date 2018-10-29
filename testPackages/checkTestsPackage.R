# Run tests and check tests coverage for the 'patientProfileVis' package
# 
# Author: Laure Cougnaud
###############################################################################

packagePath <- "../package/patientProfilesVis/"

## create reference figures for vdiffr::expect_doppelganger

library(vdiffr)

# The same collate statement should be used than the R CMD check
# at the creation of the reference figures with 'manage_cases'
# this affect the order of the elements with 'reorder'
# if different, the order of the parameters in the y-axis of the plot might differ
Sys.setlocale(category = "LC_COLLATE", locale = "C")

validate_cases(collect_cases(package = packagePath))

## create the package
library(devtools)
pkgTarballPath <- build(pkg = packagePath, args = "--resave-data")

## check the package
check_built(path = pkgTarballPath)

## check the package coverage:

library(covr)

# test coverage all 'subjectProfile'*'Plot' functions
pc <- package_coverage(
	path = packagePath, 
	type = c("tests", "vignettes"), # tests + vignettes
	function_exclusions = "^(?!subjectProfile.*Plot)", # only subject profile plot function
	combine_types = TRUE # report coverage for each type
)
report(x = pc, file = "testCoverage-patientProfilesVis.html")

