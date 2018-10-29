# Check test coverage for the 'patientProfileVis' package
# 
# Author: Laure Cougnaud
###############################################################################

library(covr)

# test coverage in tests + examples + vignettes for all 'subjectProfile'*'Plot' functions
pc <- package_coverage(
	path = "../package/patientProfilesVis", 
	type = "all", 
	function_exclusions = "^(?!subjectProfile.*Plot)"
)
report(x = pc, file = "testCoverage-patientProfilesVis.html")

