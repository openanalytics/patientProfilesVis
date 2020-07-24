library(covr)
pc <- package_coverage(
    path = "~/git/GLPGPatientProfiles/package/patientProfilesVis",
    type = "tests", quiet = FALSE, clean = FALSE
)
report(x = pc,
    file = paste0("testCoverage-tests-patientProfilesVis",
        packageVersion("patientProfilesVis"), ".html")
)
