# ABSOLUTE output path for module settings
# TO CHANGE when pushing to server!!
outputPath <- "/home/lcougnaud/git/GLPGPatientProfiles/shiny/moduleSettings"
if(!dir.exists(outputPath))	dir.create(outputPath, recursive = TRUE)
# "/root/shiny/patientProfilesVisShiny/moduleSettings"

# for initial tests, set wd to /ui/
# remove this code later
suppressWarnings({if(!require(patientProfilesVisShiny)) {
        cat("Loading via devtools...")
        library(devtools)
        load_all("./../../../patientProfilesVisShiny")
      } else {
        library(patientProfilesVisShiny)
        cat("Loading the package...")
      }})



# (1) Copy the UI files & folders from "inst/ui" for local use
tmpDir <- tempdir()
setwd(tmpDir)

uiDir <- system.file("ui", package = "patientProfilesVisShiny")
uiFiles <- list.files(path = uiDir, full.names = FALSE, recursive = TRUE)
uiFiles <- uiFiles[!(uiFiles %in% c("global.R"))]

sapply(uiFiles, function(from) {
	to <- file.path(tmpDir, from)
	toDir <- dirname(to)
	if (!dir.exists(toDir)) {
		dir.create(path = toDir, recursive = TRUE)
	}
    file.copy(from = file.path(uiDir, from), to = to, overwrite = TRUE)
})  


