# Project: GLPGPatientProfiles
# 
# Author: Laure Cougnaud
###############################################################################

library(patientProfilesVis)

## SDTM dataset

pathData <- file.path(
	"../../../../data/GLPG2737-CL-202-UNBLINDED/SDTM",
	c("CRG10GLP01_SDTM_2018_06JUN_15_v2", "CRG10GLP01_SDTM_2018_06JUN_19")
)

dataFiles <- list.files(
	pattern = "^(ae|dm|lb|mh|ex|sv|tv|cm)\\.sas7bdat$",
	path = pathData, 
	full.names = TRUE
)

dataSDTM <- loadDataADaMSDTM(files = dataFiles)

#res <- getTimeVsReference(dataList)

SDTMDataPelican <- dataSDTM
save(SDTMDataPelican, file = "../data/SDTMDataPelican.RData")

labelVarsSDTMPelican <- attr(dataSDTM, "labelVars")
save(labelVarsSDTMPelican, file = "../data/labelVarsSDTMPelican.RData")

## ADAM dataset

pathData <- "/home/lcougnaud/git/GLPGSAPCysticFibrosis/data/Pelican/ADAM/20180525 GLPG2737-CL-202-UNBLINDED/GLPG2737-CL-202_ADAM_2018_06JUN_15_v2"

dataFiles <- list.files(
	pattern = "^ad(ae|lb|mh|ex|sl)\\.sas7bdat$",
	path = pathData, 
	full.names = TRUE
)

dataADaM <- loadDataADaMSDTM(files = dataFiles)

#res <- getTimeVsReference(dataList)

ADaMDataPelican <- dataADaM
save(ADaMDataPelican, file = "../data/ADaMDataPelican.RData")

labelVarsADaMPelican <- attr(dataADaM, "labelVars")
save(labelVarsADaMPelican, file = "../data/labelVarsADaMPelican.RData")
