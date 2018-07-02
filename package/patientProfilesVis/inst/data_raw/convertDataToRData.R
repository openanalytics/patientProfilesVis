# Project: GLPGPatientProfiles
# 
# Author: Laure Cougnaud
###############################################################################

pathData <- file.path(
	"../../../../data/GLPG2737-CL-202-UNBLINDED/SDTM",
	c("CRG10GLP01_SDTM_2018_06JUN_15_v2", "CRG10GLP01_SDTM_2018_06JUN_19")
)

dataFiles <- list.files(
	pattern = "^(ae|dm|lb|mh)\\.sas7bdat$",
	path = pathData, 
	full.names = TRUE
)

res <- loadDataADaM(files = dataFiles)
sdtmDataPelican <- res$data
save(sdtmDataPelican, file = "../data/sdtmDataPelican.RData")

labelVarsPelican <- res$labelVars
save(labelVarsPelican, file = "../data/labelVarsPelican.RData")
