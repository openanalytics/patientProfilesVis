# Project: GLPGPatientProfiles
# 
# Author: Laure Cougnaud
###############################################################################

pathData <- file.path(
	"../../../../data/study/SDTM",
	c("CRG10GLP01_SDTM_2018_06JUN_15_v2", "CRG10GLP01_SDTM_2018_06JUN_19")
)

dataFiles <- list.files(
	pattern = "^(ae|dm|lb|mh|ex)\\.sas7bdat$",
	path = pathData, 
	full.names = TRUE
)

data <- loadDataADaM(files = dataFiles)
dataListWithTime <- setReferenceTime(data = data)

#res <- getTimeVsReference(dataList)

sdtmDataPelican <- data
save(sdtmDataPelican, file = "../data/sdtmDataPelican.RData")

labelVarsPelican <- attr(data, "labelVars")
save(labelVarsPelican, file = "../data/labelVarsPelican.RData")
