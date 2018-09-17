# Project: GLPGPatientProfiles
# 
# Author: Laure Cougnaud
###############################################################################

library(patientVisUtility)

dataFiles <- "../../../../../GLPGSAPCysticFibrosis/data/Pelican/ADAM/20180525 study/study_ADAM_2018_08AUG_01_ADRE/adre.sas7bdat"

dataADaM <- loadDataADaMSDTM(files = dataFiles)

#res <- getTimeVsReference(dataList)

ADaMDataPelican <- dataADaM
save(ADaMDataPelican, file = "../data/ADaMDataPelican.RData")

labelVarsADaMPelican <- attr(dataADaM, "labelVars")
save(labelVarsADaMPelican, file = "../data/labelVarsADaMPelican.RData")
