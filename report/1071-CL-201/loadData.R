# TODO: Add comment
# 
# Author: lcougnaud
###############################################################################

library(glpgUtilityFct)

files <- list.files(
#	pattern = "ae|dm|sv|cm",
	path = "/home/lcougnaud/git/GLPGPatientProfiles/data/1072-CL-201/DM005 Draft DB/DSMB/transfer 00b test/DSMB_SAS",
	full.names = TRUE
)
data <- loadDataADaMSDTM(files = files)
labelVars <- attributes(data)$labelVars
labelVars[grep("FL$", names(labelVars))]

library(tidyverse)

# all SUBJID <-> USUBJID
table((data$DM %>% group_by(USUBJID, SUBJID) %>% tally())$n)

n_distinct(data$DM$USUBJID) # 661 patients
n_distinct(data$CM$USUBJID) # 235 with CM patients
n_distinct(data$AE$USUBJID) # 17 with AE patients (sub-group)
all(data$AE$USUBJID %in% data$CM$USUBJID)


range(data$SV[, c("SVSTDY", "SVENDY")], na.rm = TRUE)
