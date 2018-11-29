# TODO: Add comment
# 
# Author: lcougnaud
###############################################################################

library(glpgUtilityFct)

files <- list.files(
	pattern = "ae|dm|sv|cm",
	path = "/home/lcougnaud/git/GLPGPatientProfiles/data/1072-CL-201/DM005 Draft DB/DSMB/transfer 00b test/DSMB_SAS",
	full.names = TRUE
)
data <- loadDataADaMSDTM(files = files)

library(tidyverse)

# all SUBJID <-> USUBJID
table((data$DM %>% group_by(USUBJID, SUBJID) %>% tally())$n)
