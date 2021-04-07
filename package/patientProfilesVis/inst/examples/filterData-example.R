library(clinUtils)

data(dataSDTMCDISCP01)
dataAll <- dataSDTMCDISCP01

# keep only a subset of subjects
# (e.g. to visualize specified patient profiles
# before creating them for all subject)
filterData(
	data = dataAll$AE,
	subjectSample = 2
)

# filter based on specified variable/value:
# only adverse events possibly related
filterData(
	data = dataAll$AE,
	subsetVar = "AEREL",
	subsetValue = "POSSIBLE"
)

# filter based on a different dataset:
# keep only adverse events for subjects in a specific treatment arm
filterData(
	data = dataAll$AE,
	subsetData = dataAll$DM,
	subsetVar = "ACTARM",
	subsetValue = "Placebo"
)

# filter based on subjects of interest
filterData(
	data = dataAll$AE,
	subjectSubset = c("01-701-1148", "01-701-1211")
)

