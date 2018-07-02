## ----options, echo = FALSE----------------------------------------------------------------------------------------------------------------------------------------------
	
	library(knitr)
	opts_chunk$set(
		echo = TRUE, results = 'asis', warning = FALSE, 
		error = FALSE, message = FALSE, cache = FALSE,
		fig.width = 8, fig.height = 7,
		fig.path = "./figures_vignette/",
		#out.width = '0.7\\textwidth', 
		fig.align = 'center')#, out.height = 0.5\\textwidth', fig.path = 'graphs/') 
	options(width = 170)#, stringsAsFactors = FALSE
	options(warn = 1)#instead of warn = 0 by default -> to have place where warnings occur in the call to Sweave function
	

## ----loadPackages-------------------------------------------------------------------------------------------------------------------------------------------------------

	library(patientProfilesVis)
	library(pander)


## ----loadData-----------------------------------------------------------------------------------------------------------------------------------------------------------
	
	# example dataset(s) formatted as a list of data.frame
	data(sdtmDataPelican)
	pander(lapply(sdtmDataPelican, head, 1))
	
	# and corresponding labels
	data(labelVarsPelican)
	pander(head(labelVarsPelican))


## ----exampleTestModule1-------------------------------------------------------------------------------------------------------------------------------------------------

	# annotate subject demographics meta-data
	# by specifying a set of variables to include
	dmPlots <- subjectProfileTextPlot(
		data = sdtmDataPelican$DM,
		paramValueVar = c("SEX", "AGE", "RACE", "COUNTRY", "ARM"),
		labelVars = labelVarsPelican
	)
	

## ----exampleTestModule1-include, echo = FALSE, fig.height = getNLinesYGgplot(dmPlots[[1]])*0.14, fig.width = 14, fig.cap = paste("Demographic information with the 'subjectProfileTextPlot' function for patient:", names(dmPlots)[1])----

	print(dmPlots[[1]])


## ----exampleTestModule2-------------------------------------------------------------------------------------------------------------------------------------------------

	# annotate subject medical history
	# by specifying a combination of parameter value/name
	mhPlots <- subjectProfileTextPlot(
		data = sdtmDataPelican$MH,
		paramNameVar = "MHDECOD",
		paramValueVar = "MHOCCUR",
		title = "Medical History",
		labelVars = labelVarsPelican
	)
	

## ----exampleTestModule2-include, echo = FALSE, fig.height = getNLinesYGgplot(mhPlots[[1]])*0.14, fig.width = 14, fig.cap = paste("Medical history with the 'subjectProfileTextPlot' function for patient:", names(mhPlots)[1])----

	print(mhPlots[[1]])


## ----exampleIntervalModule----------------------------------------------------------------------------------------------------------------------------------------------

	# AEPTCD: preferred term code
	aePlots <- subjectProfileIntervalPlot(
		data = sdtmDataPelican$AE,
		paramVar = "AETERM",
		subjectVar = "USUBJID",
		startVar = "AESTDY",
		endVar = "AEENDY",
		colorVar = "AESEV",
		labelVars = labelVarsPelican
	)
	

## ----exampleIntervalModule-include, echo = FALSE, fig.height = getNLinesYGgplot(aePlots[[1]])*0.14, fig.width = 14, fig.cap = paste("Adverse events with the 'subjectProfileIntervalPlot' function for patient:", names(aePlots)[1])----

	print(aePlots[[1]])


## ----exampleEventModule-------------------------------------------------------------------------------------------------------------------------------------------------
	
	## laboratory data
	
	# prepare data for plots:
	dataLB <- sdtmDataPelican$LB
	# sort the categories (empty values '' becomes NA)
	dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH"))
	
	# create plot
	lbPlots <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		subjectVar = "USUBJID",
		timeVar = "LBDY",
		labelVars = labelVarsPelican
	)


## ----exampleEventModule-include, echo = FALSE, fig.height = getNLinesYGgplot(lbPlots[[1]])*0.14, fig.width = 14, fig.cap = paste("Laboratory data with the 'subjectProfileEventPlot' function for patient:", names(lbPlots)[1])----

	print(lbPlots[[1]])


## ----exampleEventModuleWithColor----------------------------------------------------------------------------------------------------------------------------------------

	# create plot
	lbPlotsColor <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		subjectVar = "USUBJID",
		timeVar = "LBDY",
		colorVar = "LBNRIND",
		labelVars = labelVarsPelican
	)
	

## ----exampleEventModuleWithColor-include, echo = FALSE, fig.height = getNLinesYGgplot(lbPlotsColor[[1]])*0.14, fig.width = 14, fig.cap = paste("Laboratory data with the 'subjectProfileEventPlot' function for patient:", names(lbPlotsColor)[1])----

	print(lbPlotsColor[[1]])


## ----createReportWithAllModules, eval = FALSE---------------------------------------------------------------------------------------------------------------------------
#  
#  	createSubjectProfileReport(
#  		listPlots = list(dmPlots, mhPlots, aePlots, lbPlotsColor)
#  	)
#  

## ----includeSessionInfo, echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------

	pander(sessionInfo())


