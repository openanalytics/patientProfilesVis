# Shiny app: server script to load the data
# 
# Author: Laure Cougnaud
###############################################################################


# load the imported data
results$dataRes <- reactive({
			
	inFile <- input$dataFiles
	validate(need(inFile, "No data loaded"))
	
	data <- loadDataADaMSDTM(file.path(inFile$datapath))
	# uploaded file are renamed by Shiny,
	# this set the original dataset name(s) to the list names
	initFileNames <- toupper(
		file_path_sans_ext(
			inFile[
				match(names(data), file_path_sans_ext(basename(inFile$datapath))),
				"name"
			]
		)
	)
	names(data) <- initFileNames
	
	cat("reset plotsCurrent")
	results$plotsCurrent <- NULL
	
	return(data)
	
})

# store uploaded data, variable labels and dataset
results$dataAll <- reactive(results$dataRes()) # list of data.frame
results$labelVars <- reactive(attributes(results$dataRes())$labelVars) # named vector with variable labels
results$datasets <- reactive(names(results$dataAll()))
