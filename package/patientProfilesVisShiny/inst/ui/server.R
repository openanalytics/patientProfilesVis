#library(rmarkdown)
#library(tools)  
library(patientProfilesVis)
library(tools)
library(plyr)

# to increase the limit of size of 5MB for each uploaded file to 10Gbs
options(shiny.maxRequestSize = 10*10^3*1024^2) 

serverFunction <- function(input, output, session) {
  
	## Advanced debugging
	
	observe({
				
		if (is.null(input$debug_console))
			return(NULL)
		
		if (input$debug_console > 0) {
			
			options(browserNLdisabled = TRUE)
			saved_console <- ".RDuetConsole"
			if (file.exists(saved_console)) {load(saved_console)}
			isolate(browser())
			save(file = saved_console, list = ls(environment()))
			
		}
		
	})
	
	results <- reactiveValues(
		listPlots = NULL, 
		availableModules = NULL, 
		plotsCurrent = NULL,
		modulePreDefinedID = file_path_sans_ext(list.files(outputPath))
	)
  
	## Data
	source(file.path("./serverFiles", "serverLoadData.R"), local = TRUE)
	
	## Module specification:
	source(file.path("./serverFiles", "serverModuleSettings.R"), local = TRUE)
	source(file.path("./serverFiles", "serverModuleSpecification.R"), local = TRUE)
	
	## Preview module (plot creation)
	source(file.path("./serverFiles", "serverPreviewSaveModule.R"), local = TRUE)

	## Report creation
	source(file.path("./serverFiles", "serverCreateReport.R"), local = TRUE)
  
}




