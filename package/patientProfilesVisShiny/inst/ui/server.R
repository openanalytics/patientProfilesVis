#library(rmarkdown)
#library(tools)  
# DT, plotly
library(patientProfilesVis)
library(tools)
library(plyr)

# to increase the limit of size of 5MB for each uploaded file
options(shiny.maxRequestSize = 30*1024^2) 

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
	
	results <- reactiveValues(listPlots = NULL, availableModules = NULL, plotsCurrent = NULL)
  
	## Data
	
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
		
        return(data)
        
	})

	# store uploaded data, variable labels and dataset
	results$dataAll <- reactive(results$dataRes()) # list of data.frame
	results$labelVars <- reactive(attributes(results$dataRes())$labelVars) # named vector with variable labels
	results$datasets <- reactive(names(results$dataAll()))
	
	# create default modules for uploaded datasets
	observe({
		cat("Update available modules")
		results$availableModules <- getDefaultModules(data = results$dataAll())
	})
	results$defaultModulesNames <- reactive(names(results$availableModules))
  
	## Module specification:
	output$module <- renderUI({
					
		validate(need(results$dataRes(), "Please upload some data."))
				
		cat("Update entire module\n")
			
		tagList(
			uiOutput("moduleMessage"),
			selectInput("moduleChoice", label = "Choose module", 
				choices = c("<none>" = "none", "New module", results$defaultModulesNames())
			),
			# module specification
			uiOutput("modulePanel")
		)

	})

	# currently selected module
	results$currentModule <- reactive({
		validate(need(isTruthy(input$moduleChoice) && input$moduleChoice != "none", "Please choose a module."))		
		if(!input$moduleChoice %in% c("<none>", "New module"))
			results$availableModules[[input$moduleChoice]]
	})
	
	# create widget to specify dataset
	output$modulePanel <- renderUI({
		moduleDataAvailable <- if(!is.null(results$currentModule()))	results$currentModule()$data	else	results$datasets()	
		tagList(
			selectInput("moduleData", label = "Dataset", choices = moduleDataAvailable),
			uiOutput("moduleParamPanel")
		)
	})

	results$dataCurrent <- reactive({
		cat("Update current data\n")
		if(!is.null(input$moduleData))	results$dataAll()[[input$moduleData]]
	})
	results$variablesDataCurrent <- reactive({
		vars <- colnames(results$dataCurrent())
		names(vars) <- paste0(getLabelVar(var = vars, labelVars = results$labelVars()), " (", vars, ")")
		vars
	})
	
	results$variablesTimeDataCurrent <- reactive(
		names(which(unlist(colwise(is.numeric)(results$dataCurrent()))))
	)

	# custom wrapper for selectInput based on column names
	createWidgetVariable <- function(..., optional = FALSE, multiple = FALSE, 
		choices = c(
			if(optional)	c('<none>' = 'none'), 
			results$variablesDataCurrent())
		)
		selectInput(..., choices = choices, multiple = multiple)

	output$moduleParamPanel <- renderUI({
				
		validate(need(input$moduleData, "Please specify a dataset."))		
			
		isolate({
				
			tagList(

				selectInput(
					"moduleType", label = "Type", 
					choices = c("text", "event", "interval"),
					selected = if(!is.null(results$currentModule()))	results$currentModule()$type	else	"text"
				),
				
				textInput("moduleTitle", label = "Title", 
					value = ifelse(!is.null(results$currentModule()), results$currentModule()$title, "")
				),
				textInput("moduleLabel", label = "Label", 
					value = ifelse(!is.null(results$currentModule()), results$currentModule()$label, "")
				),
				createWidgetVariable(
					inputId = "moduleSubjectVar",
					label = "Column with subject identifier",
					selected = ifelse(!is.null(results$currentModule()), results$currentModule()$subjectVar,
						ifelse("USUBJID" %in% results$variablesDataCurrent(), "USUBJID", results$variablesDataCurrent()[1])
					)
				),
				uiOutput("moduleSpecificType"),
				createWidgetVariable(inputId = "moduleParamGroupVar", label = "Column with grouping", optional = TRUE,
					selected = ifelse(!is.null(results$currentModule()) & !is.null(results$currentModule()$paramGroupVar), 
						results$currentModule()$paramGroupVar, "<none>"
					)
				),
				fluidRow(
					column(4, actionButton(inputId = "previewModule", label = "Preview module")),
					column(4, actionButton(inputId = "saveModule", label = "Save module"))
				),
				uiOutput("moduleSaveMessage")
			)
			
		})
		
	})

	# create widgets specific of certain module type
	output$moduleSpecificType <- renderUI({
				
		validate(need(input$moduleType, "moduleType"))
		
		# widgets common to the 'event' and 'interval' modules
		tagListCommonEventInterval <- list(
			createWidgetVariable(
				inputId = "moduleParamVar", 
				label = "Column with variable(s)", 
				multiple = TRUE,
				selected = if(!is.null(results$currentModule()))	results$currentModule()$paramVar
			),
			createWidgetVariable(
				inputId = "moduleColorVar", 
				label = "Column with variable used for color",
				optional = TRUE,
				selected = if(!is.null(results$currentModule()))	results$currentModule()$colorVar
			)
		)
		
		switch(input$moduleType,
				
			'text' = {
				selected <- ifelse(
					!is.null(results$currentModule()),
					ifelse(!is.null(results$currentModule()$paramNameVar), 2, 1),
					1
				)
				list(
					radioButtons("moduleTextVarSpecType", label = "Variable(s) specification",
						choices = list(
							"Column(s) with parameter" = 1,
							"Pair of columns with parameter name/value" = 2
						), selected = selected
					),
					uiOutput("moduleTextVarPanel")
				)
			},
			'event' = c(
				list(
					createWidgetVariable(
						inputId = "moduleEventTimeVar", 
						label = "Column with time variable",
						choices = results$variablesTimeDataCurrent(),
						selected = if(!is.null(results$currentModule()))	results$currentModule()$timeVar
					)
				),
				tagListCommonEventInterval,
				list(
					createWidgetVariable(
						inputId = "moduleEventShapeVar", 
						label = "Column with variable used for symbol",
						optional = TRUE,
						selected = if(!is.null(results$currentModule()))	results$currentModule()$shapeVar
					)
				)
			),
			
			'interval' = c(
				list(
					createWidgetVariable(
						inputId = "moduleIntervalTimeStartVar", 
						label = "Column with start time variable",
						choices = results$variablesTimeDataCurrent(),
						selected = if(!is.null(results$currentModule()))	results$currentModule()$timeStartVar
					),
					createWidgetVariable(
						inputId = "moduleIntervalTimeEndVar", 
						label = "Column with end time variable",
						choices = results$variablesTimeDataCurrent(),
						selected = if(!is.null(results$currentModule()))	results$currentModule()$timeEndVar
					)
				),
				tagListCommonEventInterval
			)
		)
		
	})

	output$moduleTextVarPanel <- renderUI({
				
		validate(need(input$moduleTextVarSpecType, "moduleTextVarSpecType"))
		
		switch(input$moduleTextVarSpecType,
			'1' = createWidgetVariable(
				inputId = "moduleTextParamValueVar", label = "Column(s) with parameter", 
				multiple = TRUE,
				selected = if(!is.null(results$currentModule()))	results$currentModule()$paramValueVar
			),
			'2' = fluidRow(
				column(6, 
					createWidgetVariable(inputId = "moduleTextParamValueVarPair", 
						label = "Column with parameter value",
						selected = if(!is.null(results$currentModule()))	
							results$currentModule()$paramValueVar)
				),
				column(6, createWidgetVariable(inputId = "moduleTextParamNameVarPair",
					label = "Column with parameter name",
					selected = if(!is.null(results$currentModule()))	results$currentModule()$paramNameVar)
				)
			)
		)	
	})

	## Preview module (plot creation)

	# create the list of plot(s) for the specified module
	observeEvent(input$previewModule, {
				
		plotCurrentError <- try(
			plotCurrent <- createSubjectProfileUI(input = input, results = results)
		, silent = TRUE)

		if(inherits(plotCurrentError, "try-error")){
			output$moduleSaveMessage <- renderUI(
				div(strong(paste("The patient profiles cannot be created:", attr(plotCurrentError, "condition")$message)), 
				style = "color:red")
			)
		}else if(!input$moduleLabel %in% names(results$listPlots)){
			output$moduleSaveMessage <- renderUI(
				div(strong("Label already used, please specify a different label."), 
					style = "color:red")
			)
		}else{
			results$plotsCurrent <- plotCurrent
			output$moduleSaveMessage <- renderUI(
				div("The patient profiles have been created for the specified module, you can preview them in the right panel.", 
					style = "color:green")
			)
		}
		
	})

	# preview: fill the plot module
	output$moduleResults <- renderUI({
		
		validate(need(isTruthy(results$plotsCurrent), "No subject profile plot has been created yet."))
		
		tagList(	
				
			h3("Module preview"),
			selectInput(inputId = "subjectCurrent", label = "Select subject",
				choices = unique(results$dataCurrent()[, input$moduleSubjectVar])
			),
			uiOutput("moduleResultsMessage"),
			plotOutput("plotSubject")		
		)

	})

	# extract the plot for specified subject
	results$plotSubjectCurrent <- reactive({		
		validate(need(input$subjectCurrent, "subject"), need(results$plotsCurrent, "plot"))
		results$plotsCurrent[[input$subjectCurrent]]
	})
	
	# plot this plot in the 'preview' panel
	observe({	
		currentPlot <- results$plotSubjectCurrent()
		isolate({
			if(is.null(currentPlot)){
				output$moduleResultsMessage <- renderUI(
					div(strong(
						paste0("No data is available for the specified module for subject: '", 
							input$subjectCurrent, "', please select a different subject.")
						), style = "color:red")
					)
			}else{
				output$moduleResultsMessage <- renderUI(
					div(
						paste0("Please find a preview of the specified module for subject: '", 
							input$subjectCurrent, "'."), 
					style = "color:green")
				)
				output$plotSubject <- renderPlot(
					expr = results$plotSubjectCurrent(),
					height = getNLinesYGgplot(results$plotSubjectCurrent()) * 30
				)	
			}	
		})
	})

	## Save module
	
	# create the list of plot(s) for the specified module
	observeEvent(input$previewModule, {
				
		plotCurrentError <- try(
			plotCurrent <- createSubjectProfileUI(input = input, results = results)
			, silent = TRUE)
		if(inherits(plotCurrentError, "try-error")){
			output$moduleSaveMessage <- renderUI(
				div(strong(paste("The patient profiles cannot be created:", attr(plotCurrentError, "condition")$message)), 
					style = "color:red")
			)
		}else{
			results$plotsCurrent <- plotCurrent
			output$moduleSaveMessage <- renderUI(
				div("The patient profiles have been created for the specified module, you can preview them in the right panel.", 
					style = "color:green")
			)
		}
		
	})

	# save current module if requested
	observeEvent(input$saveModule, {
			
		if(!isTruthy(results$plotsCurrent)){
			output$moduleSaveMessage <- renderUI(
				div(strong("Please preview first your specified module."), 
					style = "color:red"))
		}else if(!is.null(input$moduleLabel) && input$moduleLabel %in% names(results$listPlots)){
			output$moduleSaveMessage <- renderUI(
				div(strong("Label already used, please specify a different label."), 
					style = "color:red"))
		}else{
			
			# save the plot
			results$listPlots <- c(results$listPlots, setNames(list(results$plotsCurrent), input$moduleLabel))	
			
			# save the parameters:
			newModule <- list(getUIParamModule(input))
			names(newModule) <- paste0(input$moduleTitle, " (custom, ", input$moduleType, ")")
			results$availableModules <- c(results$availableModules, newModule)
			
			output$moduleMessage <- renderUI(
				div(paste0("Module has been saved, you can preview it by selecting: '",
					names(newModule), "' in the selection box below."), 
					style = "color:green")
			)
			
			output$moduleSaveMessage <- moduleResultsMessage <- renderUI("")
			
			# delete current plot
			results$plotsCurrent <- NULL
			
		}
		
	})

	## Report creation
	
	# specified module

	# create the report
	results$subjectProfileReport <- eventReactive(input$createSubjectProfileReport, {
    
		validate(need(length(results$listPlots) > 0, "No module(s) are saved yet."))

        withProgress(message = 'Create subject profile report..\n', 
				
            detail = "The analysis is running. A download button will 
                appear when the results are processed and available. (Please do 
                not press the 'Get Results' button multiple times)",
		
            style = "notification", value = 0, {
            message("... Create subject profile report ...")
              
			potentialErrorMessage <- try(
				createSubjectProfileReport(
					listPlots = results$listPlots,
					outputFile = "subjectProfile.pdf",
					labelVars = results$labelVars(),
				 	shiny = TRUE
				),
				silent = TRUE
			)
              
		})
        
	})
  
	# give the report when clicking on the download button
	output$downloadSubjectProfileReport <- downloadHandler(
		filename = "subjectProfile.pdf",
		content = function(file)	file.copy("subjectProfile.pdf", file),
      	contentType = "application/pdf"
  	)

	# make the download button available
	output$downloadSubjectProfileReportPanel <- renderUI({
        validate(
			need(
				!inherits(results$subjectProfileReport(), "try-error"), 
				paste("Issue during creation of subject profile report: ", 
					results$subjectProfileReport(), "."
				)
			)
		)
        downloadButton("downloadSubjectProfileReport", label = "Download subject profile report")
	})
  
}




