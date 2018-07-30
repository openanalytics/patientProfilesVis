#library(rmarkdown)
#library(tools)  
# DT, plotly
library(patientProfilesVis)
library(tools)
library(plyr)

# to increase the limit of size of 5MB for each uploaded file
options(shiny.maxRequestSize = 30*1024^2) 

serverFunction <- function(input, output, session) {
  
	# Advanced debugging
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
	
	results <- reactiveValues(listPlots = NULL)
  
	# load the imported data
	results$dataRes <- reactive({
        
		inFile <- input$dataFiles
        validate(need(inFile, "No data loaded"))
        
        data <- loadDataADaM(file.path(inFile$datapath))
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
	results$defaultModules <- reactive({
		defaultModules <- getDefaultModules(data = results$dataAll())
	})
	results$defaultModulesNames <- reactive(names(results$defaultModules()))
	observe({
		updateSelectInput(session, inputId = "selectedModules",
			choices = results$defaultModulesNames(),
			selected = results$defaultModulesNames()
		)
	})
  
  	# create widgets for user specification
	output$module <- renderUI({
				
		cat("Update entire module\n")		
				
		conditionalPanel(
#			condition = "input.createModule % 2 == 1",
			condition = "input.createDisplayModule % 2 == 1", {
			
			tagList(
				selectInput("moduleChoice", label = "Choose module", 
					choices = c("<none>" = "none", "New module", results$defaultModulesNames())
				),
				# module specification
				uiOutput("modulePanel")
			)
			
		})

	})

	results$currentModule <- reactive({
		validate(need(isTruthy(input$moduleChoice) && input$moduleChoice != "none", "Please choose a module."))		
		if(!input$moduleChoice %in% c("<none>", "New module"))
			results$defaultModules()[[input$moduleChoice]]
	})
	
	# create widget to specify dataset
	output$modulePanel <- renderUI({
		
		moduleDataAvailable <- if(!is.null(results$currentModule()))	results$currentModule()$dataName	else	results$datasets()	
		tagList(
			selectInput("moduleData", label = "Dataset", choices = moduleDataAvailable),
			uiOutput("moduleParamPanel")
		)
	})

	results$dataCurrent <- reactive({
		cat("Update current data\n")
		if(!is.null(input$moduleData))	results$dataAll()[[input$moduleData]]
	})
	results$variablesDataCurrent <- reactive(colnames(results$dataCurrent()))
	
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
				createWidgetVariable(inputId = "moduleGroupVar", label = "Column with grouping", optional = TRUE,
					selected = ifelse(!is.null(results$currentModule()) & !is.null(results$currentModule()$paramGroupVar), 
						results$currentModule()$paramGroupVar, "<none>"
					)
				),
				fluidRow(
					column(4, actionButton(inputId = "submitModule", label = "Submit module")),
					column(4, actionButton(inputId = "saveModule", label = "Save module"))
				)
			)
			
		})
		
	})

	# create widgets specific of certain module type
	output$moduleSpecificType <- renderUI({
				
		validate(need(input$moduleType, "moduleType"))
		
		# widgets common to the 'event' and 'interval' modules
		tagListCommonEventInterval <- list(
			createWidgetVariable(
				inputId = "moduleValueVar", 
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
					radioButtons("moduleTextVarSpecType", label = "",
						choices = list(
							"Column(s) with variable" = 1,
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
				inputId = "moduleTextValueVar", label = "Column with variable", 
				multiple = TRUE,
				selected = if(!is.null(results$currentModule()))	results$currentModule()$paramValueVar
			),
			'2' = list(
				createWidgetVariable(inputId = "moduleTextValueVarPair", label = "Column with variable value",
					selected = if(!is.null(results$currentModule()))	results$currentModule()$paramValueVar),
				createWidgetVariable(inputId = "moduleTextNameVarPair", label = "Column with variable name",
					selected = if(!is.null(results$currentModule()))	results$currentModule()$paramNameVar)
			)
		)	
	})

	# create the list of plot(s) for the specified module
	results$plotsCurrent <- eventReactive(input$submitModule, {
		
		listParams <- c(
			list(
				data = results$dataCurrent(),
				subjectVar = input$moduleSubjectVar,
				title = input$moduleTitle,
				label = input$moduleLabel,
				labelVars = results$labelVars(),
				paramGroupVar = input$moduleGroupVar
			),
			switch(input$moduleType,
				'text' = switch(input$moduleTextVarSpecType,
					'1' = list(paramValueVar = input$moduleTextValueVar),
					'2' = list(
						paramValueVar = input$moduleTextValueVarPair,
						paramNameVar = input$moduleTextNameVarPair
					)
				),
				'event' = list(
					paramVar = input$moduleValueVar,
					timeVar = input$moduleEventTimeVar,
					colorVar = input$moduleColorVar,
					shapeVar = input$moduleEventShapeVar
				),
				'interval' = list(
					paramVar = input$moduleValueVar,
					timeStartVar = input$moduleIntervalTimeStartVar,
					timeEndVar = input$moduleIntervalTimeEndVar,
					colorVar = input$moduleColorVar
				)
			)
		)
		# remove empty optional parameters
		listParams <- listParams[sapply(listParams, function(x) 
			!(length(x) == 1 && x == "none")
		)]
			
		# check if all parameter(s) are specified
		reqParam <- c("data", 
			switch(input$moduleType, 
				'text' = "paramValueVar",
				'event' = c("paramVar", "timeVar"),
				'interval' = c("paramVar", "timeStartVar", "timeEndVar")
			)
		)
		specParams <- sapply(listParams[reqParam], isTruthy)
		validate(need(all(specParams), "Some parameters are missing."))
		
		subjectProfileFct <- paste0("subjectProfile", simpleCap(input$moduleType), "Plot")
		do.call(subjectProfileFct, listParams)
						
	})

	# preview: fill the plot module
	output$moduleResults <- renderUI({
		
		validate(need(isTruthy(results$plotsCurrent()), "Plot not yet created."))
		
		tagList(	
				
			h3("Module preview"),
			selectInput(inputId = "subjectCurrent", label = "Select subject",
				choices = unique(results$dataCurrent()[, input$moduleSubjectVar])
			),
			plotOutput("plotSubject")
		
		)

	})

	# extract the plot for specified subject
	results$plotSubjectCurrent <- reactive({		
		validate(need(input$subjectCurrent, "subject"), need(results$plotsCurrent(), "plot"))
		results$plotsCurrent()[[input$subjectCurrent]]
	})
	
	# plot this plot in the 'preview' panel
	observe({	
		validate(need(
			expr = results$plotSubjectCurrent(), 
			message = paste0("No data is available for the specified module for subject: '", 
				input$subjectCurrent, "'.")
		))
		output$plotSubject <- renderPlot(
			expr = results$plotSubjectCurrent(),
			height = getNLinesYGgplot(results$plotSubjectCurrent()) * 30
		)
	})

	# save current module if requested
	observeEvent(input$saveModule, {
		validate(need(isTruthy(results$plotsCurrent), "Current module not valid."))
		cat("Save module.")
		results$listPlots <- c(isolate(results$listPlots), list(results$plotsCurrent()))
	})

	# create the report
	results$subjectProfileReport <- eventReactive(input$createSubjectProfileReport, {
    
		validate(need(length(results$listPlots) > 0, "No module(s) are saved yet."))

        withProgress(message = 'Create subject profile report..\n', 
				
            detail = "The analysis is running. A download button will 
                appear when the results are processed and available. (Please do 
                not press the 'Get Results' button multiple times)",
		
            style = "notification", value = NULL, {
            message(".. Create subject profile report....")
              
			potentialErrorMessage <- try(
				createSubjectProfileReport(
					listPlots = results$listPlots,
					outputFile = "subjectProfile.pdf",
					labelVars = results$labelVars()
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




