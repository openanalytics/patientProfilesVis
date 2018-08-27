#library(rmarkdown)
#library(tools)  
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
		
		cat("reset plotsCurrent")
		results$plotsCurrent <- NULL
		
        return(data)
        
	})

	# store uploaded data, variable labels and dataset
	results$dataAll <- reactive(results$dataRes()) # list of data.frame
	results$labelVars <- reactive(attributes(results$dataRes())$labelVars) # named vector with variable labels
	results$datasets <- reactive(names(results$dataAll()))
	
	# create default modules for uploaded datasets
	observe({
		results$availableModules <- getDefaultModules(data = results$dataAll())
	})
	results$defaultModulesNames <- reactive(names(results$availableModules))
  
	## Module specification:
	output$module <- renderUI({
					
		validate(need(results$dataRes(), "Please upload some data."))
				
#		cat("Update entire module\n")
			
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
		validate(need(isTruthy(input$moduleChoice) && input$moduleChoice != "none", 
			"To create a new module or display existing module(s), please choose a module.")
		)		
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

	# save selected dataset
	results$dataCurrent <- reactive({
		if(!is.null(input$moduleData))	results$dataAll()[[input$moduleData]]
	})
	# and column names (variables)
	results$variablesDataCurrent <- reactive({
		getVarLabelsForUI(data = results$dataCurrent(), labelVars = results$labelVars())
	})
	# extract possible time variable (should be numeric)
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

	# create widgets for module specification
	output$moduleParamPanel <- renderUI({
				
		validate(need(input$moduleData, "Please specify a dataset."))		
			
#		cat("Update module param")
		
#		isolate({
				
			tagList(

				selectInput(
					"moduleType", label = "Type", 
					choices = c("text", "event", "interval", "line"),
					selected = if(!is.null(results$currentModule()))	results$currentModule()$type	else	"text"
				),
				
				textInput("moduleTitle", label = "Title", 
					value = ifelse(!is.null(results$currentModule()), results$currentModule()$title, "")
				),
				helpText("The title is also used to uniquely identify a specified module in the interface."),
#				textInput("moduleLabel", label = "Label", 
#					value = ifelse(!is.null(results$currentModule()), results$currentModule()$label, "")
#				),
#				helpText("The label is used to uniquely identify a specified module when combining modules."),
				createWidgetVariable(
					inputId = "moduleSubjectVar",
					label = "Column with subject identifier",
					selected = ifelse(!is.null(results$currentModule()), results$currentModule()$subjectVar,
						ifelse("USUBJID" %in% results$variablesDataCurrent(), "USUBJID", results$variablesDataCurrent()[1])
					)
				),
				# subset of interest
				fluidRow(
					column(6, 
						createWidgetVariable(inputId = "moduleSubsetVar", label = "Filter data based on:", optional = TRUE,
							selected = ifelse(!is.null(results$currentModule()) & !is.null(results$currentModule()$subsetVar), 
								results$currentModule()$subsetVar, "<none>"
							)
						)
					),
					column(6, uiOutput("moduleSubsetValuePanel"))
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
			
#		})
		
	})
	
	# create subsetValue parameter
	output$moduleSubsetValuePanel <- renderUI({
				
		validate(need(input$moduleSubsetVar, "moduleSubsetVar"))
		
		subsetValues <- if(!is.null(results$currentModule()) & !is.null(results$currentModule()$subsetValue)){ 
			results$currentModule()$subsetValue
		}else{
			unique(results$dataCurrent()[[input$moduleSubsetVar]])
		}
		
		createWidgetVariable(
			inputId = "moduleSubsetValue", 
			label = "with group(s) of interest:", optional = FALSE,
			choices = subsetValues, selected = subsetValues[1],
			multiple = TRUE
		)
		
	})

	# create widgets specific of certain module type
	output$moduleSpecificType <- renderUI({
				
#		cat("module specific type")		
				
		validate(need(input$moduleType, "moduleType"))
		
		# widgets common to the 'event' and 'interval' modules
		widgetParamVar <- createWidgetVariable(
			inputId = "moduleParamVar", 
			label = "Column with variable(s)", 
			multiple = TRUE,
			selected = if(!is.null(results$currentModule()))	results$currentModule()$paramVar
		)
		widgetColorVar <- createWidgetVariable(
			inputId = "moduleColorVar", 
			label = "Column with variable used for color",
			optional = TRUE,
			selected = if(!is.null(results$currentModule()))	results$currentModule()$colorVar
		)		
		widgetTimeVar <- createWidgetVariable(
			inputId = "moduleTimeVar", 
			label = "Column with time variable",
			choices = results$variablesTimeDataCurrent(),
			selected = if(!is.null(results$currentModule()))	results$currentModule()$timeVar
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
				list(widgetTimeVar, widgetParamVar, widgetColorVar),
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
				list(widgetParamVar, widgetColorVar)
			),
			
			'line' = {
				list(
					widgetTimeVar, 
					createWidgetVariable(inputId = "moduleLineParamValueVar", 
						label = "Column with parameter value",
						selected = if(!is.null(results$currentModule()))	
							results$currentModule()$paramValueVar
					),
					createWidgetVariable(inputId = "moduleLineParamNameVar",
						label = "Column with parameter name",
						selected = if(!is.null(results$currentModule()))	
							results$currentModule()$paramNameVar
					),
					createWidgetVariable(inputId = "moduleLineParamValueRangeVar",
						label = "Columns with reference range (minimum and maximum)",
						selected = if(!is.null(results$currentModule()))	
							results$currentModule()$paramValueRangeVar,
						multiple = TRUE
					),
					widgetColorVar
				)
				
			})
		
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
			plotCurrent <- createSubjectProfileFromShinyInput(input = input, results = results)
		, silent = TRUE)

		# Note: reset the plotsCurrent to NULL in case plot already created
		results$plotsCurrent <- if(inherits(plotCurrentError, "try-error")){
			output$moduleSaveMessage <- renderUI(
				div(strong(paste("The patient profiles cannot be created:", attr(plotCurrentError, "condition")$message)), 
				style = "color:red")
			)
			NULL
		}else if(!input$moduleTitle %in% names(results$listPlots)){
			output$moduleSaveMessage <- renderUI(
				div(strong("Title already used, please specify a different title."), 
					style = "color:red")
			)
			NULL
		}else{
			output$moduleSaveMessage <- renderUI(
				div("The patient profiles have been created for the specified module, you can preview them in the right panel.", 
					style = "color:green")
			)
			plotCurrent
		}
		
	})

	# preview: fill the plot module
	output$moduleResults <- renderUI({
		
		validate(need(isTruthy(results$plotsCurrent), "No subject profile plot has been created yet."))
		
		tagList(	
				
			h3("Module preview"),
			fluidRow(
				column(6, 
					selectInput(inputId = "previewSubject", label = "Select subject",
						choices = unique(results$dataCurrent()[, input$moduleSubjectVar])
					)
				),
				column(6, uiOutput("previewPagePanel"))
			),
			uiOutput("previewMessage"),
			plotOutput("previewPlotSubject")		
		)

	})

	# extract the plot for specified subject
	results$plotSubjectCurrent <- reactive({		
		validate(need(input$previewSubject, "subject"), need(results$plotsCurrent, "plot"))
		results$plotsCurrent[[input$previewSubject]]
	})
	
	# print message preview and extract pages
	observe({	
		plotSubjectError <- try(
			previewPlotSuject <- results$plotSubjectCurrent()
		, silent = TRUE)
		subject <- input$previewSubject
		isolate({
			if(inherits(plotSubjectError, "try-error")){
				output$previewMessage <- renderUI(
					div(strong(paste0("The patient profile for the subject : '", subject, "'cannot be displayed: ", 
						attr(plotSubjectError, "condition")$message)), 
						style = "color:red"
					)
				)
			}else{
				if(is.null(previewPlotSuject)){
					output$previewMessage <- renderUI(
						div(strong(
							paste0("No data is available for the specified module for subject: '", 
								subject, "', please select a different subject.")
							), style = "color:red")
						)
				}else{			
					output$previewMessage <- renderUI(
						div(
							paste0("Please find a preview of the specified module for subject: '", 
								input$previewSubject, "'."), 
						style = "color:green")
					)
					output$previewPagePanel <- renderUI(
						selectInput(
							inputId = "previewPage", label = "page",
							choices = seq_len(length(previewPlotSuject))
						)
					)
				}
			}	
		})
	})
	
	# print the plot in the preview panel
	observe({
		validate(
			need(input$previewPage, "selected page"),
			need(results$plotSubjectCurrent(), "plot for current subject")
		)
		plot <- results$plotSubjectCurrent()[[input$previewPage]]
		output$previewPlotSubject <- renderPlot(
			expr = plot, height = getNLinesYGgplot(plot) * 20
		)
	})	

	## Save module
	
	# create the list of plot(s) for the specified module
	observeEvent(input$previewModule, {
				
		plotCurrentError <- try(
			plotCurrent <- createSubjectProfileFromShinyInput(input = input, results = results)
			, silent = TRUE)
		results$plotsCurrent <- if(inherits(plotCurrentError, "try-error")){
			output$moduleSaveMessage <- renderUI(
				div(strong(paste("The patient profiles cannot be created:", attr(plotCurrentError, "condition")$message)), 
					style = "color:red")
			)
			NULL
		}else{
			output$moduleSaveMessage <- renderUI(
				div("The patient profiles have been created for the specified module, you can preview them in the right panel.", 
					style = "color:green")
			)
			plotCurrent
		}
		
	})

	# save current module if requested
	observeEvent(input$saveModule, {
			
		if(!isTruthy(results$plotsCurrent)){
			output$moduleSaveMessage <- renderUI(
				div(strong("Please preview first your specified module."), 
					style = "color:red"))
		}else if(!is.null(input$moduleTitle) && 
			length(results$listPlots > 0) && 
			# by default the label is set to the title
			input$moduleTitle %in% sapply(results$listPlots, function(x) attr(x, "label"))){
			output$moduleSaveMessage <- renderUI(
				div(strong("Title already used, please specify a different title."), 
					style = "color:red"))
		}else{
			
			currentPlotName <- paste0(input$moduleTitle, " (custom, ", input$moduleType, ")")
			
			# save the plot (because already created)
			currentPlot <- setNames(list(results$plotsCurrent), currentPlotName)
			results$listPlots <- c(results$listPlots, currentPlot)	
			
			# save the input parameters (to be able to preview plot later on):
			newModule <- setNames(list(getUIParamModule(input)), currentPlotName)
			results$availableModules <- c(results$availableModules, newModule)
			
			# update progress message(s)
			output$moduleMessage <- renderUI(
				div(paste0("Module has been saved, you can preview it by selecting:", br(), 
					"'", names(newModule), "' in the selection box below."), 
					style = "color:green")
			)
			output$moduleSaveMessage <- previewMessage <- renderUI("")
			
			# delete current plot
			results$plotsCurrent <- NULL
			
		}
		
	})

	## Report creation
	output$reportCreation <- renderUI({
				
		validate(need(results$dataRes(), "Please upload some data."))		
				
		tagList(
			selectInput(inputId = "reportSelectedModules",
				label = "Selected modules", multiple = TRUE,
				choices = results$defaultModulesNames(),
				selected = results$defaultModulesNames()
			),
			fluidRow(
				column(6,
					selectInput(inputId = "reportSubjectSortData",
						label = "Sort subjects based on dataset:", multiple = FALSE,
						choices = c('<none>' = 'none', results$datasets())
					)
				),
				column(6, uiOutput("reportSubjectSortVarPanel"))
			),
			fluidRow(
				actionButton(inputId = "createSubjectProfileReport", label = "Create subject profile report"),
				br(),
				uiOutput("downloadSubjectProfileReportPanel")
			),
			hr(),
			bookmarkButton()
		)		
				
	})

	# extract the possible variable to sort subject by
	output$reportSubjectSortVarPanel <- renderUI({
		validate(need(input$reportSubjectSortData, "reportSubjectSortData"))
		
		reportSubjectSortVars  <- if(input$reportSubjectSortData != "none")
			getVarLabelsForUI(
				data = results$dataAll()[[input$reportSubjectSortData]], 
				labelVars = results$labelVars()
			)
		selectInput(
			inputId = "reportSubjectSortVar", 
			label = "with variable:", multiple = FALSE,
			choices = reportSubjectSortVars, selected = reportSubjectSortVars[1]
		)
	})

	# create the report
	results$subjectProfileReport <- eventReactive(input$createSubjectProfileReport, {
    
		validate(need(input$reportSelectedModules, "No module(s) are selected yet."))

        withProgress(message = 'Create subject profile report..\n', 
				
            detail = "The analysis is running. A download button will 
                appear when the results are processed and available. (Please do 
                not press the 'Get Results' button multiple times)",
		
            style = "notification", value = 0, {
            message("... Create subject profile report ...")
			
				## extract the plots
						
				# plots created with the display button
				listPlots <- results$listPlots 
				
				# default plots (not yet created)
				selModNotCreated <- setdiff(input$reportSelectedModules, names(listPlots))
				if(length(selModNotCreated) > 0){
					incProgress(amount = 0.2, 
						detail = "Create module(s) not previewed in the interface"
					)
					listPlotsDefaults <- sapply(
						results$availableModules[selModNotCreated], 
						createSubjectProfileFromParam, 
						data = results$dataAll(), 
						labelVars = results$labelVars(),
						simplify = FALSE
					)
					listPlots <- c(listPlots, listPlotsDefaults)
				}	
	              
				potentialErrorMessage <- try(
					createSubjectProfileReport(
						listPlots = listPlots,
						outputFile = "subjectProfile.pdf",
						labelVars = results$labelVars(),
						subjectSortData = results$dataAll()[[input$reportSubjectSortData]],
						subjectSortVar = input$reportSubjectSortVar,
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




