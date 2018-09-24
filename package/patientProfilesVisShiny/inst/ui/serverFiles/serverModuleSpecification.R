# Shiny app: server script to specify module specification
# 
# Author: Laure Cougnaud
###############################################################################

# select a module
output$moduleSettingsGeneralPanel <- renderUI({
	tagList(
		uiOutput("moduleMessage"),
		selectInput("moduleChoice", label = "Choose module", 
			choices = c("<none>" = "none", "New module", results$defaultModulesNames())
		),
		# module specification
		uiOutput("modulePanel")
	)
})

# extract the module currently selected
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
results$variablesTimeDataCurrent <- reactive(getTimeVars(data = results$dataCurrent(), labelVars = results$labelVars()))

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
#		textInput("moduleLabel", label = "Label", 
#			value = ifelse(!is.null(results$currentModule()), results$currentModule()$label, "")
#			),
#		helpText("The label is used to uniquely identify a specified module when combining modules."),
		createWidgetVariable(
			inputId = "moduleSubjectVar",
			label = "Subject identifier",
			selected = ifelse(!is.null(results$currentModule()), results$currentModule()$subjectVar,
				ifelse("USUBJID" %in% results$variablesDataCurrent(), "USUBJID", results$variablesDataCurrent()[1])
			)
		),
		# subset of interest
		fluidRow(
			column(6, 
				createWidgetVariable(inputId = "moduleSubsetVar", label = "Selection variable", optional = TRUE,
					selected = ifelse(!is.null(results$currentModule()) & !is.null(results$currentModule()$subsetVar), 
						results$currentModule()$subsetVar, "<none>"
					)
				)
			),
			column(6, uiOutput("moduleSubsetValuePanel"))
		),
		uiOutput("moduleSpecificType"),
		createWidgetVariable(inputId = "moduleParamGroupVar", label = "Grouping variable", optional = TRUE,
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

# create subsetValue parameter
output$moduleSubsetValuePanel <- renderUI({
			
	validate(need(input$moduleSubsetVar, "moduleSubsetVar"))
	
	if(!is.null(results$currentModule()) & !is.null(results$currentModule()$subsetValue)){ 
		selectedValues <- subsetValues <- results$currentModule()$subsetValue
	}else{
		subsetValues <- unique(results$dataCurrent()[[input$moduleSubsetVar]])
		selectedValues <- subsetValues[1]
	}
	
	createWidgetVariable(
		inputId = "moduleSubsetValue", 
		label = "Group(s) of interest", optional = FALSE,
		choices = subsetValues, 
		selected = selectedValues, 
		multiple = TRUE
	)
	
})

# create widgets specific of certain module type
output$moduleSpecificType <- renderUI({	
			
	validate(need(input$moduleType, "moduleType"))
	
	# widgets common to the 'event' and 'interval' modules
	widgetParamVar <- createWidgetVariable(
		inputId = "moduleParamVar", 
		label = "Parameter variable(s)", 
		multiple = TRUE,
		selected = if(!is.null(results$currentModule()))	results$currentModule()$paramVar
	)
	widgetColorVar <- createWidgetVariable(
		inputId = "moduleColorVar", 
		label = "Color variable",
		optional = TRUE,
		selected = if(!is.null(results$currentModule()))	results$currentModule()$colorVar
	)		
	widgetTimeVar <- createWidgetVariable(
		inputId = "moduleTimeVar", 
		label = "Time variable",
		choices = results$variablesTimeDataCurrent(),
		selected = if(!is.null(results$currentModule()))	results$currentModule()$timeVar
	)
	
	switch(input$moduleType,
			
		'text' = {
			selected <- ifelse(
				!is.null(results$currentModule()),
				as.numeric(results$currentModule()$varSpecType),
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
					label = "Symbol variable",
					optional = TRUE,
					selected = if(!is.null(results$currentModule()))	results$currentModule()$shapeVar
				)
			)
		),
		
		'interval' = list(
			fluidRow(
				column(6, 
					createWidgetVariable(
						inputId = "moduleIntervalTimeStartVar", 
						label = "Start time variable",
						choices = results$variablesTimeDataCurrent(),
						selected = if(!is.null(results$currentModule()))	results$currentModule()$timeStartVar
					)
				),
				column(6,
					createWidgetVariable(
						inputId = "moduleIntervalTimeEndVar", 
						label = "End time variable",
						choices = results$variablesTimeDataCurrent(),
						selected = if(!is.null(results$currentModule()))	results$currentModule()$timeEndVar
					)
				)
			),
			selectInput(
				inputId = "moduleIntervalTimeLimSelect",
				label = "Time limits",
				choices = c("fixed", "subject-specific"),
				selected = if(!is.null(results$currentModule()))	results$currentModule()$timeLimSelect
			),
			uiOutput("moduleIntervalTimeLimPanel"),
			widgetParamVar, 
			widgetColorVar
		),
		
		'line' = {
			list(
				widgetTimeVar, 
				createWidgetVariable(inputId = "moduleLineParamValueVar", 
					label = "Parameter value variable",
					selected = if(!is.null(results$currentModule()))	
						results$currentModule()$paramValueVar
				),
				createWidgetVariable(inputId = "moduleLineParamNameVar",
					label = "Parameter name variable",
					selected = if(!is.null(results$currentModule()))	
						results$currentModule()$paramNameVar
				),
				createWidgetVariable(inputId = "moduleLineParamValueRangeVar",
					label = "Reference range (minimum and maximum) variable",
					selected = if(!is.null(results$currentModule()))	
						results$currentModule()$paramValueRangeVar,
					multiple = TRUE
				),
				widgetColorVar,
				helpText("This variable is used to color the points.")
			)
			
		})
	
})

output$moduleIntervalTimeLimPanel <- renderUI({		
			
	validate(need(input$moduleIntervalTimeLimSelect, "moduleTextVarSpecType"))
	
	switch(input$moduleIntervalTimeLimSelect,
			
		'fixed' = {
			currentData <- results$dataCurrent()
			validate(
				need(
					isTruthy(input$moduleIntervalTimeStartVar) &
					isTruthy(input$moduleIntervalTimeEndVar) &
					all(c(input$moduleIntervalTimeStartVar, input$moduleIntervalTimeEndVar) %in%
						names(currentData))
					, ""
				)
			)
			
			if(isTruthy(input$moduleSubsetVar) & isTruthy(input$moduleSubsetValue))
				currentData <- currentData[which(currentData[, input$moduleSubsetVar] %in% input$moduleSubsetValue), ]
			timeLim <- range(
				currentData[, c(input$moduleIntervalTimeStartVar, input$moduleIntervalTimeEndVar)],
				na.rm = TRUE
			)
			tagList(
				helpText("Time range is restricted to the following time limits."),
				sliderInput(
					inputId = "moduleIntervalTimeLim",
					label = "Time limits",
					min = timeLim[1], max = timeLim[2], value = timeLim
				)
			)
		},
		'subject-specific' = {
			tagList(
				helpText("In case of missing values, minimum start/maximum end time is extracted by subject based on:"),
				fluidRow(
					column(4, 
						selectInput(
							inputId = "moduleIntervalTimeLimData",
							label = "Dataset",
							choices = results$datasets(),
							selected = if(!is.null(results$currentModule()))	results$currentModule()$timeLimData	else
								if("SV" %in% results$datasets())	"SV"
						)
					),
					column(8, uiOutput("moduleIntervalTimeLimVarsPanel"))
				)
			)
		}
	)
})

results$timeLimData <- reactive({
	if(!is.null(input$moduleIntervalTimeLimData))	results$dataAll()[[input$moduleIntervalTimeLimData]]
})

output$moduleIntervalTimeLimVarsPanel <- renderUI({
	validate(need(results$timeLimData(), ""))
	timeLimVars <- getTimeVars(data = results$timeLimData(), labelVars = results$labelVars())
	fluidRow(
		column(6, 
			createWidgetVariable(
				inputId = "moduleIntervalTimeLimStartVar", 
				label = "Start time variable",
				choices = timeLimVars,
				selected = if(!is.null(results$currentModule()))	results$currentModule()$timeLimStartVar	else
					if("SVSTDY" %in% timeLimVars)	"SVSTDY"
			)
		),
		column(6,
			createWidgetVariable(
				inputId = "moduleIntervalTimeLimEndVar", 
				label = "End time variable",
				choices = timeLimVars,
				selected = if(!is.null(results$currentModule()))	results$currentModule()$timeLimEndVar	else
					if("SVENDY" %in% timeLimVars)	"SVENDY"
			)
		)
	)
})

# create modules specific of the text panel
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
					label = "Parameter value variable(s)",
					selected = if(!is.null(results$currentModule()))	
						results$currentModule()$paramValueVar,
					multiple = TRUE
				)
			),
			column(6, createWidgetVariable(inputId = "moduleTextParamNameVarPair",
				label = "Parameter name variable",
				selected = if(!is.null(results$currentModule()))	results$currentModule()$paramNameVar)
			)
		)
	)	
})
