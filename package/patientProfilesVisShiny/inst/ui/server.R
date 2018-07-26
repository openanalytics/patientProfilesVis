#library(rmarkdown)
#library(tools)  
# DT, plotly
library(patientProfilesVis)
library(tools)

# to increase the limit of size of 5MB for each uploaded file
options(shiny.maxRequestSize=30*1024^2) 

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
	
	results <- reactiveValues()
  
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

	# list of data.frame
	results$dataAll <- reactive(results$dataRes())
	# named vector with variable labels
	results$labelVars <- reactive(attributes(results$dataRes())$labelVars)
	
	results$datasets <- reactive(names(results$dataAll()))
  

  	# create widgets for user specification
	output$module <- renderUI({
				
		cat("Update entire module\n")		
				
		conditionalPanel(
			condition = "input.createModule % 2 == 1",
			
			h4("Module specification"),
			
			selectInput("moduleData", label = "Dataset", 
				choices = results$datasets()
			),
			
			uiOutput("modulePanel")
		)
	})

	results$dataCurrent <- reactive({
		cat("Update current data\n")
		if(!is.null(input$moduleData))	results$dataAll()[[input$moduleData]]
	})
	results$variablesDataCurrent <- reactive(colnames(results$dataCurrent()))

	createWidgetVariable <- function(..., optional = FALSE, multiple = FALSE)
		selectInput(...,
			choices = c(
				if(optional)	c('<none>' = 'none'), 
				results$variablesDataCurrent()
			),
			multiple = multiple
		)

	output$modulePanel <- renderUI({
				
		validate(need(input$moduleData, "Please specify a dataset."))		
			
		isolate({
		
			tagList(
	
				selectInput(
					"moduleType", label = "Type", 
					choices = c("text", "event", "interval")
				),
				
				textInput("moduleTitle", label = "Title", value = ""),
				textInput("moduleLabel", label = "Label", value = input$moduleTitle),
				createWidgetVariable(
					inputId = "moduleSubjectVar",
					label = "Column with subject identifier",
					selected = ifelse("USUBJID" %in% results$variablesDataCurrent(), "USUBJID",
						results$variablesDataCurrent()[1]
					)
				),
				uiOutput("moduleSpecificType"),
				createWidgetVariable(inputId = "moduleGroupVar", label = "Column with grouping", optional = TRUE),
				actionButton(inputId = "submitModule", label = "Submit module")
			)
			
		})
		
	})

	output$moduleSpecificType <- renderUI({
		validate(need(input$moduleType, "moduleType"))
		
		conditionalPanel("input.moduleType == 'text'",		
				
			radioButtons("moduleTextVarSpecType", label = "",
				choices = list(
					"Column(s) with variable" = 1,
					"Pair of column with parameter name/value" = 2
				)
			),
			uiOutput("moduleTextVarPanel")
		)
		
	})

	output$moduleTextVarPanel <- renderUI({
				
		validate(need(input$moduleTextVarSpecType, "moduleTextVarSpecType"))	
		
		switch(input$moduleTextVarSpecType,
			'1' = createWidgetVariable(inputId = "moduleTextValueVar", label = "Column with variable", multiple = TRUE),
			'2' = list(
				createWidgetVariable(inputId = "moduleTextValueVarPair", label = "Column with variable value"),
				createWidgetVariable(inputId = "moduleTextNameVarPair", label = "Column with variable name")
			)
		)	
	})

	# create the list of plot(s) for the specified module
	results$plotCurrent <- reactive({
				
		validate(need(input$submitModule > 0, "Submit module"))
		
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
				)
			)
		)
			
		# check if all parameter(s) are specified
		reqParam <- c("data", 
			switch(input$moduleType, 'text' = c("paramValueVar"))
		)
		specParams <- sapply(listParams[reqParam], isTruthy)
		validate(need(all(specParams), "Some parameters are missing."))
		
		subjectProfileFct <- paste0("subjectProfile", simpleCap(input$moduleType), "Plot")
		do.call(subjectProfileFct, listParams)
						
	})

	# fill the plot module
	output$moduleResults <- renderUI({
		
		validate(need(results$plotCurrent(), "Plot not yet created."))
		
		tagList(	
				
			h3("Module preview"),
			selectInput(inputId = "subjectCurrent", label = "Select subject",
				choices = unique(results$dataCurrent()[, input$moduleSubjectVar])
			),
			plotOutput("modulePlot")
		
		)

	})

	output$modulePlot <- renderPlot({
		results$plotCurrent()[[input$subjectCurrent]]		
	})

#  results$analysis <- eventReactive(input$submit, {
#        
#        withProgress(message = 'Analyzing Data...\n', 
#            detail = "The analysis is running. A download button will 
#                appear when the results are processed and available. (Please do 
#                not press the 'Get Results' button multiple times)",
#            style = "notification", value = NULL, {
#              
#              
#              message(".. Analyzing Data....")
#              
#              file.copy("www/sampleReport.Rmd", ".")
#              render("sampleReport.Rmd")
#              
#            })  
#        
#        
#        return(TRUE)
#        
#      })
  
  
  
#  output$download <- downloadHandler(
#      filename = function()
#        paste0(tools::file_path_sans_ext(input$jobFile$name), '_shiny-files.zip'),
#      content = function(fname) {
#        
#        write.csv(results$data(), file = "shinyData.csv")     
#        
#        fs <- c("shinyData.csv", "sampleReport.html")
#        
#        zip(zipfile = fname, files = fs)
#      },
#      contentType = "application/zip"
#  )
#  
#  
#  output$downloadResults <- renderUI({
#        
#        validate(need(results$analysis(), "No results available"))
#        
#        downloadButton("download", label = "Download results")
#        
#      })
  
}




