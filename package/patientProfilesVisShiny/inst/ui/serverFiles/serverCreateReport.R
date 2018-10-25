# Shiny app: server script for report creation
# 
# Author: Laure Cougnaud
###############################################################################

output$reportCreation <- renderUI({
			
	validate(need(results$dataRes(), "Please upload some data."))
	
	tagList(
		selectInput(inputId = "reportSelectedModules",
			label = "Module(s) selected for the report", multiple = TRUE,
			choices = results$defaultModulesNames(),
			selected = results$defaultModulesNames()
		),
		strong("Sort subjects based on:"),
		fluidRow(
			column(6,
				selectInput(inputId = "reportSubjectSortData",
					label = "Dataset", multiple = FALSE,
					choices = c('<none>' = 'none', results$datasets()),
					selected = c('<none>' = 'none')
				)
			),
			column(6, uiOutput("reportSubjectSortVarPanel"))
		),
		fluidRow(
			column(6, actionButton(inputId = "createSubjectProfileReport", label = "Create report")),
			column(6, uiOutput("downloadSubjectProfileReportPanel"))
		)
	)		
	
})

# extract the possible variable to sort subject by
output$reportSubjectSortVarPanel <- renderUI({
			
	validate(need(input$reportSubjectSortData, "reportSubjectSortData"))
	
	reportSubjectSortVars  <- if(input$reportSubjectSortData != "none"){
		getVarLabelsForUI(
			data = results$dataAll()[[input$reportSubjectSortData]], 
			labelVars = results$labelVars()
		)
	}
	selectInput(
		inputId = "reportSubjectSortVar", 
		label = "Variable", multiple = FALSE,
		choices = reportSubjectSortVars, 
		selected = reportSubjectSortVars[1]
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
				# sort modules as specified in the UI
				listPlots <- listPlots[input$reportSelectedModules]
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
	downloadButton("downloadSubjectProfileReport", label = "Download report")
})
