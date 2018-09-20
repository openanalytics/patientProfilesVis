# Shiny app: server script to specify module general settings
# 
# Author: Laure Cougnaud
###############################################################################

# select default or pre-defined module
output$module <- renderUI({
			
	validate(need(results$dataRes(), "Please upload some data."))
			
	# module settings
	tagList(
		h2("Module settings"),
#		h5("Import"),
		fluidRow(
			column(6, 
				selectInput("moduleSettingsGeneral", 
					label = "Module settings", 
					choices = c(
						"default (from uploaded data)" = "default", 
						"pre-defined (from previous export)" = "pre-defined"
					)
				)
			),
			column(6, uiOutput("moduleSettingsPreDefinedPanel"))
		),
		
#		h5("Export"),
		fluidRow(
			column(5, 
				actionButton(inputId = "moduleSettingsExport", label = "Export current settings")
			),
			column(5, textInput("moduleSettingsExportID", label = NULL, placeholder = "with export ID"))
		),
		uiOutput("moduleSettingsExportMessage"),
					
		# module
		h2("Preview/create new module"),
		uiOutput("moduleSettingsGeneralPanel")
	)
})


# in case of pre-defined module, allow the user to specify the ID
observe({
	validate(need(input$moduleSettingsGeneral, ""))
	output$moduleSettingsPreDefinedPanel <- renderUI({
		if(input$moduleSettingsGeneral == "pre-defined" && isTruthy(results$moduleSettingsPreDefinedID)){
			selectInput(
				inputId = "moduleSettingsPreDefinedID",
				label = "Export ID",
				choices = results$moduleSettingsPreDefinedID
			)
		}
	})
	if(input$moduleSettingsGeneral == "pre-defined" && !isTruthy(results$moduleSettingsPreDefinedID)){
		output$moduleSettingsExportMessage <- renderUI(
			div(
				"No module settings are available, please choose the 'default' settings.", 
				style = "color:red"
			)
		)
	}		
})

# create default modules for uploaded datasets or load pre-defined module
observe({
	validate(need(input$moduleSettingsGeneral, ""))
	switch(input$moduleSettingsGeneral,
		'default' = {
			results$availableModules <- getDefaultModules(data = results$dataAll())
		},
		'pre-defined' = {
			if(isTruthy(input$moduleSettingsPreDefinedID)){
				load(file.path(outputPath, paste0(input$moduleSettingsPreDefinedID, ".RData")))
				results$availableModules <- moduleSettings
			}
		}
	)
})
results$defaultModulesNames <- reactive({names(results$availableModules)})

# export current module settings
observeEvent(input$moduleSettingsExport, {
	if(is.null(results$availableModules)){
		output$moduleSettingsExportMessage <- renderUI(
			div(
				"No module settings available so they are not exported.", 
				style = "color:red"
			)
		)
	}else if(!isTruthy(input$moduleSettingsExportID)){
		output$moduleSettingsExportMessage <- renderUI(
			div(
				"Please specify a export ID.", 
				style = "color:red"
			)
		)
	}else if(input$moduleSettingsExportID %in% results$moduleSettingsPreDefinedID){
		output$moduleSettingsExportMessage <- renderUI(
			div(
				"Module settings already exported with this ID, please select a different ID.", 
				style = "color:red"
			)
		)
	}else{
		moduleSettings <- results$availableModules
		save(moduleSettings, 
			file = file.path(outputPath, paste0(input$moduleSettingsExportID, ".RData"))
		)
		output$moduleSettingsExportMessage <- renderUI(
			div("Module settings exported.", style = "color:green")
		)
		results$moduleSettingsPreDefinedID <- c(results$moduleSettingsPreDefinedID, input$moduleSettingsExportID)
	}		
})
