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
				selectInput("moduleGeneral", 
					label = "Module settings", 
					choices = c(
						"default (from uploaded data)" = "default", 
						"pre-defined (from previous export)" = "pre-defined"
					)
				)
			),
			column(6, uiOutput("moduleGeneralPreDefinedPanel"))
		),
		
#		h5("Export"),
		fluidRow(
			column(5, 
				actionButton(inputId = "moduleExportSettings", label = "Export current settings")
			),
			column(5, textInput("moduleExportSettingsID", label = NULL, placeholder = "with export ID"))
		),
		uiOutput("moduleExportSettingsMessage"),
					
		# module
		h2("Preview/create new module"),
		uiOutput("moduleGeneralPanel")
	)
})


# in case of pre-defined module, allow the user to specify the ID
observe({
	validate(need(input$moduleGeneral, ""))
	output$moduleGeneralPreDefinedPanel <- renderUI({
		if(input$moduleGeneral == "pre-defined" && isTruthy(results$modulePreDefinedID)){
			selectInput(
				inputId = "modulePreDefinedID",
				label = "Export ID",
				choices = results$modulePreDefinedID
			)
		}
	})
	if(input$moduleGeneral == "pre-defined" && !isTruthy(results$modulePreDefinedID)){
		output$moduleExportSettingsMessage <- renderUI(
			div(
				"No module settings are available, please choose the 'default' settings.", 
				style = "color:red"
			)
		)
	}		
})

# create default modules for uploaded datasets or load pre-defined module
observe({
	validate(need(input$moduleGeneral, ""))
	switch(input$moduleGeneral,
		'default' = {
			results$availableModules <- getDefaultModules(data = results$dataAll())
		},
		'pre-defined' = {
			if(isTruthy(input$modulePreDefinedID)){
				load(file.path(outputPath, paste0(input$modulePreDefinedID, ".RData")))
				results$availableModules <- moduleSettings
			}
		}
	)
})
results$defaultModulesNames <- reactive({names(results$availableModules)})

# export current module settings
observeEvent(input$moduleExportSettings, {
	if(is.null(results$availableModules)){
		output$moduleExportSettingsMessage <- renderUI(
			div(
				"No module settings available so they are not exported.", 
				style = "color:red"
			)
		)
	}else if(!isTruthy(input$moduleExportSettingsID)){
		output$moduleExportSettingsMessage <- renderUI(
			div(
				"Please specify a export ID.", 
				style = "color:red"
			)
		)
	}else if(input$moduleExportSettingsID %in% results$modulePreDefinedID){
		output$moduleExportSettingsMessage <- renderUI(
			div(
				"Module settings already exported with this ID, please select a different ID.", 
				style = "color:red"
			)
		)
	}else{
		moduleSettings <- results$availableModules
		save(moduleSettings, 
			file = file.path(outputPath, paste0(input$moduleExportSettingsID, ".RData"))
		)
		output$moduleExportSettingsMessage <- renderUI(
			div("Module settings exported.", style = "color:green")
		)
		results$modulePreDefinedID <- c(results$modulePreDefinedID, input$moduleExportSettingsID)
	}		
})
