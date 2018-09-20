# Shiny app: server script to preview/save a module
# 
# Author: Laure Cougnaud
###############################################################################

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
#	}else 
#	if(inputModule != "<none>" & !input$moduleTitle %in% names(results$listPlots)){
#		output$moduleSaveMessage <- renderUI(
#			div(strong("Title already used, please specify a different title."), 
#				style = "color:red")
#		)
#		NULL
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
					
#	h1("Module preview"),
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
						style = "color:green"
					)
				)
				output$previewPagePanel <- renderUI(
					selectInput(
						inputId = "previewPage", label = "Page",
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



# save current module if requested
observeEvent(input$saveModule, {
			
	if(!isTruthy(results$plotsCurrent)){
		output$moduleSaveMessage <- renderUI(
			div(strong("Please preview first your specified module."), 
				style = "color:red"))
	}else if(!is.null(input$moduleTitle) && 
		length(results$defaultModulesNames()) > 0 && 
		# by default the label is set to the title
		input$moduleTitle %in% sub("(.+) \\(.+\\)", "\\1", results$defaultModulesNames())){
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
			div("Module has been saved, you can preview it by selecting:", br(), 
				paste0("'", names(newModule), "' in the selection box below."), 
				style = "color:green"
			)
		)
		output$moduleSaveMessage <- previewMessage <- renderUI("")
		
		# delete current plot
		results$plotsCurrent <- NULL
		
	}
	
})
