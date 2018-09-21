library(glpgStyle)

uiFunction <- function(request){
	
	fluidPage(
			
		theme = "custom.css",
			
		# For debugging
		h4("Debugging & testing options"), 
		fluidRow(
			column(4, 
				tags$b("Print objects"),
				verbatimTextOutput("debug_text")
			),
			column(4, 
				tags$b("Connect with console"),
				helpText(HTML("The console will display a Browse prompt: <code>Browse[1]></code>")),
				helpText(HTML("Enter <code>c</code> at the prompt to stop communication with the console and resume with the shiny app")),
				actionButton(inputId = "debug_console", label = "Connect with console")
			)
		),
		
		titlePanel(
			title = h1(
				img(
					src = "logo-with-fish.png", 
					float = "top", height = "100px", hspace = "70px"
				),
				"Visualization of patient profile",
				style = "font-size: 3rem;"
			),
			windowTitle = "patientProfiles"
		),
		
	    sidebarLayout(
	        
	        sidebarPanel(
	            
				downloadLink(outputId = "downloadManual", label = "Documentation",
					style = "font-weight: bold; color: #00463E"),
					
	            h1("Data upload"),
	            
				# upload data file(s)
	            fileInput(inputId = 'dataFiles', label = NULL, 
					accept = '.sas7bdat', multiple = TRUE
				),
				helpText("Please load ADaM and SDTM data files ('sas7bdat' format).",
					br(), "Pre-defined settings are only available for the SDTM format."),
	            
				# new module specification
				h1("Module specification"),
				uiOutput("module"),
				
				# creation of subject profile report
				h1("Report creation"),
				uiOutput("reportCreation")
	        ), 
	        
	        mainPanel(
	            
	            h1("Preview subject profiles"),
				uiOutput("moduleResults")
	        
	        )
	    
	    ),
		
		em(hr(),
		"This Shiny App was created in collaboration with Paul Meyvisch and Bjorn Daems from the Galapagos Biometrics group.", 
		 "For specific questions/remarks or bugs report, please contact: laure.cougnaud@openanalytics.eu.")
	
	)
	
}

