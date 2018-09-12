library(glpgStyle)

uiFunction <- function(request){
	
	fluidPage(
			
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
		
		titlePanel(title = div(img(
			src = "logo-with-fish.png", 
			float = "top", height = "60px", hspace = "70px"),
			"Visualization of patient profile"),
			windowTitle = "patientProfiles"
		),
		
	    sidebarLayout(
	        
	        sidebarPanel(
	            
	            h3("Data upload"),
	            
				# upload data file(s)
	            fileInput(inputId = 'dataFiles', label = NULL, 
					accept = '.sas7bdat', multiple = TRUE
				),
	            
				# new module specification
				h3("Module specification"),
				uiOutput("module"),
				br(),
				
				# creation of subject profile report
				h3("Report creation"),
				uiOutput("reportCreation")
	        ), 
	        
	        mainPanel(
	            
	            h3("Preview subject profiles"),
				uiOutput("moduleResults")
	        
	        )
	    
	    )
	
	)
	
}

