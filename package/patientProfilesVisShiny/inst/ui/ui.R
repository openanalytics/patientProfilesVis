library(glpgStyle)

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
		),
		column(4,
			tags$b("Save current state"),
			verbatimTextOutput("debug_bookmark"),
			tags$br(),
			bookmarkButton()
		)
	),
		
    titlePanel(
		title = div(
			img(src = system.file("images/logo-with-fish.png", package = "glpgStyle"), 
				float = "top", height = "60px", hspace = "50px"),
         	"Visualization of patient profiles"
		), 
        windowTitle = "patientProfilesVisShiny"
	),
    
    sidebarLayout(
        
        sidebarPanel(
            
            h4("Upload data and module specification"),
            
            fileInput(inputId = 'dataFiles', label = 'Upload data', 
				accept = '.sas7bdat', multiple = TRUE
			),
            
			actionLink(inputId = "createModule", label = "Create new module"),
			uiOutput("module")
        
        ), 
        
        mainPanel(
            
            h4("Preview subject profiles"),
			
			uiOutput("moduleResults")
            
#            actionButton("submit", "Get Results")
            
#            tags$br(),
#            tags$br(),
#            
#            uiOutput("downloadResults"), 
#            uiOutput("resMessage")
        )
    
    )

)

