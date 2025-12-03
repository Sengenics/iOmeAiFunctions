# SERVER : Base Appp ####

server <- function(input, output, session) {
	
	# Data selection module (handles everything)
	data_module <- mod_app_data_selection_server(
		"data_select",
		default_selection = "sample_ImputedlogMeanNetI",
		debug = run_debug
	)
	
	output$debug_ui <- renderUI({
		if(run_debug == TRUE){
			actionButton('debug', 'Debug', class = "btn-warning btn-sm")
		}
	})
	
	observeEvent(input$debug, {
		browser()
	})
	
	# Now you have clean access to:
	# data_module$eset() - Selected ExpressionSet
	# data_module$eset_name() - Name of selected assay
	# data_module$ExpSet_list() - Full list
	
	# Add your batch testing logic here... 
}