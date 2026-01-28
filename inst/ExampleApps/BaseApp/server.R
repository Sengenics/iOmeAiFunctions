# SERVER : Base Appp #####

server <- function(input, output, session) {
	
	
	expset_data <- mod_expset_import_server("expset_import", debug = run_debug)
	
	ExpSet_list <- reactive({
		expset_data$ExpSet_list()
	})
	
	# Create the update function: 
	update_ExpSet_list <- function(new_list) {
		ExpSet_list_val(new_list)  # ✅ Update the reactiveVal
		message("✅ Updated ExpSet_list_val with ", length(new_list), " ExpressionSets")
	}
	
	# ExpressionSet Viewer Module
	expset_viewer <- mod_expset_viewer_server(
		"expset_viewer",
		ExpSet_list = ExpSet_list  # Use the same ExpSet_list from expset_import
	)
	# Data selection module (handles everything)
	# data_module <- mod_app_data_selection_server(
	# 	"data_select",
	# 	default_selection = "sample_ImputedlogMeanNetI",
	# 	debug = run_debug
	# )
	
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