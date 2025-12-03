# SERVER :  BatchCorrection App ####
	
	server <- function(input, output, session) {
		
		options(shiny.maxRequestSize = 100*1024^2)
		
		# Debug button UI
		output$debug_ui <- renderUI({
			if(run_debug == TRUE){
				actionButton('debug', 'Debug', class = "btn-warning btn-sm")
			}
		})
		
		observeEvent(input$debug, {
			browser()
		})
		
		# Data selection module
		data_module <- mod_app_data_selection_server(
			"data_select",
			default_selection = "clinical_median_normalised_PN",
			debug = run_debug
		)
		
		# Annotation analysis module
		annotation_module <- mod_annotation_analysis_server(
			"annotation_analysis",
			eset = data_module$eset,
			debug = run_debug
		)
		
		# Batch column selector module
		column_selector <- mod_batch_column_selector_server(
			"column_selector",
			eset = data_module$eset,
			useful_columns = annotation_module$useful_columns,
			default_columns = c('Labels','Batch_ID','Assay'),  # Or specify defaults like c("Sample_Group", "Batch")
			debug = run_debug
		)
		
		# Batch testing module
		batch_testing <- mod_batch_testing_server(
			"batch_testing",
			eset = data_module$eset,
			selected_columns = column_selector$selected_columns,
			debug = run_debug
		)
}
