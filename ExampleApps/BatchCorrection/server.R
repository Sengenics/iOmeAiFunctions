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
		default_selection = "clinical_loess_normalised_PN",
		debug = run_debug
	)
	
	# Annotation analysis module
	annotation_module <- mod_annotation_analysis_server(
		"annotation_analysis",
		eset = data_module$eset,
		debug = run_debug
	)
	
	# Extract filtered columns with better error handling
	filtered_columns <- reactive({
		# Wait for annotation analysis to complete
		req(annotation_module$analysis_results())
		
		results <- annotation_module$analysis_results()
		
		# Debug output
		message("\n═══ Filtered Columns Debug ═══")
		message("Analysis results available: ", ! is.null(results))
		
		if (!is.null(results$df_filter)) {
			message("df_filter rows: ", nrow(results$df_filter))
			message("df_filter columns: ", paste(colnames(results$df_filter), collapse = ", "))
			
			if ("column_name" %in% colnames(results$df_filter)) {
				cols <- results$df_filter$column_name
				message("Filtered columns: ", paste(cols, collapse = ", "))
				return(cols)
			} else {
				message("ERROR: 'column_name' not found in df_filter")
				return(character(0))
			}
		} else {
			message("ERROR: df_filter is NULL")
			return(character(0))
		}
	})
	
	# Sample group selector module
	sample_group_module <- mod_sample_group_selector_server(
		"sample_group",
		eset = data_module$eset,
		default_column = "Labels",
		debug = run_debug
	)
	
	# Batch column selector module
	batch_column_module <- mod_batch_column_selector_server(
		"column_selector",
		eset = data_module$eset,
		filtered_columns = filtered_columns,
		default_columns = c("Labels",'Assay','Batch_ID','Assay.Date',"Assay_Date.(YYYY/MM/DD)"),
		debug = run_debug
	)
	
	# Batch testing module
	batch_testing <- mod_batch_testing_server(
		"batch_testing",
		eset = data_module$eset,
		selected_columns = batch_column_module$selected_columns,
		debug = run_debug
	)
	
	# Distribution testing module
	distribution_test <- mod_batch_distribution_test_server(
		"distribution_test",
		eset = data_module$eset,
		sample_group_column = sample_group_module$selected_column,
		batch_columns = batch_column_module$selected_columns,
		debug = run_debug
	)
	
	# Combined batch analysis module
	batch_combined <- mod_batch_combined_analysis_server(
		"batch_combined",
		eset = data_module$eset,
		sample_group_column = sample_group_module$selected_column,
		batch_columns = batch_column_module$selected_columns,
		debug = run_debug
	)
}
