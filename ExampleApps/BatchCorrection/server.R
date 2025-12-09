# SERVER :  BatchCorrection App ####

server <- function(input, output, session) {
	
	options(shiny.maxRequestSize = 100*1024^2)
	
	##  Debug button UI ###
	output$debug_ui <- renderUI({
		if(run_debug == TRUE){
			actionButton('debug', 'Debug : server.R', class = "btn-warning btn-sm")
		}
	})
	
	observeEvent(input$debug, {
		browser()
	})
	

	# SHARED: ExpSet List Import (used by all) ####

	

	
	expset_data <- mod_expset_import_server("expset_import", debug = run_debug)
	
	ExpSet_list <- reactive({
		expset_data$ExpSet_list()
	})
	ExpSet_list_val <- reactiveVal(ExpSet_list)
	
	# ExpressionSet Viewer Module
	expset_viewer <- mod_expset_viewer_server(
		"expset_viewer",
		ExpSet_list = ExpSet_list  # Use the same ExpSet_list from expset_import
	)
	

	# Initial Data Selection #####
	
	data_module <- mod_eset_selector_standalone_server(
		"initial_select",
		ExpSet_list = ExpSet_list,
		default_selection = "clinical_loess_normalised_PN",
		source = expset_data$source,
		enable_subset = TRUE,
		enable_transform = TRUE,
		debug = run_debug
	)
	
	combat_data <- mod_eset_selector_standalone_server(
		"combat_data",
		ExpSet_list = ExpSet_list,
		default_selection = "sample_loess_normalised",
		source = expset_data$source,
		enable_subset = TRUE,
		enable_transform = TRUE,
		debug = run_debug
	)
	
	# vis_input_data <- mod_eset_selector_standalone_server(
	# 	"vis_input",
	# 	ExpSet_list = ExpSet_list,
	# 	default_selection = reactive({
	# 		name <- tryCatch({
	# 			combat_data$eset_name()
	# 		}, error = function(e) {
	# 			NULL
	# 		})
	# 
	# 		# Return fallback if NULL, empty, or NA
	# 		if (is.null(name) || length(name) == 0 || is.na(name)) {
	# 			"sample_loess_normalised"
	# 		} else {
	# 			name
	# 		}
	# 	}),
	# 	source = expset_data$source,
	# 	enable_subset = TRUE,
	# 	enable_transform = TRUE,
	# 	debug = run_debug
	# )
	
	
 # Batch Testing ####
	# Update annotation module to use transformed data:
	annotation_module <- mod_annotation_analysis_server(
		"annotation_analysis",
		eset = data_module$eset,  # <-- Changed from data_module$eset
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
	
	## Column Selection ####
	# Sample group selector module
	# sample_group_module <- mod_sample_group_selector_server(
	# 	"sample_group",
	# 	eset = data_module$eset,
	# 	default_column = "Labels",
	# 	debug = run_debug
	# )
	
	sample_group_module <- mod_column_selector_server(
		"sample_group",
		eset = data_module$eset,
		default_columns = "Labels",
		multiple = FALSE,
		label = "Sample Grouping Column:",
		help_text = "Primary biological grouping variable",
		debug = TRUE
	)
	
	# Batch column selector module
	batch_column_module <- mod_column_selector_server(
		"batch_columns",
		eset = data_module$eset,
		available_columns = filtered_columns,  # From annotation analysis
		default_columns = reactive(c("Labels",'Assay','Batch_ID','Assay.Date',"Assay_Date.(YYYY/MM/DD)")),
		multiple = TRUE,
		label = "Batch Testing Columns:",
		debug = FALSE
	)
	
	# batch_column_module <- mod_batch_column_selector_server(
	# 	"column_selector",
	# 	eset = data_module$eset,
	# 	filtered_columns = filtered_columns,
	# 	default_columns = c("Labels",'Assay','Batch_ID','Assay.Date',"Assay_Date.(YYYY/MM/DD)"),
	# 	debug = run_debug
	# )
	
	### Batch testing module ####
	batch_testing <- mod_batch_testing_server(
		"batch_testing",
		eset = data_module$eset,
		selected_columns = batch_column_module$selected_columns,
		debug = run_debug
	)
	
	### Distribution testing module ####
	distribution_test <- mod_batch_distribution_test_server(
		"distribution_test",
		eset = data_module$eset,
		sample_group_column = sample_group_module$selected_column,
		batch_columns = batch_column_module$selected_columns,
		debug = run_debug
	)
	
	## Combined batch analysis module ####
	batch_combined <- mod_batch_combined_analysis_server(
		"batch_combined",
		eset = data_module$eset,
		#eset = combat_module$preview_eset,
		sample_group_column = sample_group_module$selected_column,
		batch_columns = batch_column_module$selected_columns,
		debug = run_debug
	)
	
	# batch_analysis <- mod_batch_combined_analysis_server(
	# 	"batch_analysis",
	# 	eset = combat_module$preview_eset,  # ✅ Use preview instead of original
	# 	sample_group_column = sample_group_module$selected_column,
	# 	batch_columns = combat_module$selected_batch_factors,
	# 	debug = run_debug
	# )
	


	
	# Initialize it when ExpSet_list is available
	observe({
		req(ExpSet_list())
		if (is.null(ExpSet_list_val())) {
			ExpSet_list_val(ExpSet_list())
			message("✅ Initialized ExpSet_list_val")
		}
	})
	
	# ✅ Use a reactive that switches to ExpSet_list_val when it has data
	ExpSet_list_for_export <- reactive({
		# If ExpSet_list_val has been updated, use it
		if (! is.null(ExpSet_list_val())) {
			message("Using ExpSet_list_val for export")
			return(ExpSet_list_val())
		}
		
		# Otherwise fall back to original
		message("Using original ExpSet_list for export")
		ExpSet_list()
	})
	
	combat_selector <- mod_combat_correction_selector_server(
		"combat_selector",
		eset = data_module$eset,  # or whatever your data source is
		combined_results = batch_combined$results,  # or your batch analysis results
		debug = run_debug
	)
	
	# RUN COMBAT ####
	combat_correction <- mod_combat_correction_server(
		"combat",
		eset = combat_data$eset,
		sample_group_column = sample_group_module$selected_column,
		ExpSet_list = ExpSet_list,           # ✅ Pass ExpSet_list
		update_ExpSet_list = ExpSet_list_val,   # ✅ Pass update function (reactiveVal can be called to update)
		combined_results = batch_combined$results,
		selector = combat_selector,
		debug = run_debug
	)
	


	
	# Batch visualization ####
	batch_viz <- mod_batch_visualization_server(
		"batch_viz",
		eset_original_name = reactive(combat_data$eset_name()),
		eset_original = combat_data$eset,
		eset_corrected = combat_correction$corrected_eset,
		sample_group_column = sample_group_module$selected_column,      # ✅ From Batch Analysis tab
		batch_factors = combat_correction$plot_batch_factors,            # ✅ From ComBat Correction tab
		ExpSet_list = ExpSet_list,
		debug = run_debug
	)
	
	# Export ####

	# Export module uses the combined reactive
	expset_export <- mod_expset_export_server(
		"expset_export",
		ExpSet_list = ExpSet_list_for_export  # ✅ Gets updated list
	)
}
