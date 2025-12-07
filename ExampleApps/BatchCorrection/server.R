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
	
	# ============================================
	# SHARED: ExpSet List Import (used by all)
	# ============================================
	expset_data <- mod_expset_import_server("expset_import", debug = run_debug)
	
	ExpSet_list <- reactive({
		expset_data$ExpSet_list()
	})
	
	# ExpressionSet Viewer Module
	expset_viewer <- mod_expset_viewer_server(
		"expset_viewer",
		ExpSet_list = ExpSet_list  # Use the same ExpSet_list from expset_import
	)
	
	# ============================================
	# TAB 1: Initial Data Selection
	# ============================================
	# data_module <- mod_eset_selector_standalone_server(
	# 	"initial_select",
	# 	ExpSet_list = ExpSet_list,
	# 	default_selection = "clinical_loess_normalised_PN",
	# 	source = expset_data$source
	# )
	
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
	
	vis_input_data <- mod_eset_selector_standalone_server(
		"vis_input",
		ExpSet_list = ExpSet_list,
		default_selection = reactive({
			name <- tryCatch({
				combat_data$eset_name()
			}, error = function(e) {
				NULL
			})
			
			# Return fallback if NULL, empty, or NA
			if (is.null(name) || length(name) == 0 || is.na(name)) {
				"sample_loess_normalised"
			} else {
				name
			}
		}),
		#default_selection = combat_data$eset_name,
		#default_selection = "sample_loess_normalised",
		#default_selection = reactive({ combat_data$eset_name() }),
		source = expset_data$source,
		enable_subset = TRUE,
		enable_transform = TRUE,
		debug = run_debug
	)
	
	# # Data selection module
	# data_module <- mod_app_data_selection_server(
	# 	"data_select",
	# 	default_selection = "clinical_loess_normalised_PN",
	# 	debug = run_debug
	# )
	
	# # Subset module
	# subset_module <- mod_eset_subset_server(
	# 	"subset",
	# 	eset = data_module$eset,
	# 	debug = run_debug
	# )
	# 
	# # Reset subset when new data is loaded
	# observeEvent(data_module$eset(), {
	# 	req(data_module$eset())
	# 	# Trigger reset by calling the reset button programmatically
	# 	subset_module$subset_eset(data_module$eset())
	# })
	
	# # Transform module (uses subset output)
	# transform_module <- mod_eset_transform_server(
	# 	"transform",
	# 	eset = subset_module$subset_eset,
	# 	debug = run_debug
	# )
	# 
	# # Reset transform when subset changes
	# observeEvent(subset_module$subset_eset(), {
	# 	req(subset_module$subset_eset())
	# 	transform_module$transformed_eset(subset_module$subset_eset())
	# })
	

	# Update annotation module to use transformed data:
	annotation_module <- mod_annotation_analysis_server(
		"annotation_analysis",
		eset = data_module$eset,  # <-- Changed from data_module$eset
		debug = run_debug
	)
	
	# # And all other modules that use eset should use transform_module$transformed_eset
	# 
	# # Annotation analysis module
	# annotation_module <- mod_annotation_analysis_server(
	# 	"annotation_analysis",
	# 	eset = data_module$eset,
	# 	debug = run_debug
	# )
	
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
	
	# Combined batch analysis module ####
	batch_combined <- mod_batch_combined_analysis_server(
		"batch_combined",
		eset = data_module$eset,
		#eset = combat_module$preview_eset,
		sample_group_column = sample_group_module$selected_column,
		batch_columns = batch_column_module$selected_columns,
		debug = run_debug
	)
	
	batch_analysis <- mod_batch_combined_analysis_server(
		"batch_analysis",
		eset = combat_module$preview_eset,  # ✅ Use preview instead of original
		sample_group_column = sample_group_module$selected_column,
		batch_columns = combat_module$selected_batch_factors,
		debug = run_debug
	)
	
	
	
	# ComBat correction module ####
	# combat_module <- mod_combat_correction_server(
	# 	"combat",
	# 	eset = combat_data$eset,
	# 	sample_group_column = sample_group_module$selected_column,
	# 	combined_results = batch_combined$results,
	# 	debug = run_debug
	# )
	
	# ComBat Correction Module
	
	# ComBat Correction Module
	# combat_module <- mod_combat_correction_server(
	# 	"combat",
	# 	eset = data_module$eset,
	# 	sample_group_column = sample_group_module$selected_column,
	# 	combined_results = combined_analysis$results_table,
	# 	ExpSet_list = ExpSet_list,
	# 	selected_expset_name = data_module$selected_name,
	# 	update_expset_list = function(new_list) {
	# 		ExpSet_list(new_list)  # Assuming ExpSet_list is a reactiveVal
	# 	},
	# 	debug = run_debug  # Pass your debug flag
	# )
	ExpSet_list_val <- reactiveVal(ExpSet_list)
	
	combat_module <- mod_combat_correction_server(
		"combat",
		eset = combat_data$eset,
		sample_group_column = sample_group_module$selected_column,
		ExpSet_list = ExpSet_list,           # ✅ Pass ExpSet_list
		update_ExpSet_list = ExpSet_list_val,   # ✅ Pass update function (reactiveVal can be called to update)
		combined_results = batch_combined$results,
		debug = run_debug
	)
	
	# combat_module <- mod_combat_correction_server(
	# 		"combat",
	# 		eset = combat_data$eset,
	# 		sample_group_column = sample_group_module$selected_column,
	# 		combined_results = batch_combined$results,
	# 	debug = run_debug
	# )
	# combat_module <- mod_combat_correction_server(
	# 	"combat",
	# 	eset = data_module$eset,
	# 	sample_group_column = sample_group_module$selected_column,
	# 	combined_results = combined_analysis$results_table,
	# 	ExpSet_list = ExpSet_list,
	# 	selected_expset_name = data_module$selected_name,
	# 	update_expset_list = function(new_list) {
	# 		# Update the main ExpSet_list reactiveVal
	# 		# This depends on how you've structured your ExpSet_list
	# 		# If it's a reactiveVal, you'd do:
	# 		ExpSet_list_val(new_list)
	# 	}
	# )
	
	# Batch visualization
	# batch_viz <- mod_batch_visualization_server(
	# 	"batch_viz",
	# 	eset_original = combat_data$eset,
	# 	eset_corrected = combat_module$corrected_eset,
	# 	debug = run_debug
	# )
	
	# Batch visualization ####
	batch_viz <- mod_batch_visualization_server(
		"batch_viz",
		eset_original_name = reactive(vis_input_data$eset_name()),
		eset_original = vis_input_data$eset,
		eset_corrected = combat_module$corrected_eset,
		sample_group_column = sample_group_module$selected_column,      # ✅ From Batch Analysis tab
		batch_factors = combat_module$plot_batch_factors,            # ✅ From ComBat Correction tab
		ExpSet_list = ExpSet_list,
		debug = run_debug
	)
	
	# Export ####
	
	# ExpressionSet Manager Module
	expset_manager <- mod_expset_manager_server(
		"expset_manager",
		ExpSet_list = ExpSet_list,
		selected_batch_factors = combat_module$selected_batch_factors,
		all_columns = combat_module$all_columns,
		#corrected_eset = combat_module$corrected_eset,
		debug = run_debug
	)
	# ExpressionSet Manager Module
	# expset_manager_2 <- mod_expset_manager_server_2(
	# 	"expset_manager",
	# 	ExpSet_list = ExpSet_list,
	# 	
	# 	assay_choices = data_module$assay_choices,  # Use the existing assay choices
	# 	selected_batch_factors = combat_module$selected_batch_factors,
	# 	corrected_eset = combat_module$corrected_eset,
	# 	sample_group_column = sample_group_module$selected_column,
	# 	debug = run_debug
	# )
	
	# Export module
	expset_export <- mod_expset_export_server(
		"expset_export",
		ExpSet_list = ExpSet_list
	)
}
