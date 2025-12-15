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

	output$combat_dynamic_ui <- renderUI({
		
		# Get selected mode
		mode <- input$batch_interface_mode
		
		# Default to Full if not selected yet
		if (is.null(mode)) mode <- "Full"
		
		# Source the appropriate UI file
		ui_file <- sprintf("ShinySections/BatchCorrect_%s_ui.R", mode)
		
		cat(sprintf("═══ Loading %s Interface UI ═══\n", mode))
		
		if (file.exists(ui_file)) {
			source(ui_file, local = TRUE)$value
		} else {
			# Fallback if file doesn't exist
			fluidRow(
				column(12,
							 box(
							 	title = "Error",
							 	status = "danger",
							 	width = 12,
							 	sprintf("UI file not found: %s", ui_file)
							 )
				)
			)
		}
	})
	# MODE_SUFFIX <- 'Simple'
	# source("ShinySections/BatchCorrect_Full_server.R", local = TRUE)
	# 
	# MODE_SUFFIX <- 'Full'
	# source("ShinySections/BatchCorrect_Full_server.R", local = TRUE)
	
	observeEvent(input$batch_interface_mode, {
		
		mode <- input$batch_interface_mode
		
		MODE_SUFFIX <- tolower(mode)
		
		cat(sprintf("\n═══ Reloading with MODE_SUFFIX = '%s' ═══\n", MODE_SUFFIX))
		
		source("ShinySections/BatchCorrect_Full_server.R", local = TRUE)
		
		cat(sprintf("✓ Reloaded with MODE_SUFFIX = '%s'\n\n", MODE_SUFFIX))
		
	})
	
	# initial_loaded <- reactiveVal(FALSE)
	# observe({
	# 	
	# 	# Only run if not already loaded
	# 	if (!initial_loaded()) {
	# 		
	# 		MODE_SUFFIX <- "full"
	# 		
	# 		cat(sprintf("═══ Initial load with MODE_SUFFIX = '%s' ═══\n", MODE_SUFFIX))
	# 		
	# 		source("ShinySections/BatchCorrect_Full_server.R", local = TRUE)
	# 		
	# 		cat("✓ Loaded\n")
	# 		
	# 		# Mark as loaded
	# 		initial_loaded(TRUE)
	# 	}
	# })
	# 
	# # ═══════════════════════════════════════════════════════════════
	# # ✅ Reload SAME file with DIFFERENT MODE_SUFFIX when mode changes
	# # ═══════════════════════════════════════════════════════════════
	# 
	# observeEvent(input$batch_interface_mode, {
	# 	
	# 	mode <- input$batch_interface_mode
	# 	
	# 	MODE_SUFFIX <- tolower(mode)
	# 	
	# 	cat(sprintf("\n═══ Reloading with MODE_SUFFIX = '%s' ═══\n", MODE_SUFFIX))
	# 	
	# 	source("ShinySections/BatchCorrect_Full_server.R", local = TRUE)
	# 	
	# 	cat(sprintf("✓ Reloaded with MODE_SUFFIX = '%s'\n\n", MODE_SUFFIX))
	# 	
	# }, ignoreInit = TRUE)
	
	# observeEvent(input$batch_interface_mode,{
	# 
	# 	MODE_SUFFIX <- "Full"  # Default
	# 	if(is.null(input$batch_interface_mode)){
	# 		MODE_SUFFIX = input$batch_interface_mode
	# 	}
	# 
	# 	cat(sprintf("═══ INITIAL LOAD:  MODE_SUFFIX = '%s' ═══\n", MODE_SUFFIX))
	# 	print(MODE_SUFFIX)
	# 	#source("ShinySections/BatchCorrect_Full_server.R", local = TRUE)
	
	# mode_suffix = reactive({
	# # 	mode = input$batch_interface_mode
	# # 	if (is.null(mode)) mode <- "Full"
	# # 	mode
	# # })
	# 
	# observe({
	# 	
	# 	cat("═══ INITIAL SERVER LOAD ═══\n")
	# 	
	# 	# Default to Full mode
	# 	MODE_SUFFIX <- "full"
	# 	
	# 	server_file <- "ShinySections/BatchCorrect_Full_server.R"
	# 	
	# 	if (file.exists(server_file)) {
	# 		source(server_file, local = TRUE)
	# 		cat("✓ Initial server loaded\n")
	# 	}
	# 	
	# })  # ✅ Only runs ONCE
	# 
	# 
	# observeEvent(input$batch_interface_mode, {
	# 	
	# 	mode <- input$batch_interface_mode
	# 	req(mode)  # ✅ Don't run if NULL
	# 	
	# 	MODE_SUFFIX <- tolower(mode)
	# 	
	# 	cat(sprintf("═══ SWITCHING TO %s MODE ═══\n", mode))
	# 	
	# 	server_file <- sprintf("ShinySections/BatchCorrect_%s_server.R", mode)
	# 	
	# 	if (file.exists(server_file)) {
	# 		source(server_file, local = TRUE)
	# 		cat("✓ Server reloaded\n")
	# 	}
	# 	
	# }, ignoreInit = TRUE)  # ✅ Ignore initial load (handled by observe above)
	

	# observeEvent(input$batch_interface_mode, {
	# 	
	# 	# ✅ Inside reactive context - can access input$
	# 	mode <- input$batch_interface_mode
	# 	if (is.null(mode)) mode <- "Full"
	# 	
	# 	MODE_SUFFIX <- tolower(mode)
	# 	
	# 	# ✅ Source the server file with MODE_SUFFIX in scope
	# 	source("ShinySections/BatchCorrect_Full_server.R", local = TRUE)
	# 	
	# }, ignoreNULL = FALSE, ignoreInit = FALSE)
	
	# observe({
	# 	mode <- input$batch_interface_mode
	# 	if (is.null(mode)) mode <- "Full"
	# 	
	# 	# ✅ Set the suffix based on mode
	# 	MODE_SUFFIX <- tolower(mode)  # "full" or "simple"
	# 	
	# 	cat(sprintf("═══ Loading Server with MODE_SUFFIX: '%s' ═══\n", MODE_SUFFIX))
	# 	
	# 	# ✅ Source server file - it will use MODE_SUFFIX
	# 	server_file <- "ShinySections/BatchCorrect_Full_server.R"  # Single server file! 
	# 	
	# 	if (file.exists(server_file)) {
	# 		source(server_file, local = TRUE)
	# 	} else {
	# 		warning(sprintf("Server file not found:  %s", server_file))
	# 	}
	# })

	
	expset_data <- mod_expset_import_server("expset_import", debug = run_debug)
	
	ExpSet_list <- reactive({
		expset_data$ExpSet_list()
	})
	
 # ✅ Initialize as NULL
	
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
	
	# Manifest #####
	
	# Call the module
	manifest_data <- mod_manifest_upload_server(
		"manifest_uploader",
		debug = run_debug
	)
	
	annot_dist <- mod_annotation_distribution_server(
		"annot_dist",
		manifest_data = manifest_data$data,
		batch_columns = manifest_data$batch_columns,
		annotation_columns = manifest_data$annotation_columns,
		debug = run_debug
	)
	

	
	
	

	# Initial Data Selection #####
	
# 	data_module <- mod_eset_selector_standalone_server(
# 		"initial_select",
# 		ExpSet_list = ExpSet_list,
# 		default_selection = "clinical_loess_normalised_PN",
# 		#source = expset_data$source,
# 		enable_subset = TRUE,
# 		enable_transform = TRUE,
# 		debug = run_debug
# 	)
# 	
# 	combat_data <- mod_eset_selector_standalone_server(
# 		"combat_data",
# 		ExpSet_list = ExpSet_list,
# 		default_selection = "sample_loess_normalised",
# 		#source = expset_data$source,
# 		enable_subset = TRUE,
# 		enable_transform = TRUE,
# 		debug = run_debug
# 	)
#  # Batch Testing ####
# 	# Update annotation module to use transformed data:
# 	annotation_module <- mod_annotation_analysis_server(
# 		"annotation_analysis",
# 		eset = data_module$eset,  # <-- Changed from data_module$eset
# 		debug = run_debug
# 	)
# 	
# 	
# 	# Extract filtered columns with better error handling
# 	filtered_columns <- reactive({
# 		# Wait for annotation analysis to complete
# 		req(annotation_module$analysis_results())
# 		
# 		results <- annotation_module$analysis_results()
# 		
# 		# Debug output
# 		message("\n═══ Filtered Columns Debug ═══")
# 		message("Analysis results available: ", ! is.null(results))
# 		
# 		if (!is.null(results$df_filter)) {
# 			message("df_filter rows: ", nrow(results$df_filter))
# 			message("df_filter columns: ", paste(colnames(results$df_filter), collapse = ", "))
# 			
# 			if ("column_name" %in% colnames(results$df_filter)) {
# 				cols <- results$df_filter$column_name
# 				message("Filtered columns: ", paste(cols, collapse = ", "))
# 				return(cols)
# 			} else {
# 				message("ERROR: 'column_name' not found in df_filter")
# 				return(character(0))
# 			}
# 		} else {
# 			message("ERROR: df_filter is NULL")
# 			return(character(0))
# 		}
# 	})
# 	
# 	
# 	sample_group_module <- mod_column_selector_server(
# 		"sample_group",
# 		eset = data_module$eset,
# 		default_columns = "Labels",
# 		multiple = FALSE,
# 		label = "Sample Grouping Column:",
# 		help_text = "Primary biological grouping variable",
# 		debug = TRUE
# 	)
# 	
# 	# Batch column selector module
# 	batch_column_module <- mod_column_selector_server(
# 		"batch_columns",
# 		eset = data_module$eset,
# 		available_columns = filtered_columns,  # From annotation analysis
# 		default_columns = reactive(c("Labels",'Assay','Batch_ID','Assay.Date',"Assay_Date.(YYYY/MM/DD)")),
# 		multiple = TRUE,
# 		label = "Batch Testing Columns:",
# 		debug = FALSE
# 	)
# 	
# 	### Batch testing module ####
# 	batch_testing <- mod_batch_testing_server(
# 		"batch_testing",
# 		eset = data_module$eset,
# 		selected_columns = batch_column_module$selected_columns,
# 		debug = run_debug
# 	)
# 	
# 	### Distribution testing module ####
# 	distribution_test <- mod_batch_distribution_test_server(
# 		"distribution_test",
# 		eset = data_module$eset,
# 		sample_group_column = sample_group_module$selected_column,
# 		batch_columns = batch_column_module$selected_columns,
# 		debug = run_debug
# 	)
# 	
# 	## Combined batch analysis module ####
# 	batch_combined <- mod_batch_combined_analysis_server(
# 		"batch_combined",
# 		eset = data_module$eset,
# 		#eset = combat_module$preview_eset,
# 		sample_group_column = sample_group_module$selected_column,
# 		batch_columns = batch_column_module$selected_columns,
# 		debug = run_debug
# 	)
# 
# 	
# 	# Initialize it when ExpSet_list is available
# 	observe({
# 		req(ExpSet_list())
# 		if (is.null(ExpSet_list_val())) {
# 			ExpSet_list_val(ExpSet_list())
# 			message("✅ Initialized ExpSet_list_val")
# 		}
# 	})
# 	
# 	# ✅ Use a reactive that switches to ExpSet_list_val when it has data
# 	ExpSet_list_for_export <- reactive({
# 		# If ExpSet_list_val has been updated, use it
# 		if (! is.null(ExpSet_list_val())) {
# 			message("Using ExpSet_list_val for export")
# 			return(ExpSet_list_val())
# 		}
# 		# Otherwise fall back to original
# 		message("Using original ExpSet_list for export")
# 		ExpSet_list()
# 	})
# 	
# 	combat_selector <- mod_combat_correction_selector_server(
# 		"combat_selector",
# 		eset = data_module$eset,  # or whatever your data source is
# 		combined_results = batch_combined$results,  # or your batch analysis results
# 		debug = run_debug
# 	)
# 	
# 	# ✅ Call single correction module
# 	single_combat <- mod_combat_single_server(
# 		"combat_single",  # ✅ Must match the ID in ui.R
# 		eset = combat_data$eset,  # ✅ Use the actual reactive from your app
# 		sample_group_column = sample_group_module$selected_column,  # ✅ Use the actual reactive
# 		combined_results = batch_combined$results,  # ✅ Use the actual reactive
# 		selector = combat_selector,  # ✅ Use the selector module you already called
# 		show_auto_run_toggle = TRUE,
# 		debug = run_debug
# 	)
# 	
# 	batch_viz <- mod_batch_visualization_server(
# 		"batch_viz",
# 		eset_original_name = reactive(combat_data$eset_name()),
# 		eset_original = combat_data$eset,
# 		eset_corrected = single_combat$corrected_eset,
# 		sample_group_column = sample_group_module$selected_column,
# 		batch_factors = reactive({
# 			# ✅ Create plot_batch_factors from combat_selector
# 			req(combat_selector$batch_factors())
# 			c(combat_selector$batch_factors(), 'ComBat')
# 		}),
# 		ExpSet_list = ExpSet_list,
# 		debug = run_debug
# 	)
# 	
# 	default_assays_to_correct <- c(
# 		'clinical_loess_normalised',
# 		'clinical_loess_normalised_PN',
# 		'sample_loess_normalised'
# 	)
# 	
# 	mod_combat_multi_assay_server(
# 		id = "combat_multi",
# 		ExpSet_list = ExpSet_list,
# 		update_ExpSet_list = update_ExpSet_list,
# 		sample_group_column = sample_group_module$selected_column,
# 		selector = combat_selector,
# 		default_target_assays = default_assays_to_correct,
# 		debug = run_debug
# 	)
# 	
# 	# Export ####
# 
# 	# Export module uses the combined reactive
# 	expset_export <- mod_expset_export_server(
# 		"expset_export",
# 		ExpSet_list = ExpSet_list_for_export  # ✅ Gets updated list
# 	)
}
