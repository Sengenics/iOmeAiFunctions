# ═══════════════════════════════════════════════════════════════
# Batch Correction - Server Logic (Mode-Agnostic)
# Uses MODE_SUFFIX variable to construct IDs dynamically
# ═══════════════════════════════════════════════════════════════

# MODE_SUFFIX is set before this file is sourced
# It will be "full" or "simple"

#cat(sprintf("  → Server using suffix: '_%s'\n", MODE_SUFFIX))


# ═══════════════════════════════════════════════════════════════
# Module Server Calls - IDs constructed dynamically
# ═══════════════════════════════════════════════════════════════
output$debug_bc_server_ui <- renderUI({
	#if (run_debug) {
		tagList(
			actionButton(
				"debug_bc_server",
				"Debug",
				icon = icon("bug"),
				class = "btn-warning btn-sm"
			)
		)
	#}
})

observeEvent(input$debug_bc_server, {
	message("🔍 BatchCorrect_Full_server.R")
	browser()
})


ExpSet_list_val <- reactiveVal(NULL) 


data_module <- mod_eset_selector_standalone_server(
	mid("initial_select"),
	ExpSet_list = ExpSet_list,
	default_selection = "clinical_loess_normalised_PN",
	#source = expset_data$source,
	enable_subset = TRUE,
	enable_transform = TRUE,
	debug = run_debug
)

combat_data <- mod_eset_selector_standalone_server(
	mid("combat_data"),
	ExpSet_list = ExpSet_list,
	default_selection = "sample_loess_normalised",
	#source = expset_data$source,
	enable_subset = TRUE,
	enable_transform = TRUE,
	debug = run_debug
)
# Batch Testing ####
# Update annotation module to use transformed data:
annotation_module <- mod_annotation_analysis_server(
	mid("annotation_analysis"),
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


sample_group_module <- mod_column_selector_server(
	mid("sample_group"),
	eset = data_module$eset,
	default_columns = "Labels",
	multiple = FALSE,
	label = "Sample Grouping Column:",
	help_text = "Primary biological grouping variable",
	debug = TRUE
)

# Batch column selector module
batch_column_module <- mod_column_selector_server(
	mid("batch_columns"),
	eset = data_module$eset,
	available_columns = filtered_columns,  # From annotation analysis
	default_columns = reactive(c("Labels",'Assay','Batch_ID','Assay.Date',"Assay_Date.(YYYY/MM/DD)")),
	multiple = TRUE,
	label = "Batch Testing Columns:",
	debug = FALSE
)

### Batch testing module ####
batch_testing <- mod_batch_testing_server(
	mid("batch_testing"),
	eset = data_module$eset,
	selected_columns = batch_column_module$selected_columns,
	debug = run_debug
)

### Distribution testing module ####
distribution_test <- mod_batch_distribution_test_server(
	mid("distribution_test"),
	eset = data_module$eset,
	sample_group_column = sample_group_module$selected_column,
	batch_columns = batch_column_module$selected_columns,
	debug = run_debug
)

## Combined batch analysis module ####
batch_combined <- mod_batch_combined_analysis_server(
	mid("batch_combined"),
	eset = data_module$eset,
	#eset = combat_module$preview_eset,
	sample_group_column = sample_group_module$selected_column,
	batch_columns = batch_column_module$selected_columns,
	debug = run_debug
)


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
	mid("combat_selector"),
	eset = data_module$eset,  # or whatever your data source is
	combined_results = batch_combined$results,  # or your batch analysis results
	debug = run_debug
)

# ✅ Call single correction module
single_combat <- mod_combat_single_server(
	mid("combat_single"),  # ✅ Must match the ID in ui.R
	eset = combat_data$eset,  # ✅ Use the actual reactive from your app
	sample_group_column = sample_group_module$selected_column,  # ✅ Use the actual reactive
	combined_results = batch_combined$results,  # ✅ Use the actual reactive
	selector = combat_selector,  # ✅ Use the selector module you already called
	show_auto_run_toggle = TRUE,
	debug = run_debug
)
# 
# batch_viz <- mod_batch_visualization_server(
# 	mid("batch_viz"),
# 	eset_original_name = reactive(combat_data$eset_name()),
# 	eset_original = combat_data$eset,
# 	eset_corrected = single_combat$corrected_eset,
# 	sample_group_column = sample_group_module$selected_column,
# 	batch_factors = combat_selector$batch_factors,
# 	# batch_factors = reactive({
# 	# 	# ✅ Create plot_batch_factors from combat_selector
# 	# 	req(combat_selector$batch_factors())
# 	# 	c(combat_selector$batch_factors(), 'ComBat')
# 	# }),
# 	ExpSet_list = ExpSet_list,
# 	debug = run_debug
# )

batch_viz <- mod_batch_visualization_server(
	mid("batch_viz"),
	eset_original_name = reactive(combat_data$eset_name()),
	eset_original = combat_data$eset,
	eset_corrected = single_combat$corrected_eset,
	sample_group_column = sample_group_module$selected_column,
	#batch_factors = combat_selector$batch_factors,
	# batch_factors = reactive({
	# 	# ✅ Create plot_batch_factors from combat_selector
	# 	req(combat_selector$batch_factors())
	# 	c(combat_selector$batch_factors(), 'ComBat')
	# }),
	batch_factors = reactive({
		# ✅ Don't require batch factors - allow empty
		factors <- combat_selector$batch_factors() %||% character(0)
		
		if (length(factors) > 0) {
			c(factors, 'ComBat')  # Add ComBat if there are batch factors
		} else {
			character(0)  # Return empty vector if none
		}
	}),
	ExpSet_list = ExpSet_list,
	debug = run_debug
)

default_assays_to_correct <- c(
	'clinical_loess_normalised',
	'clinical_loess_normalised_PN',
	'sample_loess_normalised'
)

mod_combat_multi_assay_server(
	id = mid("combat_multi"),
	ExpSet_list = ExpSet_list,
	update_ExpSet_list = update_ExpSet_list,
	sample_group_column = sample_group_module$selected_column,
	selector = combat_selector,
	default_target_assays = default_assays_to_correct,
	debug = run_debug
)

# Export ####

# Export module uses the combined reactive
expset_export <- mod_expset_export_server(
	mid("expset_export"),
	ExpSet_list = ExpSet_list_for_export  # ✅ Gets updated list
)