# Server : Denoiser ####

server <- function(input, output, session) {
	
	## App Configuration ####
	options(shiny.maxRequestSize = 100*1024^2)
	
	## Debug Handler ####
	observeEvent(input$debug, {
		browser()
	})
	
	# Data Management ####
	
	## ExpSet List Storage ####
	ExpSet_list_val <- reactiveVal(NULL)
	ExpSet_list_version <- reactiveVal(0)
	
	## Load Default Data ####
	observe({
		# Only load default if nothing loaded yet
		if (is.null(ExpSet_list_val())) {
			#default_file <- "data/ExpSet_list.rds"
			default_file = '../../../data/ExpSet.rda'
			default_file = '../../../data/ExpSet_list.rds'
			
			
			if (file.exists(default_file)) {
				tryCatch({
					data <- readRDS(default_file)
					ExpSet_list_val(data)
					ExpSet_list_version(1)
					message("✅ Loaded default ExpSet_list from ", default_file)
				}, error = function(e) {
					message("⚠️ Failed to load default data: ", e$message)
				})
			} else {
				message("ℹ️ No default data file found at ", default_file)
			}
			print('test_stop')
			#browser()
		}
	})
	
	## Import Module ####
	expset_data <- mod_expset_import_server("expset_import", debug = run_debug)
	
	### Handle Uploaded Data ####
	observe({
		req(expset_data$ExpSet_list())
		
		# ✅ Use isolate() to prevent circular dependency
		isolate({
			ExpSet_list_val(expset_data$ExpSet_list())
			ExpSet_list_version(ExpSet_list_version() + 1)
			message("✅ Loaded uploaded ExpSet_list with ", length(expset_data$ExpSet_list()), " ExpressionSets")
		})
		# ExpSet_list_val(expset_data$ExpSet_list())
		# ExpSet_list_version(ExpSet_list_version() + 1)
		# message("✅ Loaded uploaded ExpSet_list with ", length(expset_data$ExpSet_list()), " ExpressionSets")
	})
	
	## Create Reactive Wrapper ####
	# ExpSet_list <- reactive({
	# 	ExpSet_list_val()
	# })
	
	ExpSet_list <- reactive({
		req(ExpSet_list_val())
		list_copy <- ExpSet_list_val()  # Force a new evaluation
		list_copy
	})
	
	## Update Function ####
	update_ExpSet_list <- function(new_list) {
		ExpSet_list_val(new_list)
		ExpSet_list_version(ExpSet_list_version() + 1)
		message("✅ ExpSet_list updated with ", length(new_list), " ExpressionSets")
	}
	
	# Metadata Management ####
	
	## Metadata Manager Module ####
	metadata_manager <- mod_expset_metadata_manager_server(
		"metadata_mgr",
		ExpSet_list = ExpSet_list,
		update_ExpSet_list = update_ExpSet_list,
		master_eset_name = "RawData_ExpSet",
		debug = run_debug
	)
	
	# Data Viewing ####
	
	## ExpressionSet Viewer Module ####
	expset_viewer <- mod_expset_viewer_server(
		"expset_viewer",
		ExpSet_list = ExpSet_list,
		ExpSet_list_version = ExpSet_list_version,
		default_selection = "sample_loess_normalised",
		debug = run_debug
	)
	
	# Data Selection ####
	
	## Raw/NetI Data Selector ####
	eset_raw_selected <- mod_eset_selector_standalone_server(
		"eset_raw",
		ExpSet_list = ExpSet_list,
		ExpSet_list_version = ExpSet_list_version,
		default_selection = "sample_ImputedlogMeanNetI",
		enable_subset = TRUE,
		enable_transform = FALSE,
		debug = run_debug
	)
	
	## Normalized Data Selector ####
	eset_norm_selected <- mod_eset_selector_standalone_server(
		"eset_norm",
		ExpSet_list = ExpSet_list,
		ExpSet_list_version = ExpSet_list_version,
		default_selection = "sample_loess_normalised",
		enable_subset = TRUE,
		enable_transform = FALSE,
		debug = run_debug
	)
	
	eset_pFC_selected <- mod_eset_selector_standalone_server(
		"eset_pFC",
		ExpSet_list = ExpSet_list,
		ExpSet_list_version = ExpSet_list_version,
		default_selection = "clinical_loess_normalised",
		enable_subset = TRUE,
		enable_transform = FALSE,
		debug = run_debug
	)
	
	
	
	## Selected Data Reactives ####
	
	expset_list_version <- reactive({
		req(ExpSet_list())
		list(
			names = names(ExpSet_list()),
			timestamp = Sys.time()
		)
	})
	
	### Raw Data ####
	# eset_raw <- reactive({
	# 	print(expset_list_version())
	# 	req(ExpSet_list())
	# 	req(eset_raw_selected)
	# 	eset_raw_selected$eset_subset()
	# 	# expset_list <- ExpSet_list()
	# 	# 
	# 	# # Get selected name
	# 	# selected_name <- eset_raw_selected$eset_name()
	# 	# 
	# 	# # Handle NULL/empty selection
	# 	# if (is.null(selected_name) || selected_name == "") {
	# 	# 	if (length(expset_list) > 0) {
	# 	# 		return(expset_list[[1]])
	# 	# 	} else {
	# 	# 		return(NULL)
	# 	# 	}
	# 	# }
	# 	# 
	# 	# # Get ExpSet name (handles nested assayData)
	# 	# ExpSet_name <- get_ExpSet_name(selected_name, expset_list)
	# 	# 
	# 	# # Fetch from current list
	# 	# if (ExpSet_name %in% names(expset_list)) {
	# 	# 	expset_list[[ExpSet_name]]
	# 	# } else {
	# 	# 	if (length(expset_list) > 0) {
	# 	# 		expset_list[[1]]
	# 	# 	} else {
	# 	# 		NULL
	# 	# 	}
	# 	# }
	# })
	
	eset_raw <- reactive({
		# ✅ Take explicit dependency on ExpSet_list to force invalidation
		req(ExpSet_list())
		expsets <- ExpSet_list()  # This forces re-evaluation when list changes
		
		req(eset_raw_selected$eset_name())
		selected_name <- eset_raw_selected$eset_name()
		
		# Get the ExpSet directly from the current list
		ExpSet_name <- get_ExpSet_name(selected_name, expsets)
		
		if (ExpSet_name %in% names(expsets)) {
			expsets[[ExpSet_name]]
		} else {
			NULL
		}
		eset_raw_selected$eset_subset()
	})
	
	### Normalized Data ####
	eset_norm <- reactive({
		req(ExpSet_list())
		req(eset_norm_selected$eset_subset())
		eset_norm_selected$eset_subset()
	})
	
	eset_pFC <- reactive({
		req(ExpSet_list())
		req(eset_pFC_selected$eset_subset())
		eset_pFC_selected$eset_subset()
	})
	
	# Status Dashboard ####
	
	## ExpSet List Status ####
	output$status_eset_list <- renderValueBox({
		if (!is.null(ExpSet_list())) {
			source_label <- if (!is.null(expset_data$ExpSet_list())) {
				"Uploaded ExpSets"
			} else {
				"ExpressionSets Loaded"
			}
			
			valueBox(
				value = length(ExpSet_list()),
				subtitle = source_label,
				icon = icon("database"),
				color = "green"
			)
		} else {
			valueBox(
				value = "NONE",
				subtitle = "Upload ExpSet file",
				icon = icon("upload"),
				color = "red"
			)
		}
	})
	
	## Raw Data Status ####
	output$status_raw <- renderValueBox({
		if (!is.null(eset_raw())) {
			valueBox(
				value = ncol(eset_raw()),
				subtitle = "Raw Data Samples",
				icon = icon("vial"),
				color = "blue"
			)
		} else {
			valueBox(
				value = "—",
				subtitle = "No Raw Data",
				icon = icon("vial"),
				color = "yellow"
			)
		}
	})
	
	## Normalized Data Status ####
	output$status_norm <- renderValueBox({
		if (!is.null(eset_norm())) {
			valueBox(
				value = ncol(eset_norm()),
				subtitle = "Normalized Data Samples",
				icon = icon("chart-line"),
				color = "purple"
			)
		} else {
			valueBox(
				value = "Optional",
				subtitle = "No Normalized Data",
				icon = icon("chart-line"),
				color = "light-blue"
			)
		}
	})
	
	## Ready Status ####
	output$status_ready <- renderValueBox({
		if (!is.null(eset_raw())) {
			valueBox(
				value = "READY",
				subtitle = "Ready to Denoise",
				icon = icon("check-circle"),
				color = "green"
			)
		} else {
			valueBox(
				value = "NOT READY",
				subtitle = "Select Data First",
				icon = icon("hourglass-half"),
				color = "orange"
			)
		}
	})
	
	# Data Info Outputs ####
	
	## Raw Data Info ####
	output$raw_info <- renderPrint({
		req(eset_raw())
		
		cat("Assay: ", eset_raw_selected$eset_name(), "\n")
		cat("Samples: ", ncol(eset_raw()), "\n")
		cat("Features: ", nrow(eset_raw()), "\n")
	})
	
	## Normalized Data Info ####
	output$norm_info <- renderPrint({
		if (!is.null(eset_norm())) {
			cat("Assay: ", eset_norm_selected$eset_name(), "\n")
			cat("Samples: ", ncol(eset_norm()), "\n")
			cat("Features: ", nrow(eset_norm()), "\n")
		} else {
			cat("Not selected (optional)")
		}
	})
	
	## Complete Summary ####
	output$eset_summary <- renderPrint({
		req(eset_raw())
		
		cat("═══════════════════════════════════════════════════════════\n")
		
		# Show data source
		if (!is.null(expset_data$ExpSet_list())) {
			cat("DATA SOURCE: Uploaded File\n")
		} else {
			cat("DATA SOURCE: Default ExpSet_list.rds\n")
		}
		cat("═══════════════════════════════════════════════════════════\n")
		
		cat("\nRAW/NETI DATA\n")
		cat("═══════════════════════════════════════════════════════════\n")
		cat("Selected Assay: ", eset_raw_selected$eset_name(), "\n")
		cat("Dimensions: ", nrow(eset_raw()), " features × ", ncol(eset_raw()), " samples\n")
		cat("Available Assays: ", paste(Biobase::assayDataElementNames(eset_raw()), collapse = ", "), "\n")
		cat("Metadata Columns: ", paste(colnames(Biobase::pData(eset_raw())), collapse = ", "), "\n")
		
		# Check for required controls
		controls <- c("ZZ_con1", "ZZ_con2", "ZZ_con3", "ZZ_con4")
		present_controls <- controls[controls %in% rownames(eset_raw())]
		cat("Control Antigens: ", paste(present_controls, collapse = ", "), "\n")
		
		# Check for PN samples
		metadata <- Biobase::pData(eset_raw())
		if ("Sample_Group" %in% colnames(metadata)) {
			sample_groups <- table(metadata$Sample_Group)
			cat("\nSample Groups:\n")
			print(sample_groups)
		}
		
		if (!is.null(eset_norm())) {
			cat("\n═══════════════════════════════════════════════════════════\n")
			cat("NORMALIZED DATA\n")
			cat("═══════════════════════════════════════════════════════════\n")
			cat("Selected Assay: ", eset_norm_selected$eset_name(), "\n")
			cat("Dimensions: ", nrow(eset_norm()), " features × ", ncol(eset_norm()), " samples\n")
			cat("Available Assays: ", paste(Biobase::assayDataElementNames(eset_norm()), collapse = ", "), "\n")
		}
	})
	
	# Debug Functions ####
	
	## Debug Data Selection ####
	observeEvent(input$debug_data, {
		if (!interactive()) {
			showNotification(
				"Debug mode only works in interactive R sessions",
				type = "warning",
				duration = 5
			)
			return(NULL)
		}
		
		message("\n╔═══════════════════════════════════════════════════════════╗")
		message("║          🔍 DEBUG MODE - Data Selection                  ║")
		message("╚═══════════════════════════════════════════════════════════╝")
		message("\n📊 Session Info:")
		message("   User: DrGarnett")
		message("   Date: ", Sys.time())
		message("\n📍 Available objects:")
		message("   ✓ ExpSet_list()         : Full list of ExpressionSets")
		message("   ✓ expset_data           : Import module reactive")
		message("   ✓ eset_raw()            : Selected raw/NetI ExpressionSet")
		message("   ✓ eset_norm()           : Selected normalized ExpressionSet")
		message("   ✓ eset_raw_selected     : Selection module reactive")
		message("   ✓ eset_norm_selected    : Selection module reactive")
		message("\n💡 Useful commands:")
		message("")
		message("   # List all ExpressionSets")
		message("   names(ExpSet_list())")
		message("")
		message("   # Check data source")
		message("   is.null(expset_data$ExpSet_list())")
		message("")
		message("   # Full diagnostics")
		message("   diagnose_ExpSet_list(ExpSet_list())")
		message("")
		message("   # Quick inspect selected data")
		message("   quick_inspect_eset(eset_raw())")
		message("   quick_inspect_eset(eset_norm())")
		message("")
		message("   # Check what's selected")
		message("   eset_raw_selected$eset_name()")
		message("   eset_norm_selected$eset_name()")
		message("")
		message("   # Manual inspection")
		message("   str(eset_raw())")
		message("   dim(Biobase::exprs(eset_raw()))")
		message("   head(Biobase::pData(eset_raw()))")
		message("")
		message("   # Validate")
		message("   validate_denoise_inputs(eset_raw())")
		message("\n⌨️  Commands:")
		message("   c    Continue")
		message("   Q    Quit browser")
		message("═══════════════════════════════════════════════════════════\n")
		
		browser()
	})
	
	## Run Diagnostics ####
	observeEvent(input$run_diagnostics, {
		
		output$eset_summary <- renderPrint({
			cat("═══════════════════════════════════════════════════════════\n")
			cat("RUNNING FULL DIAGNOSTICS...\n")
			cat("═══════════════════════════════════════════════════════════\n")
			cat("User: DrGarnett\n")
			cat("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"), "\n")
			
			if (!is.null(expset_data$ExpSet_list())) {
				cat("Data Source: UPLOADED FILE\n")
			} else {
				cat("Data Source: DEFAULT ExpSet_list.rds\n")
			}
			cat("═══════════════════════════════════════════════════════════\n\n")
			
			if (!is.null(ExpSet_list())) {
				cat("EXPRESSIONSET LIST STRUCTURE:\n")
				cat("───────────────────────────────────────────────────────────\n")
				diagnose_ExpSet_list(ExpSet_list())
				cat("\n")
			} else {
				cat("❌ ExpSet_list is NULL\n\n")
			}
			
			if (!is.null(eset_raw())) {
				cat("SELECTED RAW DATA INSPECTION:\n")
				cat("───────────────────────────────────────────────────────────\n")
				quick_inspect_eset(eset_raw())
				cat("\n")
			} else {
				cat("❌ eset_raw is NULL\n\n")
			}
			
			if (!is.null(eset_norm())) {
				cat("SELECTED NORMALIZED DATA INSPECTION:\n")
				cat("───────────────────────────────────────────────────────────\n")
				quick_inspect_eset(eset_norm())
			} else {
				cat("ℹ️  Normalized data not selected (optional)\n\n")
			}
			
			cat("\n═══════════════════════════════════════════════════════════\n")
			cat("DIAGNOSTICS COMPLETE\n")
			cat("═══════════════════════════════════════════════════════════\n")
		})
		
		showNotification(
			"✅ Diagnostics complete! Check the Complete Data Summary box above.",
			type = "message",
			duration = 5
		)
	})
	
	## Quick Check ####
	observeEvent(input$quick_check, {
		
		result <- tryCatch({
			req(eset_raw())
			validate_denoise_inputs(eset_raw())
			TRUE
		}, error = function(e) {
			showNotification(
				paste("❌ Validation Error:", e$message),
				type = "error",
				duration = 10
			)
			FALSE
		})
		
		if (result) {
			showNotification(
				"✅ Quick check passed! Data structure looks good.",
				type = "message",
				duration = 5
			)
		}
	})
	
	## Data Seletct ######
	
	### Primary Variable Select ######
	
	observe({
		req(eset_norm())  # Use normalized data as source for metadata
		
		eset <- eset_norm()
		metadata <- Biobase::pData(eset)
		col_names <- colnames(metadata)
		
		# Exclude PN samples from variable if present
		col_names <- col_names[!grepl("^PN", col_names, ignore.case = TRUE)]
		
		# Try to auto-select common variable names
		default_var <- NULL
		possible_defaults <- c("Sample_Group", "Group", "sample_status", 
													 "Sample_Status", "Status", "Labels", "Label")
		
		for (var in possible_defaults) {
			if (var %in% col_names) {
				default_var <- var
				break
			}
		}
		
		# If no default found, use first column
		if (is.null(default_var) && length(col_names) > 0) {
			default_var <- col_names[1]
		}
		
		updateSelectInput(
			session, 
			"primary_variable", 
			choices = col_names,
			selected = default_var
		)
	})
	
	## Create reactive for selected variable ####
	primary_variable <- reactive({
		req(input$primary_variable)
		input$primary_variable
	})
	
	## Variable summary table ####
	output$variable_summary <- renderTable({
		req(eset_norm(), primary_variable())
		
		metadata <- Biobase::pData(eset_norm())
		var_name <- primary_variable()
		
		if (!var_name %in% colnames(metadata)) {
			return(data.frame(Note = "Variable not found in metadata"))
		}
		
		var_data <- metadata[[var_name]]
		
		if (is.numeric(var_data)) {
			# Numeric variable summary
			data.frame(
				Statistic = c("Min", "Max", "Mean", "Median", "SD", "NAs"),
				Value = c(
					round(min(var_data, na.rm = TRUE), 2),
					round(max(var_data, na.rm = TRUE), 2),
					round(mean(var_data, na.rm = TRUE), 2),
					round(median(var_data, na.rm = TRUE), 2),
					round(sd(var_data, na.rm = TRUE), 2),
					sum(is.na(var_data))
				)
			)
		} else {
			# Categorical variable summary
			tbl <- table(var_data, useNA = "ifany")
			tbl_sorted <- sort(tbl, decreasing = TRUE)
			
			data.frame(
				Group = names(tbl_sorted),
				Count = as.numeric(tbl_sorted),
				Percentage = paste0(round(as.numeric(tbl_sorted) / sum(tbl_sorted) * 100, 1), "%")
			)
		}
	}, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs", width = "100%", align = "lrr")
	
	

	
	# Analysis Modules ####
	

	
	### pFC Analysis Module #####
	pfc_results <- pFC_Server(
		"pfc_module",
		eset_reactive = eset_pFC,  # Use normalized data
		assay_name = eset_pFC_selected$eset_name(),
		default_var_reactive = reactive({
			# Try to auto-detect the sample group column
			req(eset_pFC())
			eset <- eset_pFC()
			pdata <- Biobase::pData(eset)
			
			# Look for common column names
			possible_vars <- c("Sample_Group", "Group", "sample_status", "Sample_Status", "Status")
			matching_vars <- possible_vars[possible_vars %in% colnames(pdata)]
			
			if (length(matching_vars) > 0) {
				return(matching_vars[1])
			} else {
				return(NULL)
			}
		}),
		debug = run_debug
	)
	
	#### Log pFC Results (Optional) ####
	observe({
		req(pfc_results())
		
		results <- pfc_results()
		
		if (!is.null(results$results)) {
			showNotification(
				HTML(paste0(
					"<strong>✅ pFC Analysis Complete!</strong><br>",
					"Significant hits: ", nrow(results$results$pfc_significant), "<br>",
					"Total features tested: ", nrow(results$results$pfc_stats)
				)),
				type = "message",
				duration = 5
			)
		}
	})
	
	
	### PN Limma Analysis ####
	pn_limma_results <- mod_pn_limma_server(
		"pn_limma",
		eset = eset_norm,
		default_assay = reactive({
			req(eset_norm_selected$eset_name())
			eset_norm_selected$eset_name()
		})
	)
	
	### Denoiser Module ####
	
	
	denoiser_results <- mod_denoiser_server( 
		"denoiser",
		ExpSet_list = ExpSet_list,
		eset_raw = eset_raw,
		eset_norm = eset_norm,
		pn_limma_results = pn_limma_results
	)
	
	#### Navigate to Denoiser ####
	observeEvent(input$goto_denoiser, {
		updateTabItems(session, "sidebar", "denoise")
		
		showNotification(
			"📊 Ready to run denoiser analysis!",
			type = "message",
			duration = 3
		)
	})
	
	#### PC Visualizer Module ####
	pc_viz_results <- mod_pc_visualizer_server(
		"pc_viz",
		eset_raw = eset_raw,
		denoiser_results = denoiser_results
	)
	
	#### Log Denoising Results ####
	observe({
		req(denoiser_results())
		
		results <- denoiser_results()
		
		if (!is.null(results$optimal_cutpoint)) {
			showNotification(
				HTML(paste0(
					"<strong>🎉 Denoising Complete!</strong><br>",
					"Optimal: ", results$optimal_cutpoint$PCs_removed, " PCs removed<br>",
					"Cutpoint: ", results$optimal_cutpoint$cutpoint, "<br>",
					"AAbs detected: ", results$optimal_cutpoint$N_unique_AAbs, "<br>",
					"TP:FP ratio: ", round(results$optimal_cutpoint$TP_FP_ratio, 2)
				)),
				type = "message",
				duration = 10
			)
		}
	})
	
	## PC Visualizer Module ####
	pc_viz_results <- mod_pc_visualizer_server(
		"pc_viz",
		eset_raw = eset_raw,
		denoiser_results = denoiser_results
	)
	
}