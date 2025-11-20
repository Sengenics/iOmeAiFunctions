# Server ####
server <- function(input, output, session) {
	
	
	
	options(shiny.maxRequestSize = 100*1024^2)
	
	observeEvent(input$debug,{
		browser()
	})
	
	# ===================================================================
	# NEW: FILE UPLOAD AND IMPORT FUNCTIONALITY
	# ===================================================================
	
	# Reactive value to store uploaded ExpSet data
	uploaded_expset <- reactiveVal(NULL)
	upload_status <- reactiveVal("No file uploaded")
	upload_success <- reactiveVal(FALSE)
	
	# Load ExpSet when button is clicked
	observeEvent(input$load_expset, {
		req(input$expset_file)
		
		upload_status("Loading...")
		upload_success(FALSE)
		
		tryCatch({
			# Read the RDS file
			message("Loading ExpSet from: ", input$expset_file$name)
			expset_data <- readRDS(input$expset_file$datapath)
			print(names(expset_data))
			
			# Validate the uploaded data
			is_valid <- FALSE
			error_msg <- ""
			
			# Check if it's an ExpressionSet object
			if (inherits(expset_data, "ExpressionSet")) {
				message("✓ Single ExpressionSet detected")
				# Wrap in a list with a default name
				expset_data <- list(uploaded_data = expset_data)
				is_valid <- TRUE
			}
			# Check if it's a list of ExpressionSets
			else if (is.list(expset_data)) {
				# Verify all elements are ExpressionSets
				all_eset <- all(sapply(expset_data, function(x) inherits(x, "ExpressionSet")))
				
				if (all_eset && length(expset_data) > 0) {
					message("✓ List of ", length(expset_data), " ExpressionSets detected")
					is_valid <- TRUE
				} else {
					error_msg <- "List does not contain valid ExpressionSet objects"
				}
			} else {
				error_msg <- paste("Invalid data type:", class(expset_data)[1])
			}
			
			if (is_valid) {
				# Additional validation: check that ExpressionSets have required components
				(validation_results <- sapply(expset_data, function(eset) {
					#has_exprs <- !is.null(tryCatch(Biobase::exprs(eset), error = function(e) NULL))
					has_pdata <- !is.null(tryCatch(Biobase::pData(eset), error = function(e) NULL))
					#has_exprs &&
					has_pdata
				}))
				
				(validation_results_exprs <- sapply(expset_data, function(eset) {
					has_exprs <- !is.null(tryCatch(Biobase::exprs(eset), error = function(e) NULL))
					#has_pdata <- !is.null(tryCatch(Biobase::pData(eset), error = function(e) NULL))
					has_exprs
					#has_pdata
				}))
				
				if (all(validation_results)) {
					# Store the loaded ExpSet
					uploaded_expset(expset_data)
					upload_status(paste("✅ Successfully loaded:", input$expset_file$name))
					upload_success(TRUE)
					
					showNotification(
						HTML(paste0(
							"<strong>✅ ExpSet loaded successfully!</strong><br>",
							"File: ", input$expset_file$name, "<br>",
							"Contains: ", length(expset_data), " ExpressionSet(s)"
						)),
						type = "message",
						duration = 8
					)
					
					message("✓ ExpSet validation passed")
					message("✓ Available assays: ", paste(names(expset_data), collapse = ", "))
				} else {
					error_msg <- "ExpressionSets missing required components (exprs or pData)"
					upload_status(paste("❌ Error:", error_msg))
					showNotification(error_msg, type = "error", duration = 10)
				}
			} else {
				upload_status(paste("❌ Error:", error_msg))
				showNotification(
					HTML(paste0(
						"<strong>❌ Invalid ExpSet file</strong><br>",
						error_msg, "<br>",
						"Please upload a valid ExpressionSet or list of ExpressionSets"
					)),
					type = "error",
					duration = 10
				)
			}
			
		}, error = function(e) {
			error_message <- paste("Error loading file:", e$message)
			upload_status(paste("❌", error_message))
			upload_success(FALSE)
			
			showNotification(
				HTML(paste0("<strong>❌ Error loading ExpSet:</strong><br>", e$message)),
				type = "error",
				duration = 10
			)
			
			message("✗ Error loading ExpSet: ", e$message)
		})
	})
	
	# Render upload status with styling
	output$expset_status_ui <- renderUI({
		status <- upload_status()
		success <- upload_success()
		
		if (success) {
			div(
				style = "padding: 10px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; color: #155724;",
				icon("check-circle"),
				strong(status)
			)
		} else if (grepl("Error|❌", status)) {
			div(
				style = "padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;",
				icon("exclamation-triangle"),
				strong(status)
			)
		} else if (status == "Loading...") {
			div(
				style = "padding: 10px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; color: #0c5460;",
				icon("spinner", class = "fa-spin"),
				strong(" Loading...")
			)
		} else {
			div(
				style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; color: #6c757d;",
				icon("info-circle"),
				status
			)
		}
	})
	
	# ===================================================================
	# MODIFIED: Load ExpSet_list (default or uploaded)
	# ===================================================================
	
	# Load ExpSet_list
	# ExpSet_list <- reactive({
	# 	# Priority 1: Use uploaded data if available
	# 	if (!is.null(uploaded_expset())) {
	# 		message("✓ Using uploaded ExpSet data")
	# 		return(uploaded_expset())
	# 	}
	# 	
	# 	# Priority 2: Try to load from default locations
	# 	possible_paths <- c(
	# 		"ExampleData/ExpSet_list.rds",
	# 		"../ExampleData/ExpSet_list.rds",
	# 		"../../ExampleData/ExpSet_list.rds",
	# 		"../../../ExampleData/ExpSet_list.rds",
	# 		system.file("extdata", "ExpSet_list.rds", package = "iOmeAiFunctions")
	# 	)
	# 	
	# 	for (path in possible_paths) {
	# 		if (file.exists(path)) {
	# 			message("✓ Loading ExpSet_list from: ", path)
	# 			return(readRDS(path))
	# 		}
	# 	}
	# 	
	# 	showNotification(
	# 		"ℹ️ No default ExpSet_list.rds found. Please upload an ExpSet.rds file above.",
	# 		type = "warning",
	# 		duration = 8
	# 	)
	# 	return(NULL)
	# })
	
	ExpSet_list <- reactive({
		# Priority 1: Use uploaded data if available
		if (!is.null(uploaded_expset())) {
			message("✓ Using uploaded ExpSet data")
			return(uploaded_expset())
		}
		
		# Priority 2: Load from package data
		tryCatch({
			data(ExpSet, package = "iOmeAiFunctions", envir = environment())
			if (exists("ExpSet", inherits = FALSE)) {
				message("✓ Using ExpSet from iOmeAiFunctions package")
				return(ExpSet)
			}
		}, error = function(e) {
			message("ℹ️ ExpSet not found in package: ", e$message)
		})
		
		# No data available
		showNotification(
			"ℹ️ No ExpSet data available. Please upload an ExpSet.rds file above.",
			type = "warning",
			duration = 8
		)
		return(NULL)
	})
	
	# Raw/NetI ExpressionSet selector
	eset_raw_selected <- mod_eset_selector_server(
		"eset_raw",
		ExpSet_list = ExpSet_list,
		default_selection = "sample_ImputedlogMeanNetI"
	)
	
	# Normalized ExpressionSet selector
	eset_norm_selected <- mod_eset_selector_server(
		"eset_norm",
		ExpSet_list = ExpSet_list,
		default_selection = "sample_loess_normalised"
	)
	
	# Extract the actual ExpressionSets
	eset_raw <- reactive({
		req(eset_raw_selected$eset())
		eset_raw_selected$eset()
	})
	
	eset_norm <- reactive({
		if (!is.null(eset_norm_selected$eset())) {
			eset_norm_selected$eset()
		} else {
			NULL
		}
	})
	
	# Status value boxes
	output$status_eset_list <- renderValueBox({
		if (!is.null(ExpSet_list())) {
			# Check if using uploaded data
			source_label <- if (!is.null(uploaded_expset())) {
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
	
	# Raw data info
	output$raw_info <- renderPrint({
		req(eset_raw())
		
		cat("Assay: ", eset_raw_selected$name(), "\n")
		cat("Samples: ", ncol(eset_raw()), "\n")
		cat("Features: ", nrow(eset_raw()), "\n")
	})
	
	# Normalized data info
	output$norm_info <- renderPrint({
		if (!is.null(eset_norm())) {
			cat("Assay: ", eset_norm_selected$name(), "\n")
			cat("Samples: ", ncol(eset_norm()), "\n")
			cat("Features: ", nrow(eset_norm()), "\n")
		} else {
			cat("Not selected (optional)")
		}
	})
	
	# Complete ExpressionSet summary
	output$eset_summary <- renderPrint({
		req(eset_raw())
		
		cat("═══════════════════════════════════════════════════════════\n")
		
		# Show data source
		if (!is.null(uploaded_expset())) {
			cat("DATA SOURCE: Uploaded File\n")
		} else {
			cat("DATA SOURCE: Default ExpSet_list.rds\n")
		}
		cat("═══════════════════════════════════════════════════════════\n")
		
		cat("\nRAW/NETI DATA\n")
		cat("═══════════════════════════════════════════════════════════\n")
		cat("Selected Assay: ", eset_raw_selected$name(), "\n")
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
			cat("Selected Assay: ", eset_norm_selected$name(), "\n")
			cat("Dimensions: ", nrow(eset_norm()), " features × ", ncol(eset_norm()), " samples\n")
			cat("Available Assays: ", paste(Biobase::assayDataElementNames(eset_norm()), collapse = ", "), "\n")
		}
	})
	
	# ===================================================================
	# DEBUG FUNCTIONS
	# ===================================================================
	
	# Debug button - Data Selection
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
		message("   ✓ uploaded_expset()     : Uploaded ExpSet data (if any)")
		message("   ✓ eset_raw()            : Selected raw/NetI ExpressionSet")
		message("   ✓ eset_norm()           : Selected normalized ExpressionSet")
		message("   ✓ eset_raw_selected     : Selection module reactive")
		message("   ✓ eset_norm_selected    : Selection module reactive")
		message("\n💡 Useful commands:")
		message("")
		message("   # List all ExpressionSets")
		message("   names(ExpSet_list())")
		message("")
		message("   # Check if using uploaded data")
		message("   is.null(uploaded_expset())")
		message("")
		message("   # Full diagnostics")
		message("   diagnose_ExpSet_list(ExpSet_list())")
		message("")
		message("   # Quick inspect selected data")
		message("   quick_inspect_eset(eset_raw())")
		message("   quick_inspect_eset(eset_norm())")
		message("")
		message("   # Check what's selected")
		message("   eset_raw_selected$name()")
		message("   eset_norm_selected$name()")
		message("")
		message("   # Manual inspection")
		message("   str(eset_raw())")
		message("   class(Biobase::exprs(eset_raw()))")
		message("   dim(Biobase::exprs(eset_raw()))")
		message("   head(Biobase::exprs(eset_raw())[, 1:5])")
		message("")
		message("   # Check metadata")
		message("   meta <- Biobase::pData(eset_raw())")
		message("   colnames(meta)")
		message("   table(meta$Sample_Group)")
		message("")
		message("   # Validate")
		message("   validate_denoise_inputs(eset_raw())")
		message("\n⌨️  Commands:")
		message("   c    Continue")
		message("   Q    Quit browser")
		message("   n    Next (step through)")
		message("═══════════════════════════════════════════════════════════\n")
		
		browser()
	})
	
	# Run diagnostics button
	observeEvent(input$run_diagnostics, {
		
		output$eset_summary <- renderPrint({
			cat("═══════════════════════════════════════════════════════════\n")
			cat("RUNNING FULL DIAGNOSTICS...\n")
			cat("═══════════════════════════════════════════════════════════\n")
			cat("User: DrGarnett\n")
			cat("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"), "\n")
			
			if (!is.null(uploaded_expset())) {
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
	
	# Quick check button
	observeEvent(input$quick_check, {
		
		# Run quick validation
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
	
	# Navigate to denoiser tab
	observeEvent(input$goto_denoiser, {
		updateTabItems(session, "sidebar", "denoise")
		
		showNotification(
			"📊 Ready to run denoiser analysis!",
			type = "message",
			duration = 3
		)
	})
	
	# ===================================================================
	# DENOISER MODULE
	# ===================================================================
	
	# Call denoiser module
	denoiser_results <- mod_denoiser_server(
		"denoiser",
		ExpSet_list = ExpSet_list,
		eset_raw = eset_raw,
		eset_norm = eset_norm
	)
	
	# Log results when denoising completes
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
	
	## Visualisation Module #####
	# In the server function, modify the denoiser call to capture results:
	
	# Call denoiser module and capture results
	# denoiser_results <- mod_denoiser_server(
	# 	"denoiser",
	# 	eset_raw = eset_raw,
	# 	eset_norm = eset_norm
	# )
	
	# PC Visualizer module - pass denoiser results
	pc_viz_results <- mod_pc_visualizer_server(
		"pc_viz",
		eset_raw = eset_raw,
		denoiser_results = denoiser_results  # <-- Pass the denoiser results here
	)
	
	## PN LIMMA ####
	
	# Call PN limma module
	pn_limma_results <- mod_pn_limma_server(
		"pn_limma",
		eset_raw = eset_raw,
		eset_norm = eset_norm
	)
	# 
	# Pass results to denoiser module
	denoiser_results <- mod_denoiser_server(
		"denoiser",
		ExpSet_list = ExpSet_list,
		eset_raw = eset_raw,
		eset_norm = eset_norm,
		pn_limma_results = pn_limma_results  # <-- Add this
	)
	
	
}