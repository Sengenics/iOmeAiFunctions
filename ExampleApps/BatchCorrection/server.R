# Server  Batch Correction ####
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
	
	# ===================================================================
	# EXPSET IMPORT MODULE
	# ===================================================================
	
	expset_data <- mod_expset_import_server("expset_import",debug = run_debug)
	
	# ExpSet_list reactive
	ExpSet_list <- reactive({
		expset_data$ExpSet_list()
	})
	
	# ===================================================================
	# EXPSET SELECTION MODULE
	# ===================================================================
	
	eset_selected_module <- mod_eset_selector_server(
		"eset_select",
		ExpSet_list = ExpSet_list,
		default_selection = "sample_ImputedlogMeanNetI"
	)
	
	# Extract selected ExpressionSet
	eset_selected <- reactive({
		req(eset_selected_module$eset())
		eset_selected_module$eset()
	})
	
	# ===================================================================
	# DATA SELECTION TAB - OUTPUTS
	# ===================================================================
	
	# Status value boxes
	output$status_eset_list <- renderValueBox({
		if (! is.null(ExpSet_list())) {
			source_label <- switch(
				expset_data$source(),
				"uploaded" = "Uploaded ExpSets",
				"package" = "Package ExpSets",
				"ExpressionSets Loaded"
			)
			
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
	
	output$status_selected <- renderValueBox({
		if (!is.null(eset_selected())) {
			valueBox(
				value = "✓",
				subtitle = "ExpSet Selected",
				icon = icon("check-circle"),
				color = "blue"
			)
		} else {
			valueBox(
				value = "—",
				subtitle = "No Selection",
				icon = icon("exclamation-circle"),
				color = "yellow"
			)
		}
	})
	
	output$status_samples <- renderValueBox({
		if (!is.null(eset_selected())) {
			valueBox(
				value = ncol(eset_selected()),
				subtitle = "Samples",
				icon = icon("users"),
				color = "purple"
			)
		} else {
			valueBox(
				value = "—",
				subtitle = "Samples",
				icon = icon("users"),
				color = "light-blue"
			)
		}
	})
	
	output$status_features <- renderValueBox({
		if (!is.null(eset_selected())) {
			valueBox(
				value = nrow(eset_selected()),
				subtitle = "Features",
				icon = icon("dna"),
				color = "teal"
			)
		} else {
			valueBox(
				value = "—",
				subtitle = "Features",
				icon = icon("dna"),
				color = "light-blue"
			)
		}
	})
	
	# ExpSet info
	output$eset_info <- renderPrint({
		req(eset_selected())
		
		cat("Assay: ", eset_selected_module$name(), "\n")
		cat("Samples: ", ncol(eset_selected()), "\n")
		cat("Features: ", nrow(eset_selected()), "\n")
		cat("Available Assays: ", paste(Biobase::assayDataElementNames(eset_selected()), collapse = ", "), "\n")
	})
	
	# Complete ExpSet summary
	output$eset_summary <- renderPrint({
		req(eset_selected())
		
		cat("═══════════════════════════════════════════════════════════\n")
		cat("EXPRESSIONSET SUMMARY\n")
		cat("═══════════════════════════════════════════════════════════\n")
		cat("Selected Assay: ", eset_selected_module$name(), "\n")
		cat("Dimensions: ", nrow(eset_selected()), " features × ", ncol(eset_selected()), " samples\n\n")
		
		cat("Available Assays:\n")
		cat(" ", paste(Biobase::assayDataElementNames(eset_selected()), collapse = ", "), "\n\n")
		
		cat("Sample Metadata (pData) Columns:\n")
		cat(" ", paste(colnames(Biobase::pData(eset_selected())), collapse = ", "), "\n\n")
		
		# Feature metadata
		tryCatch({
			fdata <- Biobase::fData(eset_selected())
			if (!is.null(fdata) && ncol(fdata) > 0) {
				cat("Feature Metadata (fData) Columns:\n")
				cat(" ", paste(colnames(fdata), collapse = ", "), "\n")
			} else {
				cat("Feature Metadata: Not available\n")
			}
		}, error = function(e) {
			cat("Feature Metadata: Not available\n")
		})
	})
	
	# ===================================================================
	# DEBUG FUNCTIONS
	# ===================================================================
	
	observeEvent(input$debug_data, {
		if (! interactive()) {
			showNotification(
				"Debug mode only works in interactive R sessions",
				type = "warning",
				duration = 5
			)
			return(NULL)
		}
		
		message("\n╔═══════════════════════════════════════════════════════════╗")
		message("║          🔍 DEBUG MODE - Heatmap App                     ║")
		message("╚═══════════════════════════════════════════════════════════╝")
		message("\n📊 Session Info:")
		message("   User: DrGarnett")
		message("   Date: ", Sys.time())
		message("\n📍 Available objects:")
		message("   ✓ ExpSet_list()         : Full list of ExpressionSets")
		message("   ✓ eset_selected()       : Selected ExpressionSet")
		message("   ✓ sample_filter         : Sample filter module")
		message("   ✓ feature_filter        : Feature filter module")
		message("\n💡 Useful commands:")
		message("   names(ExpSet_list())")
		message("   quick_inspect_eset(eset_selected())")
		message("   sample_filter$filtered_indices()")
		message("   feature_filter$filtered_indices()")
		message("═══════════════════════════════════════════════════════════\n")
		
		browser()
	})
	
	observeEvent(input$run_diagnostics, {
		output$eset_summary <- renderPrint({
			cat("═══════════════════════════════════════════════════════════\n")
			cat("RUNNING FULL DIAGNOSTICS.. .\n")
			cat("═══════════════════════════════════════════════════════════\n")
			
			if (! is.null(ExpSet_list())) {
				cat("\nEXPRESSIONSET LIST STRUCTURE:\n")
				cat("───────────────────────────────────────────────────────────\n")
				diagnose_ExpSet_list(ExpSet_list())
			}
			
			if (!is.null(eset_selected())) {
				cat("\nSELECTED EXPRESSIONSET INSPECTION:\n")
				cat("───────────────────────────────────────────────────────────\n")
				quick_inspect_eset(eset_selected())
			}
			
			cat("\n═══════════════════════════════════════════════════════════\n")
			cat("DIAGNOSTICS COMPLETE\n")
			cat("═══════════════════════════════════════════════════════════\n")
		})
		
		showNotification(
			"✅ Diagnostics complete!  Check the Data Summary box.",
			type = "message",
			duration = 5
		)
	})
	
	observeEvent(input$quick_check, {
		tryCatch({
			req(eset_selected())
			quick_inspect_eset(eset_selected())
			
			showNotification(
				"✅ Quick check passed! Data structure looks good.",
				type = "message",
				duration = 5
			)
		}, error = function(e) {
			showNotification(
				paste("❌ Validation Error:", e$message),
				type = "error",
				duration = 10
			)
		})
	})
	
	observeEvent(input$goto_configure, {
		updateTabItems(session, "sidebar", "configure")
	})
}