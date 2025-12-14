# Server : HeatMap ####
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
	
	# ===================================================================
	# CONFIGURE TAB
	# ===================================================================
	
	# Sample annotation selector
	output$sample_anno_selector <- renderUI({
		req(eset_selected())
		
		pdata_cols <- colnames(Biobase::pData(eset_selected()))
		
		selectInput(
			"sample_anno_cols",
			"Sample Annotations (pData):",
			choices = pdata_cols,
			selected = NULL,
			multiple = TRUE
		)
	})
	
	# Feature annotation selector
	output$feature_anno_selector <- renderUI({
		req(eset_selected())
		
		tryCatch({
			fdata <- Biobase::fData(eset_selected())
			
			if (!is.null(fdata) && ncol(fdata) > 0) {
				fdata_cols <- colnames(fdata)
				
				selectInput(
					"feature_anno_cols",
					"Feature Annotations (fData):",
					choices = fdata_cols,
					selected = NULL,
					multiple = TRUE
				)
			} else {
				p(em("No feature metadata (fData) available"))
			}
		}, error = function(e) {
			p(em("No feature metadata (fData) available"))
		})
	})
	
	# Assay info
	output$assay_info <- renderPrint({
		req(input$assay_name, eset_selected())
		
		assay_data <- Biobase::assayDataElement(eset_selected(), input$assay_name)
		
		cat("Assay: ", input$assay_name, "\n")
		cat("Class: ", class(assay_data), "\n")
		cat("Dimensions: ", nrow(assay_data), " × ", ncol(assay_data), "\n")
		cat("Range: [", min(assay_data, na.rm = TRUE), ", ", max(assay_data, na.rm = TRUE), "]\n")
		cat("NA values: ", sum(is.na(assay_data)), "\n")
	})
	
	# Filter modules
	sample_filter <- mod_data_filter_server(
		"sample_filter",
		eset = eset_selected,
		data_type = "pData", debug = run_debug
		
	)
	
	feature_filter <- mod_data_filter_server(
		"feature_filter",
		eset = eset_selected,
		data_type = "fData", debug = run_debug
	)
	
	# Configuration summary
	output$config_summary <- renderUI({
		req(eset_selected())
		
		n_samples <- if (! is.null(sample_filter)) {
			sample_filter$n_filtered()
		} else {
			ncol(eset_selected())
		}
		
		n_features <- if (!is.null(feature_filter)) {
			feature_filter$n_filtered()
		} else {
			nrow(eset_selected())
		}
		
		sample_annos <- if (!is.null(input$sample_anno_cols)) {
			length(input$sample_anno_cols)
		} else {
			0
		}
		
		feature_annos <- if (!is.null(input$feature_anno_cols)) {
			length(input$feature_anno_cols)
		} else {
			0
		}
		
		tagList(
			h4("Current Configuration:"),
			tags$ul(
				tags$li(strong("Assay:"), input$assay_name %||% "Not selected"),
				tags$li(strong("Samples:"), paste(n_samples, "selected")),
				tags$li(strong("Features:"), paste(n_features, "selected")),
				tags$li(strong("Sample Annotations:"), sample_annos),
				tags$li(strong("Feature Annotations:"), feature_annos)
			)
		)
	})
	
	observeEvent(input$goto_heatmap, {
		updateTabItems(session, "sidebar", "heatmap")
	})
	
	# ===================================================================
	# HEATMAP MODULE
	# ===================================================================
	
	# heatmap_results <- heatmap_eset_module_server(
	# 	"heatmap",
	# 	eset = eset_selected,
	# 	assay_name = reactive(input$assay_name),
	# 	sample_filter = sample_filter,
	# 	feature_filter = feature_filter,
	# 	sample_anno_cols = reactive(input$sample_anno_cols),
	# 	feature_anno_cols = reactive(input$feature_anno_cols),
	# 	mode = "basic",
	# 	features = list()
	# )
	
	# Enhanced heatmap controls
	heatmap_controls <- mod_heatmap_controls_enhanced_server(
		"heatmap_controls",
		eset = eset_selected,
		debug = run_debug
	)
	
	# Enhanced heatmap display
	heatmap_display <- mod_heatmap_display_enhanced_server(
		"heatmap_display",
		eset = eset_selected,
		assay_name = eset_selected_module$name,
		controls = heatmap_controls,
		plot_path = "plots",  # or reactive(some_path)
		debug = run_debug
	)
	
	
	
	# ===================================================================
	# EXPORT HANDLERS
	# ===================================================================
	
	output$download_plot <- downloadHandler(
		filename = function() {
			paste0("heatmap_", Sys.Date(), ".png")
		},
		content = function(file) {
			req(heatmap_results())
			
			data <- heatmap_results()
			
			png(file, width = 12, height = 10, units = "in", res = 300)
			
			heatmap_args <- list(
				mat = data$m,
				cluster_cols = TRUE,
				cluster_rows = TRUE,
				show_rownames = ifelse(data$rows <= 50, TRUE, FALSE),
				show_colnames = ifelse(data$cols <= 50, TRUE, FALSE)
			)
			
			if (! is.null(data$sample_meta) && ncol(data$sample_meta) > 0) {
				heatmap_args$annotation_col <- data$sample_meta
				heatmap_args$annotation_colors <- data$sample_anno_col
			}
			
			if (!is.null(data$feature_meta) && ncol(data$feature_meta) > 0) {
				heatmap_args$annotation_row <- data$feature_meta
				if (! is.null(data$feature_anno_col)) {
					if (is.null(heatmap_args$annotation_colors)) {
						heatmap_args$annotation_colors <- data$feature_anno_col
					} else {
						heatmap_args$annotation_colors <- c(
							heatmap_args$annotation_colors,
							data$feature_anno_col
						)
					}
				}
			}
			
			do.call(pheatmap::pheatmap, heatmap_args)
			
			dev.off()
		}
	)
	
	output$download_data <- downloadHandler(
		filename = function() {
			paste0("heatmap_data_", Sys.Date(), ".csv")
		},
		content = function(file) {
			req(heatmap_results())
			
			data <- heatmap_results()
			write.csv(data$m, file, row.names = TRUE)
		}
	)
	
	output$download_eset <- downloadHandler(
		filename = function() {
			paste0("filtered_eset_", Sys.Date(), ".rds")
		},
		content = function(file) {
			req(eset_selected())
			
			# Create filtered ExpSet
			sample_idx <- if (!is.null(sample_filter)) {
				sample_filter$filtered_indices()
			} else {
				1:ncol(eset_selected())
			}
			
			feature_idx <- if (!is.null(feature_filter)) {
				feature_filter$filtered_indices()
			} else {
				1:nrow(eset_selected())
			}
			
			filtered_eset <- eset_selected()[feature_idx, sample_idx]
			
			saveRDS(filtered_eset, file)
		}
	)
}