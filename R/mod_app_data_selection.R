#' App Data Selection Module - Reusable UI
#'
#' @param id Module namespace ID
#' @param debug Show debug controls
#' @export
mod_app_data_selection_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		# File Upload Box
		fluidRow(
			actionButton(
				ns("debug"),
				"Debug",
				icon = icon("bug"),
				class = "btn-warning"
			),
			box(
				title = "Import New ExpSet Data",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = FALSE,
				
				p(icon("upload"), strong("Upload a new ExpSet. rds file"), "to import custom expression data"),
				
				mod_expset_import_ui(ns("expset_import"), debug = debug)
			)
		),
		
		# ExpressionSet Selection
		fluidRow(
			box(
				title = "ExpressionSet Data Selection",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				
				p("Select the expression data to analyze."),
				
				fluidRow(
					column(
						width = 6,
						box(
							title = "Select ExpressionSet",
							width = NULL,
							status = "info",
							solidHeader = TRUE,
							
							mod_eset_selector_ui(ns("eset_select")),
							
							hr(),
							
							p(strong("Selected Data Info:")),
							verbatimTextOutput(ns("eset_info"))
						)
					),
					column(
						width = 6,
						box(
							title = "Data Summary",
							width = NULL,
							status = "success",
							solidHeader = TRUE,
							
							verbatimTextOutput(ns("eset_summary"))
						)
					)
				)
			)
		),
		
		# Status indicators
		fluidRow(
			box(
				title = "Data Status",
				width = 12,
				status = "info",
				
				fluidRow(
					column(width = 3, valueBoxOutput(ns("status_eset_list"), width = NULL)),
					column(width = 3, valueBoxOutput(ns("status_selected"), width = NULL)),
					column(width = 3, valueBoxOutput(ns("status_samples"), width = NULL)),
					column(width = 3, valueBoxOutput(ns("status_features"), width = NULL))
				)
			)
		),
		
		# Optional Debug Section
		if (debug) {
			fluidRow(
				box(
					title = "Debug & Diagnostics",
					width = 12,
					status = "warning",
					solidHeader = TRUE,
					collapsible = TRUE,
					collapsed = TRUE,
					
					fluidRow(
						column(3, actionButton(ns("debug_data"), "🔍 Debug", class = "btn-warning btn-block")),
						column(3, actionButton(ns("run_diagnostics"), "📊 Diagnostics", class = "btn-info btn-block")),
						column(3, actionButton(ns("quick_check"), "⚡ Quick Check", class = "btn-secondary btn-block"))
					)
				)
			)
		}
	)
}

#' App Data Selection Module - Reusable Server
#'
#' @param id Module namespace ID
#' @param default_selection Default ExpSet to select
#' @param debug Enable debug mode
#' @export
mod_app_data_selection_server <- function(id, default_selection = NULL, debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Annotation Analysis Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • annotation_analysis() - Analysis results")
				message("\nUseful commands:")
				message("  str(eset())")
				message("  colnames(Biobase::pData(eset()))")
				message("  annotation_analysis()$col_names")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# ExpSet Import Module
		expset_data <- mod_expset_import_server("expset_import", debug = debug)
		
		# ExpSet_list reactive
		ExpSet_list <- reactive({
			expset_data$ExpSet_list()
		})
		
		# ExpSet Selection Module
		eset_selected_module <- mod_eset_selector_server(
			"eset_select",
			ExpSet_list = ExpSet_list,
			default_selection = default_selection
		)
		
		# Extract selected ExpressionSet
		eset_selected <- reactive({
			req(eset_selected_module$eset())
			eset_selected_module$eset()
		})
		
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
				valueBox(value = "✓", subtitle = "ExpSet Selected", icon = icon("check-circle"), color = "blue")
			} else {
				valueBox(value = "—", subtitle = "No Selection", icon = icon("exclamation-circle"), color = "yellow")
			}
		})
		
		output$status_samples <- renderValueBox({
			if (!is.null(eset_selected())) {
				valueBox(value = ncol(eset_selected()), subtitle = "Samples", icon = icon("users"), color = "purple")
			} else {
				valueBox(value = "—", subtitle = "Samples", icon = icon("users"), color = "light-blue")
			}
		})
		
		output$status_features <- renderValueBox({
			if (!is.null(eset_selected())) {
				valueBox(value = nrow(eset_selected()), subtitle = "Features", icon = icon("dna"), color = "teal")
			} else {
				valueBox(value = "—", subtitle = "Features", icon = icon("dna"), color = "light-blue")
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
			cat("  ", paste(Biobase::assayDataElementNames(eset_selected()), collapse = ", "), "\n\n")
			
			cat("Sample Metadata (pData) Columns:\n")
			cat("  ", paste(colnames(Biobase::pData(eset_selected())), collapse = ", "), "\n\n")
			
			tryCatch({
				fdata <- Biobase::fData(eset_selected())
				if (!is.null(fdata) && ncol(fdata) > 0) {
					cat("Feature Metadata (fData) Columns:\n")
					cat("  ", paste(colnames(fdata), collapse = ", "), "\n")
				} else {
					cat("Feature Metadata: Not available\n")
				}
			}, error = function(e) {
				cat("Feature Metadata: Not available\n")
			})
		})
		
		# Debug functions
		if (debug) {
			observeEvent(input$debug_data, {
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
				})
			})
			
			observeEvent(input$quick_check, {
				tryCatch({
					req(eset_selected())
					quick_inspect_eset(eset_selected())
					showNotification("✅ Quick check passed!", type = "message", duration = 5)
				}, error = function(e) {
					showNotification(paste("❌ Error:", e$message), type = "error", duration = 10)
				})
			})
		}
		
		# Return values
		return(list(
			eset = eset_selected,
			eset_name = eset_selected_module$name,
			ExpSet_list = ExpSet_list,
			source = expset_data$source
		))
	})
}