#' pFC Analysis Shiny Module - UI
#'
#' UI component for pFC parameter selection in Shiny apps
#'
#' @param id Module namespace ID
#' @param use_box Logical; wrap in shinydashboard box? (default: FALSE)
#'
#' @export
pFC_UI <- function(id, use_box = FALSE) {
	ns <- NS(id)
	
	# Core UI elements
	ui_content <- tagList(
		# Parameters Section
		h3("pFC Analysis Parameters"),
		fluidRow(
			column(
				width = 6,
				selectInput(
					ns("var"),
					"Variable of Interest:",
					choices = NULL,
					selected = NULL
				),
				
				selectInput(
					ns("groupPos"),
					"Positive Group (Case):",
					choices = NULL,
					selected = NULL
				),
				
				selectInput(
					ns("groupNeg"),
					"Negative Group (Control):",
					choices = NULL,
					selected = NULL
				),
				
				numericInput(
					ns("fold_change"),
					"Fold Change Threshold:",
					value = 2,
					min = 1,
					max = 10,
					step = 0.1
				)
			),
			
			column(
				width = 6,
				numericInput(
					ns("p_val"),
					"P-value Threshold:",
					value = 0.2,
					min = 0.001,
					max = 1,
					step = 0.01
				),
				
				textInput(
					ns("descriptor"),
					"Output Descriptor:",
					value = "pFC_analysis"
				),
				
				selectInput(
					ns("add_anno"),
					"Additional Annotations:",
					choices = NULL,
					selected = NULL,
					multiple = TRUE
				),
				
				numericInput(
					ns("cores"),
					"Number of Cores:",
					value = 1,
					min = 1,
					max = parallel::detectCores(),
					step = 1
				)
			)
		),
		
		fluidRow(
			column(
				width = 12,
				hr(),
				actionButton(
					ns("run_analysis"),
					"Run pFC Analysis",
					icon = icon("play"),
					class = "btn-success btn-lg",
					style = "width: 100%;"
				)
			)
		),
		
		hr(),
		
		# Results Section
		h3("Analysis Results"),
		
		tabsetPanel(
			id = ns("results_tabs"),
			
			tabPanel(
				"Summary",
				br(),
				verbatimTextOutput(ns("summary_text")),
				br(),
				h4("Significant Results"),
				DT::DTOutput(ns("sig_table"))
			),
			
			tabPanel(
				"Violin Plots",
				br(),
				uiOutput(ns("violin_ui"))
			),
			
			tabPanel(
				"Heatmaps",
				br(),
				h4("Heatmap - Manual Sort"),
				plotOutput(ns("heatmap_manual"), height = "800px"),
				hr(),
				h4("Heatmap - Manual Sort (Row-Centered)"),
				plotOutput(ns("heatmap_manual_RC"), height = "800px"),
				hr(),
				h4("Heatmap - Clustered (Row-Centered)"),
				plotOutput(ns("heatmap_clustered_RC"), height = "800px")
			),
			
			tabPanel(
				"Data Tables",
				br(),
				h4("All Statistics"),
				DT::DTOutput(ns("all_stats_table")),
				br(),
				h4("Fold Change Data"),
				DT::DTOutput(ns("fc_table"))
			),
			
			tabPanel(
				"Download",
				br(),
				h4("Download Results"),
				fluidRow(
					column(
						width = 6,
						downloadButton(ns("download_sig"), "Download Significant Results (.csv)", class = "btn-primary btn-block"),
						br(), br(),
						downloadButton(ns("download_all_stats"), "Download All Statistics (.csv)", class = "btn-primary btn-block"),
						br(), br(),
						downloadButton(ns("download_fc"), "Download FC Data (.csv)", class = "btn-primary btn-block")
					),
					column(
						width = 6,
						downloadButton(ns("download_violins"), "Download Violin Plots (.pdf)", class = "btn-primary btn-block"),
						br(), br(),
						downloadButton(ns("download_heatmaps"), "Download Heatmaps (.pdf)", class = "btn-primary btn-block"),
						br(), br(),
						downloadButton(ns("download_all"), "Download All Results (.zip)", class = "btn-success btn-block")
					)
				)
			)
		)
	)
	
	# Optionally wrap in shinydashboard box
	if (use_box) {
		if (!requireNamespace("shinydashboard", quietly = TRUE)) {
			stop("shinydashboard package required when use_box = TRUE")
		}
		shinydashboard::box(
			title = "pFC Analysis",
			status = "primary",
			solidHeader = TRUE,
			width = 12,
			collapsible = TRUE,
			ui_content
		)
	} else {
		ui_content
	}
}

#' pFC Analysis Shiny Module - Server
#'
#' Server component for pFC analysis in Shiny apps
#'
#' @param id Module namespace ID
#' @param eset_reactive Reactive expression returning an ExpressionSet object
#' @param default_var_reactive Reactive expression returning the default variable column name
#'
#' @export
pFC_Server <- function(id, eset_reactive, default_var_reactive = reactive(NULL)) {
	moduleServer(id, function(input, output, session) {
		
		# Reactive values to store results
		rv <- reactiveValues(
			pfc_results = NULL,
			pfc_plots = NULL,
			current_violin_page = 1,
			var_initialized = FALSE  # Track if var has been set to default
		)
		
		# Update UI inputs when ExpressionSet changes
		observe({
			req(eset_reactive())
			eset <- eset_reactive()
			metadata_cols <- colnames(Biobase::pData(eset))
			
			# Get default variable
			default_var <- default_var_reactive()
			
			# Determine selected variable
			if (!is.null(default_var) && default_var %in% metadata_cols) {
				selected_var <- default_var
			} else {
				selected_var <- metadata_cols[1]
			}
			
			# Update variable selector
			updateSelectInput(
				session,
				"var",
				choices = metadata_cols,
				selected = selected_var
			)
			
			# Update additional annotation selector
			updateSelectInput(
				session,
				"add_anno",
				choices = metadata_cols
			)
			
			rv$var_initialized <- TRUE
		})
		
		# Update when default_var changes (if user changes AP_SampleGroup_column)
		observe({
			req(eset_reactive(), rv$var_initialized)
			
			default_var <- default_var_reactive()
			
			if (!is.null(default_var)) {
				eset <- eset_reactive()
				metadata_cols <- colnames(Biobase::pData(eset))
				
				if (default_var %in% metadata_cols) {
					updateSelectInput(
						session,
						"var",
						selected = default_var
					)
				}
			}
		})
		
		# Update group selectors when variable changes
		observe({
			req(eset_reactive(), input$var)
			eset <- eset_reactive()
			
			var_values <- unique(as.character(Biobase::pData(eset)[[input$var]]))
			var_values <- var_values[!is.na(var_values) & var_values != "" & var_values != "PN"]
			
			updateSelectInput(
				session,
				"groupPos",
				choices = var_values,
				selected = var_values[1]
			)
			
			updateSelectInput(
				session,
				"groupNeg",
				choices = var_values,
				selected = if (length(var_values) > 1) var_values[2] else NULL
			)
		})
		
		# Auto-generate descriptor based on groups
		observe({
			req(input$groupPos, input$groupNeg)
			descriptor <- paste0(input$groupPos, "_vs_", input$groupNeg, "_pFC")
			updateTextInput(session, "descriptor", value = descriptor)
		})
		
		# Run analysis
		observeEvent(input$run_analysis, {
			req(eset_reactive(), input$var, input$groupPos, input$groupNeg)
			
			# Validate groups are different
			if (input$groupPos == input$groupNeg) {
				showNotification("Positive and Negative groups must be different!", type = "error", duration = 5)
				return()
			}
			
			# Show progress
			showNotification("Running pFC analysis...", type = "message", duration = NULL, id = "pfc_progress")
			
			tryCatch({
				# Run processing
				rv$pfc_results <- pFC_process(
					eset = eset_reactive(),
					var = input$var,
					groupPos = input$groupPos,
					groupNeg = input$groupNeg,
					fold_change = input$fold_change,
					p_val = input$p_val,
					PSA_flag = FALSE,  # Disable PSA in shiny app
					cores = input$cores
				)
				
				# Generate plots
				rv$pfc_plots <- pFC_plot(
					pfc_results = rv$pfc_results,
					add_anno = input$add_anno
				)
				
				removeNotification("pfc_progress")
				showNotification("pFC analysis completed!", type = "message", duration = 3)
				
			}, error = function(e) {
				removeNotification("pfc_progress")
				showNotification(paste("Error:", e$message), type = "error", duration = 10)
				print(e)  # Print to console for debugging
			})
		})
		
		# Summary text
		output$summary_text <- renderPrint({
			req(rv$pfc_results)
			
			cat("pFC Analysis Summary\n")
			cat("====================\n\n")
			cat("Parameters:\n")
			cat("  Variable:", rv$pfc_results$parameters$var, "\n")
			cat("  Positive Group:", rv$pfc_results$parameters$groupPos, "\n")
			cat("  Negative Group:", rv$pfc_results$parameters$groupNeg, "\n")
			cat("  Fold Change Threshold:", rv$pfc_results$parameters$fold_change, "\n")
			cat("  P-value Threshold:", rv$pfc_results$parameters$p_val, "\n\n")
			
			cat("Results:\n")
			cat("  Total Features Tested:", nrow(rv$pfc_results$pfc_stats), "\n")
			cat("  Significant Hits:", nrow(rv$pfc_results$pfc_significant), "\n")
			cat("  Sample Size (", rv$pfc_results$parameters$groupPos, "):", 
					sum(rv$pfc_results$metadata[[rv$pfc_results$parameters$var]] == rv$pfc_results$parameters$groupPos), "\n")
			cat("  Sample Size (", rv$pfc_results$parameters$groupNeg, "):",
					sum(rv$pfc_results$metadata[[rv$pfc_results$parameters$var]] == rv$pfc_results$parameters$groupNeg), "\n")
		})
		
		# Significant results table
		output$sig_table <- DT::renderDT({
			req(rv$pfc_results)
			DT::datatable(
				rv$pfc_results$pfc_significant,
				options = list(pageLength = 10, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		# All stats table
		output$all_stats_table <- DT::renderDT({
			req(rv$pfc_results)
			DT::datatable(
				rv$pfc_results$pfc_stats,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		# FC table
		output$fc_table <- DT::renderDT({
			req(rv$pfc_results)
			DT::datatable(
				rv$pfc_results$fc_data,
				options = list(pageLength = 25, scrollX = TRUE),
				rownames = FALSE
			)
		})
		
		# Violin plots UI
		output$violin_ui <- renderUI({
			req(rv$pfc_plots$violin_plots)
			
			n_plots <- length(rv$pfc_plots$violin_plots)
			
			tagList(
				if (n_plots > 1) {
					fluidRow(
						column(
							width = 12,
							sliderInput(
								session$ns("violin_page"),
								"Page:",
								min = 1,
								max = n_plots,
								value = 1,
								step = 1,
								width = "100%"
							)
						)
					)
				},
				plotOutput(session$ns("violin_plot"), height = "800px")
			)
		})
		
		# Render violin plot
		output$violin_plot <- renderPlot({
			req(rv$pfc_plots$violin_plots)
			
			page_num <- if (!is.null(input$violin_page)) input$violin_page else 1
			rv$pfc_plots$violin_plots[[page_num]]
		})
		
		# Heatmaps
		output$heatmap_manual <- renderPlot({
			req(rv$pfc_plots$heatmap_manual)
			print(rv$pfc_plots$heatmap_manual)
		})
		
		output$heatmap_manual_RC <- renderPlot({
			req(rv$pfc_plots$heatmap_manual_RC)
			print(rv$pfc_plots$heatmap_manual_RC)
		})
		
		output$heatmap_clustered_RC <- renderPlot({
			req(rv$pfc_plots$heatmap_clustered_RC)
			print(rv$pfc_plots$heatmap_clustered_RC)
		})
		
		# Download handlers
		output$download_sig <- downloadHandler(
			filename = function() {
				paste0("pFC_significant_results_", Sys.Date(), ".csv")
			},
			content = function(file) {
				req(rv$pfc_results)
				write.csv(rv$pfc_results$pfc_significant, file, row.names = FALSE)
			}
		)
		
		output$download_all_stats <- downloadHandler(
			filename = function() {
				paste0("pFC_all_statistics_", Sys.Date(), ".csv")
			},
			content = function(file) {
				req(rv$pfc_results)
				write.csv(rv$pfc_results$pfc_stats, file, row.names = FALSE)
			}
		)
		
		output$download_fc <- downloadHandler(
			filename = function() {
				paste0("pFC_fold_changes_", Sys.Date(), ".csv")
			},
			content = function(file) {
				req(rv$pfc_results)
				write.csv(rv$pfc_results$fc_data, file, row.names = FALSE)
			}
		)
		
		output$download_violins <- downloadHandler(
			filename = function() {
				paste0("pFC_violin_plots_", Sys.Date(), ".pdf")
			},
			content = function(file) {
				req(rv$pfc_plots$violin_plots)
				pdf(file, width = 15, height = 10)
				for (p in rv$pfc_plots$violin_plots) {
					print(p)
				}
				dev.off()
			}
		)
		
		output$download_heatmaps <- downloadHandler(
			filename = function() {
				paste0("pFC_heatmaps_", Sys.Date(), ".pdf")
			},
			content = function(file) {
				req(rv$pfc_plots$heatmap_manual)
				pdf(file, width = 15, height = 10)
				print(rv$pfc_plots$heatmap_manual)
				print(rv$pfc_plots$heatmap_manual_RC)
				print(rv$pfc_plots$heatmap_clustered_RC)
				dev.off()
			}
		)
		
		output$download_all <- downloadHandler(
			filename = function() {
				paste0("pFC_complete_results_", Sys.Date(), ".zip")
			},
			content = function(file) {
				req(rv$pfc_results, rv$pfc_plots)
				
				# Create temporary directory
				temp_dir <- tempfile()
				dir.create(temp_dir)
				
				# Save all files to temp directory
				pFC_save(
					pfc_results = rv$pfc_results,
					pfc_plots = rv$pfc_plots,
					descriptor = temp_dir,
					plot_width = 15,
					plot_height = 10
				)
				
				# Create zip file
				zip::zip(file, files = list.files(temp_dir, full.names = TRUE), mode = "cherry-pick")
				
				# Clean up
				unlink(temp_dir, recursive = TRUE)
			}
		)
		
		# Return reactive results for use elsewhere in the app
		return(reactive({
			list(
				results = rv$pfc_results,
				plots = rv$pfc_plots
			)
		}))
	})
}