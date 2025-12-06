#' Batch Effect Visualization Module - UI
#'
#' Compare batch effects before and after correction using multiple visualization methods
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_batch_visualization_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		
		fluidRow(
			box(
				title = "Batch Effect Visualization Settings",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				uiOutput(ns("debug_ui")),
				textOutput(ns('eset_name_text')),
				
				fluidRow(
					box(
						title = "Corrected Data Selection",
						width = 12,
						status = "info",
						solidHeader = TRUE,
						collapsible = TRUE,
						
						fluidRow(
							column(
								width = 4,
								radioButtons(
									ns("corrected_source"),
									"Corrected Data Source:",
									choices = c(
										"Use ComBat from this session" = "session",
										"Load from ExpressionSet assay" = "assay"
									),
									selected = "session"
								)
							),
							column(
								width = 8,
								conditionalPanel(
									condition = "input.corrected_source == 'assay'",
									ns = ns,
									textInput(
										ns("corrected_suffix"),
										"Corrected Assay Suffix:",
										value = "_ComBat",
										placeholder = "_ComBat"
									),
									helpText("Will append this to the current assay name"),
									uiOutput(ns("corrected_assay_selector"))  # Shows status, not dropdown
								)
							)
							
							# column(
							# 	width = 4,
							# 	conditionalPanel(
							# 		condition = "input.corrected_source == 'assay'",
							# 		ns = ns,
							# 		textInput(
							# 			ns("corrected_suffix"),
							# 			"Corrected Assay Suffix:",
							# 			value = "_ComBat",
							# 			placeholder = "_ComBat"
							# 		),
							# 		helpText("Will append this to the selected assay name")
							# 	)
							# ),
							# column(
							# 	width = 4,
							# 	uiOutput(ns("corrected_assay_selector"))
							# )
						),
						
						hr(),
						
						uiOutput(ns("corrected_data_status"))
					)
				),
				
				fluidRow(
					column(
						width = 3,
						selectInput(
							ns("color_by"),
							"Color by:",
							choices = NULL
						)
					),
					column(
						width = 3,
						selectInput(
							ns("shape_by"),
							"Shape by (t-SNE only):",
							choices = NULL
						)
					),
					column(
						width = 3,
						selectInput(
							ns("qc_column"),
							"QC Column (for TR identification):",
							choices = NULL,
							selected = "QC"
						),
						helpText("Used to identify technical replicates on dendrogram")
					),
					column(
						width = 3,
						selectInput(
							ns("label_column"),
							"Label Column (for TR grouping):",
							choices = NULL,
							selected = "Labels"
						),
						helpText("TR samples with same Label will be colored together")
					)
				),
				
				hr(),
				
				fluidRow(
					column(
						width = 3,
						numericInput(
							ns("tsne_perplexity"),
							"t-SNE Perplexity:",
							value = 30,
							min = 5,
							max = 50
						)
					),
					column(
						width = 3,
						numericInput(
							ns("tsne_iterations"),
							"t-SNE Iterations:",
							value = 1000,
							min = 250,
							max = 5000,
							step = 250
						)
					),
					column(
						width = 3,
						checkboxInput(
							ns("show_corrected"),
							"Show Corrected Data",
							value = TRUE
						),
						helpText("Enable to compare before/after ComBat correction")
					),
					column(
						width = 3,
						actionButton(
							ns("run_analysis"),
							"Generate Visualizations",
							icon = icon("chart-line"),
							class = "btn-primary btn-lg",
							style = "margin-top: 25px;"
						)
					)
				)
			)
		),
		
		# PN Correlations ####
		
		fluidRow(
			box(
				title = "Technical Replicate Correlation Analysis",
				width = 12,
				status = "warning",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Correlation between technical replicates should be high (>0.9).  ComBat correction should improve correlation."),
				
				tabsetPanel(
					id = ns("correlation_tabs"),
					
					tabPanel(
						"Correlation Matrix",
						br(),
						fluidRow(
							column(
								width = 6,
								h4("Original Data"),
								htmlOutput(ns("correlation_stats_original")),
								plotOutput(ns("correlation_matrix_original"), height = "600px")
							),
							column(
								width = 6,
								h4("Corrected Data"),
								htmlOutput(ns("correlation_stats_corrected")),
								plotOutput(ns("correlation_matrix_corrected"), height = "600px")
							)
						)
					),
					
					tabPanel(
						"Correlation Distribution",
						br(),
						plotOutput(ns("correlation_histogram"), height = "500px")
					),
					
					tabPanel(
						"By Batch",
						br(),
						plotOutput(ns("correlation_by_batch"), height = "600px")
					),
					
					tabPanel(
						"Intra vs Inter Batch",
						br(),
						DT::dataTableOutput(ns("correlation_summary_table")),
						br(),
						plotOutput(ns("correlation_intra_inter"), height = "600px")
					)
				)
			)
		),
		
		# Sample Correlation Analysis (Non-TR) #####
		fluidRow(
			box(
				title = "Sample Correlation Analysis (Non-Technical Replicates)",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				p("Summary statistics for all pairwise correlations between non-TR samples. "),
				tabsetPanel(
					id = ns("sample_correlation_tabs"),
					
					tabPanel(
						"Summary Statistics",
							fluidRow(
								column(
									width = 6,
									h4("Original Data"),
									htmlOutput(ns("sample_correlation_stats_original"))
								),
								column(
									width = 6,
									h4("Corrected Data"),
									htmlOutput(ns("sample_correlation_stats_corrected"))
								)
							),
							
							hr(),
							
							fluidRow(
								column(
									width = 12,
									h4("Comparison Table"),
									DT::dataTableOutput(ns("sample_correlation_comparison"))
								)
							)
					),
					tabPanel(
						"Distribution",
						br(),
							fluidRow(
								column(
									width = 6,
									h4("Distribution - Original"),
									plotOutput(ns("sample_correlation_hist_original"), height = "400px")
								),
								column(
									width = 6,
									h4("Distribution - Corrected"),
									plotOutput(ns("sample_correlation_hist_corrected"), height = "400px")
								)
							)
					),
					
					tabPanel(
						"By Batch",
						br(),
						p("Mean correlation for each sample, grouped by batch.  Samples should have consistent correlations across batches."),
						shinycssloaders::withSpinner(
							plotOutput(ns("sample_correlation_by_batch"), height = "600px"),
							type = 4,
							color = "#3c8dbc"
						)
					),
					
					tabPanel(
						"Intra vs Inter Batch",
						br(),
						p("Compare correlations within batches vs between batches.  ComBat should reduce differences. "),
						DT::dataTableOutput(ns("sample_correlation_intra_inter_table")),
						br(),
						shinycssloaders::withSpinner(
							plotOutput(ns("sample_correlation_intra_inter"), height = "600px"),
							type = 4,
							color = "#3c8dbc"
						)
					)
				)
			)
		),
		
		# Dendrogram #####
		fluidRow(
			box(
				title = "Hierarchical Clustering Dendrogram",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Technical replicates should cluster together if batch effects are minimal."),
				
				tabsetPanel(selected = 'Side-by-Side',
					id = ns("dendro_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("dendrogram_original"), height = "600px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("dendrogram_corrected"), height = "600px")
					),
					tabPanel('Side-by-Side',
									 plotOutput(ns("dendrogram_original_2"), height = "300px"),
									 plotOutput(ns("dendrogram_corrected_2"), height = "300px")
									 )
				)
			)
		),
		
		# t-SNE #####
		fluidRow(
			box(
				title = "t-SNE Dimensionality Reduction",
				width = 12,
				status = "warning",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Samples should separate by biological groups, not batch factors."),
				
				tabsetPanel(selected = "Side-by-Side",
					id = ns("tsne_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("tsne_original"), height = "600px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("tsne_corrected"), height = "600px")
					),
					tabPanel(
						"Side-by-Side",
						plotOutput(ns("tsne_comparison"), height = "600px")
					)
				)
			)
		),
		
		# PCA
		fluidRow(
			box(
				title = "Principal Component Analysis (PCA)",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				tabsetPanel(
					id = ns("pca_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("pca_original"), height = "600px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("pca_corrected"), height = "600px")
					),
					tabPanel(
						"Variance Explained",
						plotOutput(ns("pca_variance"), height = "400px")
					)
				)
			)
		),
		
		# Heatmap
		fluidRow(
			box(
				title = "Sample Distance Heatmap",
				width = 12,
				status = "danger",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				tabsetPanel(
					id = ns("heatmap_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("heatmap_original"), height = "700px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("heatmap_corrected"), height = "700px")
					)
				)
			)
		)
	)
}

#' Batch Effect Visualization Module - Server
#'
#' @param id Module namespace ID
#' @param eset_original Reactive ExpressionSet (original data)
#' @param eset_corrected Reactive ExpressionSet (ComBat corrected data)
#' @param debug Enable debug mode
#' @export
mod_batch_visualization_server <- function(id,
																					 eset_original_name = reactive(NULL),
																					 eset_original,
																					 eset_corrected = reactive(NULL),
																					 sample_group_column = reactive(NULL),
																					 batch_factors = reactive(NULL),
																					 ExpSet_list = NULL,
																					 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns 
		# Render debug button
		output$debug_ui <- renderUI({
			if (debug) {
				tagList(
					actionButton(
						session$ns("debug"),
						"Debug",
						icon = icon("bug"),
						class = "btn-warning btn-sm"
					),
					hr()
				)
			}
		})
		
		output$eset_name_text = renderText({
			print(eset_original_name())
		})
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Batch Visualization Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset_original() - Original ExpressionSet")
				message("  • eset_corrected() - Corrected ExpressionSet")
				message("  • input$color_by - Coloring factor")
				message("  • input$shape_by - Shape factor")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		observe({
			req(eset_original())
			message("\n🔄 BATCH VIZ: eset_original CHANGED")
			message("  Samples: ", ncol(eset_original()))
			message("  Features: ", nrow(eset_original()))
			message("  First sample: ", colnames(eset_original())[1])
		})
		
		#Check assay compatibility #####
		
		app_corrected_name <- reactive({
			if (is.null(eset_corrected) || is.null(eset_corrected())) {
				return(NULL)
			}
			
			ExpSet <- eset_corrected()
			if (is.null(ExpSet)) {
				return(NULL)
			}
			
			notes <- Biobase::notes(ExpSet)
			notes$name
		})
		
		current_assay_name <- reactive({
			eset_original_name()
		})
		
		# ✅ Build the expected corrected assay name
		expected_corrected_assay_name <- reactive({
			req(current_assay_name())
			
			suffix <- input$corrected_suffix
			if (is.null(suffix) || suffix == "") {
				suffix <- "_ComBat"
			}
			
			paste0(current_assay_name(), suffix)
		})
		
		# ✅ Check if the expected corrected assay exists in the ExpressionSet
		corrected_assay_exists <- reactive({
			req(eset_original())
			req(expected_corrected_assay_name())
			
			ExpSet <- eset_original()
			all_assays <- Biobase::assayDataElementNames(ExpSet)
			
			expected <- expected_corrected_assay_name()
			exists <- expected %in% all_assays
			
			message("🔍 BATCH VIZ: Checking for corrected assay")
			message("  Current assay: ", current_assay_name())
			message("  Expected corrected assay: ", expected)
			message("  All available assays: ", paste(all_assays, collapse = ", "))
			message("  Exists: ", exists)
			
			exists
		})
		
		# ✅ Check if session ComBat can be used (ONLY if assay names match)
		combat_is_compatible <- reactive({
			req(current_assay_name())
			
			app_corrected <- app_corrected_name()
			current <- current_assay_name()
			
			message("🔍 BATCH VIZ: Checking ComBat compatibility")
			message("  Current assay: ", current)
			message("  App corrected assay: ", app_corrected %||% "NULL")
			
			if (is.null(app_corrected)) {
				message("  ❌ No app corrected data")
				return(FALSE)
			}
			
			# ONLY compatible if names match exactly
			compatible <- (current == app_corrected)
			message("  Compatible: ", compatible)
			
			return(compatible)
		})
		
		# ✅ NEW: Show dropdown for assay selection when in "assay" mode
		# output$corrected_assay_selector <- renderUI({
		# 	req(input$corrected_source == "assay") 
		# 	req(available_corrected_assays())
		# 	
		# 	# Default to the expected corrected assay name
		# 	default_selection <- expected_corrected_assay_name()
		# 	
		# 	selectInput(
		# 		ns("corrected_assay_name"),
		# 		"Select Corrected Assay:",
		# 		choices = available_corrected_assays(),
		# 		selected = if (default_selection %in% available_corrected_assays()) default_selection else NULL
		# 	)
		# })
		
		# Keep the renderUI simple
		# output$corrected_assay_selector <- renderUI({
		# 	req(input$corrected_source == "assay") 
		# 	req(available_corrected_assays())
		# 	
		# 	selectInput(
		# 		ns("corrected_assay_name"),
		# 		"Select Corrected Assay:",
		# 		choices = available_corrected_assays()
		# 	)
		# })
		# 
		# # ✅ Add separate observer to update the selection
		# # ✅ Separate observer to update the selection
		# observe({
		# 	req(input$corrected_source == "assay")
		# 	req(available_corrected_assays())
		# 	req(expected_corrected_assay_name())
		# 	
		# 	default_selection <- expected_corrected_assay_name()
		# 	available <- available_corrected_assays()
		# 	
		# 	message("🔍 BATCH VIZ: Updating corrected_assay_name selection")
		# 	message("  Expected: ", default_selection)
		# 	message("  Available: ", paste(available, collapse = ", "))
		# 	
		# 	# Check if the expected corrected assay exists
		# 	if (default_selection %in% available) {
		# 		message("  ✅ Selecting: ", default_selection)
		# 		updateSelectInput(session, "corrected_assay_name", selected = default_selection)
		# 	} else {
		# 		message("  ⚠️ Expected assay '", default_selection, "' not found in available assays")
		# 		message("  Keeping current selection or selecting first available")
		# 		
		# 		# Optionally, select the first available
		# 		if (length(available) > 0) {
		# 			updateSelectInput(session, "corrected_assay_name", selected = available[1])
		# 		}
		# 	}
		# })
		
		# ✅ REPLACE the dropdown with a status display
		output$corrected_assay_selector <- renderUI({
			req(input$corrected_source == "assay")
			req(expected_corrected_assay_name())
			
			expected <- expected_corrected_assay_name()
			available <- available_corrected_assays()
			exists <- expected %in% available
			
			message("🔍 BATCH VIZ: Rendering corrected assay status")
			message("  Expected: ", expected)
			message("  Exists: ", exists)
			
			if (exists) {
				div(
					class = "alert alert-info",
					style = "margin-top: 10px;",
					icon("info-circle"),
					strong(" Corrected Assay Name: "),
					tags$code(expected),
					p(style = "margin-top: 5px; margin-bottom: 0;", 
						"This assay will be loaded from the ExpressionSet.")
				)
			} else {
				div(
					class = "alert alert-warning",
					style = "margin-top: 10px;",
					icon("exclamation-triangle"),
					strong(" Corrected Assay Name: "),
					tags$code(expected),
					p(style = "margin-top: 5px; margin-bottom: 0;", 
						strong("⚠️ This assay does not exist in the ExpressionSet"))
				)
			}
		})
		
		# ✅ Dynamic corrected eset - simplified since no user selection
		eset_corrected_dynamic <- reactive({
			
			if (input$corrected_source == "session") {
				# Use ComBat from this session - ONLY IF COMPATIBLE
				message("🔍 BATCH VIZ: Attempting to use session ComBat")
				
				if (!  combat_is_compatible()) {
					message("  ❌ Not compatible - assay names don't match")
					return(NULL)
				}
				
				req(eset_corrected())
				corrected <- eset_corrected()
				
				if (is.null(corrected)) {
					message("  ❌ Corrected is NULL")
					return(NULL)
				}
				
				message("  ✅ Using session ComBat")
				return(corrected)
				
			} else if (input$corrected_source == "assay") {
				# Load from ExpressionSet assay using expected name
				message("🔍 BATCH VIZ: Loading from ExpressionSet assay")
				
				req(eset_original())
				req(expected_corrected_assay_name())
				
				ExpSet <- eset_original()
				corrected_assay_name <- expected_corrected_assay_name()
				
				message("  Looking for assay: ", corrected_assay_name)
				
				# Check if it exists
				all_assays <- Biobase::assayDataElementNames(ExpSet)
				
				if (! corrected_assay_name %in% all_assays) {
					message("  ❌ Assay '", corrected_assay_name, "' not found")
					return(NULL)
				}
				
				message("  ✅ Found assay: ", corrected_assay_name)
				
				# Create a copy with the corrected data as exprs
				corrected_ExpSet <- ExpSet
				Biobase::exprs(corrected_ExpSet) <- Biobase::assayDataElement(ExpSet, corrected_assay_name)
				
				# Add note about source
				notes <- Biobase::notes(corrected_ExpSet)
				notes$visualization_source <- list(
					assay_name = corrected_assay_name,
					base_assay_name = current_assay_name(),
					loaded_from = "ExpressionSet assayData"
				)
				Biobase::notes(corrected_ExpSet) <- notes
				
				return(corrected_ExpSet)
			}
			
			return(NULL)
		})
		
		
		# ✅ Dynamic corrected eset based on selection WITH VALIDATION
		# eset_corrected_dynamic <- reactive({
		# 	
		# 	if (input$corrected_source == "session") {
		# 		# Use ComBat from this session - ONLY IF COMPATIBLE
		# 		message("🔍 BATCH VIZ: Attempting to use session ComBat")
		# 		
		# 		if (! combat_is_compatible()) {
		# 			message("  ❌ Not compatible - assay names don't match")
		# 			return(NULL)
		# 		}
		# 		
		# 		req(eset_corrected())
		# 		corrected <- eset_corrected()
		# 		
		# 		if (is.null(corrected)) {
		# 			message("  ❌ Corrected is NULL")
		# 			return(NULL)
		# 		}
		# 		
		# 		message("  ✅ Using session ComBat")
		# 		return(corrected)
		# 		
		# 	} else if (input$corrected_source == "assay") {
		# 		# Load from ExpressionSet assay
		# 		message("🔍 BATCH VIZ: Loading from ExpressionSet assay")
		# 		
		# 		req(eset_original())
		# 		
		# 		ExpSet <- eset_original()
		# 		
		# 		# Use the selected assay name from dropdown (or expected)
		# 		if (! is.null(input$corrected_assay_name) && input$corrected_assay_name != "") {
		# 			corrected_assay_name <- input$corrected_assay_name
		# 		} else {
		# 			corrected_assay_name <- expected_corrected_assay_name()
		# 		}
		# 		
		# 		message("  Looking for assay: ", corrected_assay_name)
		# 		
		# 		# Check if it exists
		# 		all_assays <- Biobase::assayDataElementNames(ExpSet)
		# 		
		# 		if (!corrected_assay_name %in% all_assays) {
		# 			message("  ❌ Assay '", corrected_assay_name, "' not found")
		# 			return(NULL)
		# 		}
		# 		
		# 		message("  ✅ Found assay: ", corrected_assay_name)
		# 		
		# 		# Create a copy with the corrected data as exprs
		# 		corrected_ExpSet <- ExpSet
		# 		Biobase::exprs(corrected_ExpSet) <- Biobase::assayDataElement(ExpSet, corrected_assay_name)
		# 		
		# 		# Add note about source
		# 		notes <- Biobase::notes(corrected_ExpSet)
		# 		notes$visualization_source <- list(
		# 			assay_name = corrected_assay_name,
		# 			base_assay_name = current_assay_name(),
		# 			loaded_from = "ExpressionSet assayData"
		# 		)
		# 		Biobase::notes(corrected_ExpSet) <- notes
		# 		
		# 		return(corrected_ExpSet)
		# 	}
		# 	
		# 	return(NULL)
		# })
		
		# ✅ Status display with clear messaging
		# output$corrected_data_status <- renderUI({
		# 	
		# 	if (input$corrected_source == "session") {
		# 		app_corrected <- app_corrected_name()
		# 		current <- current_assay_name()
		# 		
		# 		if (is.null(app_corrected)) {
		# 			div(
		# 				class = "alert alert-warning",
		# 				icon("exclamation-triangle"),
		# 				strong(" No ComBat-corrected data available from this session"),
		# 				p("Run ComBat correction in the 'Batch Correction' tab or switch to 'Load from ExpressionSet assay' mode.")
		# 			)
		# 		} else if (!combat_is_compatible()) {
		# 			div(
		# 				class = "alert alert-danger",
		# 				icon("times-circle"),
		# 				strong(" ComBat data is for a different assay"),
		# 				tags$ul(
		# 					tags$li("ComBat was run on: ", strong(app_corrected)),
		# 					tags$li("Currently viewing: ", strong(current))
		# 				),
		# 				p(strong("Solutions:")),
		# 				tags$ul(
		# 					tags$li("Select '", app_corrected, "' in the Input Data selector above, OR"),
		# 					tags$li("Re-run ComBat correction on '", current, "', OR"),
		# 					tags$li("Switch to 'Load from ExpressionSet assay' mode below")
		# 				)
		# 			)
		# 		} else {
		# 			notes <- Biobase::notes(eset_corrected())
		# 			combat_info <- notes$combat_correction
		# 			
		# 			div(
		# 				class = "alert alert-success",
		# 				icon("check-circle"),
		# 				strong(" Using ComBat data from this session ✓"),
		# 				tags$ul(
		# 					tags$li("Assay: ", strong(app_corrected)),
		# 					tags$li("Batch factors: ", paste(combat_info$batch_factors, collapse = ", ")),
		# 					tags$li("Correction date: ", as.character(combat_info$correction_date))
		# 				)
		# 			)
		# 		}
		# 	} else if (input$corrected_source == "assay") {
		# 		corrected <- eset_corrected_dynamic()
		# 		expected <- expected_corrected_assay_name()
		# 		
		# 		if (is.null(corrected)) {
		# 			div(
		# 				class = "alert alert-danger",
		# 				icon("times-circle"),
		# 				strong(" Corrected assay not found in ExpressionSet"),
		# 				tags$ul(
		# 					tags$li("Current assay: ", strong(current_assay_name())),
		# 					tags$li("Looking for: ", strong(expected)),
		# 					tags$li("Available assays: ", paste(Biobase::assayDataElementNames(eset_original()), collapse = ", "))
		# 				),
		# 				p("Adjust the 'Corrected Assay Suffix' or select a different corrected assay from the dropdown.")
		# 			)
		# 		} else {
		# 			notes <- Biobase::notes(corrected)
		# 			source_info <- notes$visualization_source
		# 			
		# 			div(
		# 				class = "alert alert-success",
		# 				icon("check-circle"),
		# 				strong(" Using corrected data from ExpressionSet ✓"),
		# 				tags$ul(
		# 					tags$li("Base assay: ", strong(source_info$base_assay_name)),
		# 					tags$li("Corrected assay: ", strong(source_info$assay_name)),
		# 					tags$li("Dimensions: ", nrow(corrected), " features × ", ncol(corrected), " samples")
		# 				)
		# 			)
		# 		}
		# 	}
		# })
		
		output$corrected_data_status <- renderUI({
			
			if (input$corrected_source == "session") {
				# ...  your session code ...
				
			} else if (input$corrected_source == "assay") {
				expected <- expected_corrected_assay_name()
				available <- available_corrected_assays()
				
				# ✅ Check if expected exists
				if (!  expected %in% available) {
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" Expected corrected assay not found"),
						tags$ul(
							tags$li("Current assay: ", strong(current_assay_name())),
							tags$li("Expected corrected assay: ", strong(expected)),
							tags$li("Available corrected assays: ", paste(available, collapse = ", "))
						),
						p("The expected corrected assay does not exist in this ExpressionSet. "),
						p(strong("You can:")),
						tags$ul(
							tags$li("Select a different corrected assay from the dropdown above"),
							tags$li("Adjust the 'Corrected Assay Suffix'"),
							tags$li("Run ComBat correction on '", current_assay_name(), "' to create it")
						)
					)
				} else {
					# Expected exists - try to load it
					corrected <- eset_corrected_dynamic()
					
					if (is.null(corrected)) {
						div(
							class = "alert alert-danger",
							icon("times-circle"),
							strong(" Error loading corrected assay"),
							p("The assay '", expected, "' exists but could not be loaded.")
						)
					} else {
						notes <- Biobase::notes(corrected)
						source_info <- notes$visualization_source
						
						div(
							class = "alert alert-success",
							icon("check-circle"),
							strong(" Using corrected data from ExpressionSet ✓"),
							tags$ul(
								tags$li("Base assay: ", strong(source_info$base_assay_name)),
								tags$li("Corrected assay: ", strong(source_info$assay_name)),
								tags$li("Dimensions: ", nrow(corrected), " features × ", ncol(corrected), " samples")
							)
						)
					}
				}
			}
		})
		
		#####
		
		
		# 
		# 
		# 
		# # ✅ Track the current visualization assay name
		# current_assay_name <- reactive({
		# 	req(eset_original())
		# 	
		# 	# Get the assay name being displayed
		# 	# This assumes eset_original has a way to identify its assay
		# 	ExpSet <- eset_original()
		# 	
		# 	# Option 1: If you're passing it from vis_input_data
		# 	# You'll need to add eset_assay_name to the module parameters
		# 	
		# 	# Option 2: Check notes
		# 	notes <- Biobase::notes(ExpSet)
		# 	if (! is.null(notes$current_assay_name)) {
		# 		return(notes$current_assay_name)
		# 	}
		# 	
		# 	# Option 3: Fallback to first assay name
		# 	Biobase::assayDataElementNames(ExpSet)[1]
		# })
		# 
		# # ✅ NEW: Show dropdown for assay selection when in "assay" mode
		# output$corrected_assay_selector <- renderUI({
		# 	req(input$corrected_source == "assay")
		# 	req(available_corrected_assays())
		# 	
		# 	message("🔍 BATCH VIZ: Rendering corrected assay selector")
		# 	message("  Current assay: ", current_assay_name())
		# 	message("  Available corrected assays: ", paste(names(available_corrected_assays()), collapse = ", "))
		# 	
		# 	# Build the corrected assay name based on current assay
		# 	default_corrected <- paste0(current_assay_name(), input$corrected_suffix %||% "_ComBat")
		# 	
		# 	# Select it if it exists
		# 	selected_assay <- if (default_corrected %in% available_corrected_assays()) {
		# 		default_corrected
		# 	} else if (length(available_corrected_assays()) > 0) {
		# 		available_corrected_assays()[1]
		# 	} else {
		# 		NULL
		# 	}
		# 	
		# 	selectInput(
		# 		ns("corrected_assay_name"),
		# 		"Select Corrected Assay:",
		# 		choices = available_corrected_assays(),
		# 		selected = selected_assay
		# 	)
		# })
		# 
		# # ✅ IMPROVED: Check if session ComBat matches current visualization eset by ASSAY NAME
		# combat_is_compatible <- reactive({
		# 	req(eset_original())
		# 	req(current_assay_name())
		# 	
		# 	if (is.null(eset_corrected) || is.null(eset_corrected())) {
		# 		message("🔍 BATCH VIZ: No corrected eset from session")
		# 		return(FALSE)
		# 	}
		# 	
		# 	corrected <- eset_corrected()
		# 	if (is.null(corrected)) {
		# 		message("🔍 BATCH VIZ: Corrected eset is NULL")
		# 		return(FALSE)
		# 	}
		# 	
		# 	# Get the source assay name from ComBat notes
		# 	notes <- Biobase::notes(corrected)
		# 	combat_source_assay <- notes$combat_correction$source_assay_name
		# 	
		# 	current_assay <- current_assay_name()
		# 	
		# 	message("🔍 BATCH VIZ: Compatibility check")
		# 	message("  Current assay: ", current_assay)
		# 	message("  ComBat source assay: ", combat_source_assay %||% "NOT RECORDED")
		# 	
		# 	# If no source assay recorded, fall back to sample/feature check
		# 	if (is.null(combat_source_assay)) {
		# 		message("  ⚠️ No source assay name recorded, using dimension check")
		# 		original <- eset_original()
		# 		same_samples <- identical(colnames(original), colnames(corrected))
		# 		same_features <- identical(rownames(original), rownames(corrected))
		# 		compatible <- same_samples && same_features
		# 		message("  Compatible (by dimensions): ", compatible)
		# 		return(compatible)
		# 	}
		# 	
		# 	# Compare assay names
		# 	compatible <- (current_assay == combat_source_assay)
		# 	message("  Compatible (by assay name): ", compatible)
		# 	
		# 	return(compatible)
		# })
		# 
		# # ✅ IMPROVED: Dynamic corrected eset based on selection WITH PROPER VALIDATION
		# eset_corrected_dynamic <- reactive({
		# 	
		# 	if (input$corrected_source == "session") {
		# 		# Use ComBat from this session - BUT ONLY IF COMPATIBLE
		# 		message("🔍 BATCH VIZ: Using session ComBat")
		# 		
		# 		req(eset_corrected())
		# 		
		# 		if (! combat_is_compatible()) {
		# 			message("  ❌ Not compatible")
		# 			return(NULL)
		# 		}
		# 		
		# 		corrected <- eset_corrected()
		# 		
		# 		if (is.null(corrected)) {
		# 			message("  ❌ Corrected is NULL")
		# 			return(NULL)
		# 		}
		# 		
		# 		message("  ✅ Using session ComBat")
		# 		return(corrected)
		# 		
		# 	} else if (input$corrected_source == "assay") {
		# 		# Load from ExpressionSet assay
		# 		message("🔍 BATCH VIZ: Loading from assay")
		# 		
		# 		req(eset_original())
		# 		req(current_assay_name())
		# 		
		# 		ExpSet <- eset_original()
		# 		current_assay <- current_assay_name()
		# 		
		# 		# Determine corrected assay name
		# 		if (! is.null(input$corrected_assay_name) && input$corrected_assay_name != "") {
		# 			# User explicitly selected an assay
		# 			corrected_assay_name <- input$corrected_assay_name
		# 			message("  Using user-selected assay: ", corrected_assay_name)
		# 		} else {
		# 			# Build from current assay + suffix
		# 			suffix <- input$corrected_suffix %||% "_ComBat"
		# 			corrected_assay_name <- paste0(current_assay, suffix)
		# 			message("  Built assay name: ", corrected_assay_name, " (", current_assay, " + ", suffix, ")")
		# 		}
		# 		
		# 		# Check if it exists
		# 		all_assays <- Biobase::assayDataElementNames(ExpSet)
		# 		message("  Available assays: ", paste(all_assays, collapse = ", "))
		# 		
		# 		if (!corrected_assay_name %in% all_assays) {
		# 			message("  ❌ Corrected assay '", corrected_assay_name, "' not found")
		# 			return(NULL)
		# 		}
		# 		
		# 		message("  ✅ Found corrected assay: ", corrected_assay_name)
		# 		
		# 		# Create a copy with the corrected data as exprs
		# 		corrected_ExpSet <- ExpSet
		# 		Biobase::exprs(corrected_ExpSet) <- Biobase::assayDataElement(ExpSet, corrected_assay_name)
		# 		
		# 		# Add note about source
		# 		notes <- Biobase::notes(corrected_ExpSet)
		# 		notes$visualization_source <- list(
		# 			assay_name = corrected_assay_name,
		# 			base_assay_name = current_assay,
		# 			loaded_from = "ExpressionSet assayData"
		# 		)
		# 		Biobase::notes(corrected_ExpSet) <- notes
		# 		
		# 		return(corrected_ExpSet)
		# 	}
		# 	
		# 	return(NULL)
		# })
		# 
		# # ✅ IMPROVED: Status display with better messaging
		# output$corrected_data_status <- renderUI({
		# 	
		# 	if (input$corrected_source == "session") {
		# 		if (is.null(eset_corrected) || is.null(eset_corrected())) {
		# 			div(
		# 				class = "alert alert-warning",
		# 				icon("exclamation-triangle"),
		# 				strong(" No ComBat-corrected data available from this session"),
		# 				p("Run ComBat correction in the 'Batch Correction' tab or switch to 'Load from ExpressionSet assay' mode.")
		# 			)
		# 		} else if (!combat_is_compatible()) {
		# 			notes <- Biobase::notes(eset_corrected())
		# 			combat_source <- notes$combat_correction$source_assay_name %||% "unknown"
		# 			current <- current_assay_name()
		# 			
		# 			div(
		# 				class = "alert alert-danger",
		# 				icon("times-circle"),
		# 				strong(" ComBat data incompatible with selected visualization data"),
		# 				p(paste0("ComBat was run on assay: ", strong(combat_source))),
		# 				p(paste0("Currently viewing assay: ", strong(current))),
		# 				p(strong("Solutions:")),
		# 				tags$ul(
		# 					tags$li("Select the '", combat_source, "' assay in the Input Data selector above"),
		# 					tags$li("Re-run ComBat correction on the '", current, "' assay"),
		# 					tags$li("Switch to 'Load from ExpressionSet assay' mode below")
		# 				)
		# 			)
		# 		} else {
		# 			notes <- Biobase::notes(eset_corrected())
		# 			combat_info <- notes$combat_correction
		# 			
		# 			div(
		# 				class = "alert alert-success",
		# 				icon("check-circle"),
		# 				strong(" Using ComBat data from this session ✓"),
		# 				tags$ul(
		# 					tags$li("Source assay: ", strong(combat_info$source_assay_name %||% "not recorded")),
		# 					tags$li("Batch factors: ", paste(combat_info$batch_factors, collapse = ", ")),
		# 					tags$li("Correction date: ", as.character(combat_info$correction_date)),
		# 					tags$li("Dimensions: ", nrow(eset_corrected()), " features × ", ncol(eset_corrected()), " samples")
		# 				)
		# 			)
		# 		}
		# 	} else if (input$corrected_source == "assay") {
		# 		corrected <- eset_corrected_dynamic()
		# 		
		# 		if (is.null(corrected)) {
		# 			# Show what we were looking for
		# 			current <- current_assay_name()
		# 			suffix <- input$corrected_suffix %||% "_ComBat"
		# 			expected <- paste0(current, suffix)
		# 			
		# 			div(
		# 				class = "alert alert-warning",
		# 				icon("exclamation-triangle"),
		# 				strong(" Corrected assay not found"),
		# 				p("Looking for assay: ", strong(expected)),
		# 				p("Available assays: ", paste(Biobase::assayDataElementNames(eset_original()), collapse = ", ")),
		# 				p("Try adjusting the suffix or selecting a different assay from the dropdown above.")
		# 			)
		# 		} else {
		# 			notes <- Biobase::notes(corrected)
		# 			source_info <- notes$visualization_source
		# 			
		# 			div(
		# 				class = "alert alert-success",
		# 				icon("check-circle"),
		# 				strong(" Using corrected data from ExpressionSet ✓"),
		# 				tags$ul(
		# 					tags$li("Base assay: ", strong(source_info$base_assay_name)),
		# 					tags$li("Corrected assay: ", strong(source_info$assay_name)),
		# 					tags$li("Dimensions: ", nrow(corrected), " features × ", ncol(corrected), " samples")
		# 				)
		# 			)
		# 		}
		# 	}
		# })
		


		
		available_corrected_assays <- reactive({
			req(eset_original())

			message("🔍 BATCH VIZ: Updating available_corrected_assays")

			ExpSet <- eset_original()
			all_assays <- Biobase::assayDataElementNames(ExpSet)

			message("  All assays: ", paste(all_assays, collapse = ", "))

			# Filter for assays that look like ComBat-corrected
			corrected <- grep("combat|corrected|batch", all_assays, ignore.case = TRUE, value = TRUE)

			message("  Corrected assays found: ", paste(corrected, collapse = ", "))

			if (length(corrected) == 0) {
				return(c("No corrected assays found" = ""))
			}

			setNames(corrected, corrected)
		})
		

		
		# Update column choices
			
			# Update column choices (keep your existing code)
			observe({
						req(eset_original())
						
						message("🔍 BATCH VIZ: Updating column choices")
						message("  Current assay: ", current_assay_name())
						
						cols <- colnames(Biobase::pData(eset_original()))
						message("  Available columns: ", paste(cols, collapse = ", "))
				
					sample_group <- sample_group_column()
					batch_cols <- batch_factors()
					
					color_default <- if (! is.null(batch_cols) && length(batch_cols) > 0) {
						# Use first batch factor that was corrected
						batch_cols[1]
					} else if ("Batch_ID" %in% cols) {
						"Batch_ID"
					} else if ("Batch" %in% cols) {
						"Batch"
					} else {
						cols[1]
					}
					
					# Determine default for shape_by
					shape_default <- if (!is.null(sample_group) && length(sample_group) > 1) {
						# If multiple batch factors, use second for shape
						sample_group[1]
					} else if ("Sample_Group" %in% cols) {
						"Sample_Group"
					} else if ("Labels" %in% cols) {
						"Labels"
					} else {
						""
					}
					
					updateSelectInput(session, "color_by", choices = cols, selected = color_default)
					updateSelectInput(session, "shape_by", choices = c("None" = "", cols), selected = shape_default)
					
					# Auto-select QC and Labels
					qc_default <- if ("QC" %in% cols) "QC" else cols[1]
					label_default <- if ("Labels" %in% cols) "Labels" else cols[1]
					
					updateSelectInput(session, "qc_column", 
														choices = cols, 
														selected = qc_default)
					
					updateSelectInput(session, "label_column", 
														choices = cols, 
														selected = shape_default)
					# updateSelectInput(session, "replicate_column", choices = c("None" = "", cols), selected = "QC")
		})
		
		# Display what's being visualized
		output$correction_info <- renderUI({
			sample_group <- sample_group_column()
			batch_cols <- batch_factors()
			
			if (is.null(batch_cols) || length(batch_cols) == 0) {
				return(
					div(
						class = "alert alert-info",
						icon("info-circle"),
						strong(" Batch Analysis Configuration:"),
						p(
							"Sample Group: ", 
							if (! is.null(sample_group)) tags$code(sample_group) else tags$em("Not selected")
						),
						p("No batch correction applied yet.")
					)
				)
			}
			
			tagList(
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" Batch Correction Applied"),
					p(
						"Sample Group Preserved: ", 
						if (!is.null(sample_group)) tags$code(sample_group) else tags$em("None")
					),
					p(
						strong(sprintf("Corrected for %d batch factor(s):", length(batch_cols)))
					),
					tags$ul(
						style = "margin-bottom: 0;",
						lapply(batch_cols, function(x) tags$li(tags$code(x)))
					)
				),
				div(
					class = "alert alert-info",
					icon("palette"),
					strong(" Current Visualization Settings:"),
					p(
						"Color by: ", tags$code(input$color_by %||% "Not set"), " | ",
						"Shape by: ", tags$code(if (is.null(input$shape_by) || input$shape_by == "") "None" else input$shape_by)
					)
				)
			)
		})
		
		# Storage for analysis results
		analysis_results <- reactiveVal(NULL)
		
		# Run analysis
		observeEvent(input$run_analysis, {
			req(eset_original())
			req(input$color_by)
			
			showNotification("Running visualization analysis...", id = "viz_progress", duration = NULL, type = "message")
			
			tryCatch({
				results <- list()
				
				# Original data analysis
				results$original <- perform_analysis(
					eset_original(),
					perplexity = input$tsne_perplexity,
					iterations = input$tsne_iterations
				)
				
				# Corrected data analysis (if available)
				if (input$show_corrected && !is.null(eset_corrected_dynamic())) {
					results$corrected <- perform_analysis(
						eset_corrected_dynamic(),
						perplexity = input$tsne_perplexity,
						iterations = input$tsne_iterations
					)
				}
				
				analysis_results(results)
				
				removeNotification("viz_progress")
				showNotification("✅ Visualization complete!", type = "message", duration = 3)
				
			}, error = function(e) {
				removeNotification("viz_progress")
				showNotification(paste("❌ Analysis failed:", e$message), type = "error", duration = 10)
			})
		})
		
		# Helper function to perform all analyses
		perform_analysis <- function(eset, perplexity, iterations) {
			expr_data <- Biobase::exprs(eset)
			meta <- Biobase::pData(eset)
			
			# Row scale for distance calculations
			expr_scaled <- row_scale_function(expr_data)
			
			# Hierarchical clustering
			dist_mat <- dist(t(expr_scaled))
			hclust_res <- hclust(dist_mat, method = "ward.D2")
			
			# t-SNE
			set.seed(42)
			tsne_res <- Rtsne::Rtsne(
				t(expr_scaled),
				dims = 2,
				perplexity = perplexity,
				max_iter = iterations,
				check_duplicates = FALSE
			)
			
			# PCA
			pca_res <- prcomp(t(expr_scaled), scale.  = FALSE)
			
			list(
				hclust = hclust_res,
				dist = dist_mat,
				tsne = tsne_res,
				pca = pca_res,
				meta = meta,
				expr = expr_data,
				expr_scaled = expr_scaled
			)
		}
		
		# # Dendrogram - Original
		# output$dendrogram_original <- renderPlot({
		# 	req(analysis_results())
		# 	req(input$color_by)
		# 	
		# 	results <- analysis_results()$original
		# 	plot_dendrogram(
		# 		results$hclust,
		# 		results$meta,
		# 		color_by = input$color_by,
		# 		replicate_col = input$replicate_column,
		# 		title = "Original Data"
		# 	)
		# 	
		# 	results_meta_plot = results$meta %>% 
		# 		mutate(TR = ifelse( input$replicate_column == 'TR',input$color_by,input$replicate_column))
		# 	
		# 	plot_dendrogram(
		# 		results$hclust,
		# 		results_meta_plot,
		# 		color_by = "TR",
		# 		title = "Original Data"
		# 	)
		# })
		# 
		# # Dendrogram - Corrected
		# output$dendrogram_corrected <- renderPlot({
		# 	req(analysis_results())
		# 	req(analysis_results()$corrected)
		# 	req(input$color_by)
		# 	
		# 	results <- analysis_results()$corrected
		# 	plot_dendrogram(
		# 		results$hclust,
		# 		results$meta,
		# 		color_by = input$color_by,
		# 		replicate_col = input$replicate_column,
		# 		title = "ComBat Corrected Data"
		# 	)
		# })
		
		# PN Correlations ####
		# Add these outputs to the server function:
		
		# Correlation analysis for technical replicates
		tr_data <- reactive({
			req(analysis_results())
			req(input$qc_column)
			
			results <- analysis_results()$original
			meta <- results$meta
			
			# Identify TR samples
			is_tr <- grepl("TR", meta[[input$qc_column]], ignore.case = TRUE)
			print(rownames(meta[is_tr,]))
			
			if (sum(is_tr) == 0) {
				return(NULL)
			}
			
			list(
				is_tr = is_tr,
				tr_samples = rownames(meta)[is_tr],
				meta = meta
			)
		})
		
		# Calculate correlations
		correlation_results <- reactive({ 
			req(tr_data())
			req(analysis_results())
			
			tr_info <- tr_data()
			tr_samples <- tr_info$tr_samples
			
			if (length(tr_samples) < 2) {
				return(NULL)
			}
			
			# Original data correlations
			expr_orig <- analysis_results()$original$expr[, tr_samples]
			dim(expr_orig)
			cor_orig <- cor(expr_orig, method = "pearson")
			
			results <- list(
				original = list(
					expr_orig = expr_orig,
					cor_matrix = cor_orig,
					mean = mean(cor_orig[upper.tri(cor_orig)], na.rm = TRUE),
					median = median(cor_orig[upper.tri(cor_orig)], na.rm = TRUE),
					min = min(cor_orig[upper.tri(cor_orig)], na.rm = TRUE)
				)
			)
			
			# Corrected data correlations (if available)
			if (!is.null(analysis_results()$corrected)) {
				expr_corr <- analysis_results()$corrected$expr[, tr_samples]
				cor_corr <- cor(expr_corr, method = "pearson")
				
				results$corrected <- list(
					expr_corr = expr_corr,
					cor_matrix = cor_corr,
					mean = mean(cor_corr[upper.tri(cor_corr)], na.rm = TRUE),
					median = median(cor_corr[upper.tri(cor_corr)], na.rm = TRUE),
					min = min(cor_corr[upper.tri(cor_corr)], na.rm = TRUE)
				)
			}
			
			results
		})
		
		# Correlation statistics display
		output$correlation_stats_original <- renderUI({
			req(correlation_results())
			
			stats <- correlation_results()$original
			threshold <- 0.9
			
			mean_color <- if (stats$mean < threshold) "red" else "black"
			median_color <- if (stats$median < threshold) "red" else "black"
			min_color <- if (stats$min < threshold) "red" else "black"
			
			HTML(paste0(
				"<strong>Correlation Statistics:</strong><br>",
				"Mean = <span style='color: ", mean_color, ";'>", round(stats$mean, 3), "</span><br>",
				"Median = <span style='color: ", median_color, ";'>", round(stats$median, 3), "</span><br>",
				"Minimum = <span style='color: ", min_color, ";'>", round(stats$min, 3), "</span>"
			))
		})
		
		output$correlation_stats_corrected <- renderUI({
			req(correlation_results())
			req(correlation_results()$corrected)
			
			stats <- correlation_results()$corrected
			threshold <- 0.9
			
			mean_color <- if (stats$mean < threshold) "red" else "green"
			median_color <- if (stats$median < threshold) "red" else "green"
			min_color <- if (stats$min < threshold) "red" else "green"
			
			HTML(paste0(
				"<strong>Correlation Statistics:</strong><br>",
				"Mean = <span style='color: ", mean_color, ";'>", round(stats$mean, 3), "</span><br>",
				"Median = <span style='color: ", median_color, ";'>", round(stats$median, 3), "</span><br>",
				"Minimum = <span style='color: ", min_color, ";'>", round(stats$min, 3), "</span>"
			))
		})
		
		# Correlation matrix plots
		output$correlation_matrix_original <- renderPlot({
			req(correlation_results())
			
			expr_orig <- correlation_results()$original$expr_orig
			plot_correlation_matrix(expr_orig, "Original Data")
		})
		
		output$correlation_matrix_corrected <- renderPlot({
			req(correlation_results())
			req(correlation_results()$corrected)
			
			expr_corr <- correlation_results()$corrected$expr_corr
			plot_correlation_matrix(expr_corr, "ComBat Corrected Data")
		})
		
		# Correlation histogram
		output$correlation_histogram <- renderPlot({
			req(correlation_results())
			
			plot_correlation_histogram(correlation_results())
		})
		
		# Correlation by batch
		output$correlation_by_batch <- renderPlot({
			req(correlation_results())  
			req(tr_data())
			req(input$color_by)
			
			#correlation_results, meta, batch_column, tr_samples
			
			plot_correlation_by_batch(
				correlation_results = correlation_results(),
				meta = tr_data()$meta,
				batch_column = input$color_by,
				tr_samples = tr_data()$tr_samples
			)
		})
		
		# Intra vs Inter batch correlation
		output$correlation_intra_inter <- renderPlot({
			req(correlation_results())
			req(tr_data())
			req(input$color_by)
			
			plot_correlation_intra_inter(
				correlation_results(),
				tr_data()$meta,
				batch_column = input$color_by,
				tr_samples = tr_data()$tr_samples
			)
		})
		
		output$correlation_summary_table <- DT::renderDataTable({
			req(correlation_results())
			req(tr_data())
			req(input$color_by)
			
			create_correlation_summary_table(
				correlation_results(),
				tr_data()$meta,
				batch_column = input$color_by,
				tr_samples = tr_data()$tr_samples
			)
		})
		
		
		# Sample correlation analysis (non-TR samples) ####
		sample_correlation_results <- reactive({
			req(analysis_results())
			req(tr_data())
			
			tr_info <- tr_data()
			all_samples <- colnames(analysis_results()$original$expr)
			
			# Get non-TR samples
			non_tr_samples <- setdiff(all_samples, tr_info$tr_samples)
			
			if (length(non_tr_samples) < 2) {
				return(NULL)
			}
			
			# Original data correlations
			expr_orig <- analysis_results()$original$expr[, non_tr_samples]
			dim(expr_orig)
			cor_orig <- cor(expr_orig, method = "pearson")
			mean(cor_orig)
			
			# Extract upper triangle (to avoid duplicates and diagonal)
			upper_tri_orig <- cor_orig[upper.tri(cor_orig)]
			
			results <- list(
				n_samples = length(non_tr_samples),
				original = list(
					cor_matrix = cor_orig,
					mean = mean(upper_tri_orig, na.rm = TRUE),
					median = median(upper_tri_orig, na.rm = TRUE),
					min = min(upper_tri_orig, na.rm = TRUE),
					max = max(upper_tri_orig, na.rm = TRUE),
					sd = sd(upper_tri_orig, na.rm = TRUE),
					values = upper_tri_orig
				)
			)
			
			# Corrected data correlations (if available)
			if (!is.null(analysis_results()$corrected)) {
				expr_corr <- analysis_results()$corrected$expr[, non_tr_samples]
				cor_corr <- cor(expr_corr, method = "pearson")
				upper_tri_corr <- cor_corr[upper.tri(cor_corr)]
				
				results$corrected <- list(
					cor_matrix = cor_corr,
					mean = mean(upper_tri_corr, na.rm = TRUE),
					median = median(upper_tri_corr, na.rm = TRUE),
					min = min(upper_tri_corr, na.rm = TRUE),
					max = max(upper_tri_corr, na.rm = TRUE),
					sd = sd(upper_tri_corr, na.rm = TRUE),
					values = upper_tri_corr
				)
			}
			
			results
		})
		
		# Sample correlation statistics - Original
		output$sample_correlation_stats_original <- renderUI({
			req(sample_correlation_results())
			
			results <- sample_correlation_results()
			stats <- results$original
			n_samples <- results$n_samples
			n_comparisons <- length(stats$values)
			
			HTML(paste0(
				"<div style='padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>",
				"<strong>Number of Samples:</strong> ", n_samples, "<br>",
				"<strong>Number of Comparisons:</strong> ", format(n_comparisons, big.mark = ","), "<br><br>",
				"<strong>Correlation Statistics:</strong><br>",
				"<table style='margin-left: 20px;'>",
				"<tr><td>Mean:</td><td style='padding-left: 10px;'><strong>", round(stats$mean, 4), "</strong></td></tr>",
				"<tr><td>Median:</td><td style='padding-left: 10px;'><strong>", round(stats$median, 4), "</strong></td></tr>",
				"<tr><td>Minimum:</td><td style='padding-left: 10px;'><strong>", round(stats$min, 4), "</strong></td></tr>",
				"<tr><td>Maximum:</td><td style='padding-left: 10px;'><strong>", round(stats$max, 4), "</strong></td></tr>",
				"<tr><td>Std Dev:</td><td style='padding-left: 10px;'><strong>", round(stats$sd, 4), "</strong></td></tr>",
				"</table>",
				"</div>"
			))
		})
		
		# Sample correlation statistics - Corrected
		output$sample_correlation_stats_corrected <- renderUI({
			req(sample_correlation_results())
			req(sample_correlation_results()$corrected)
			
			results <- sample_correlation_results()
			stats <- results$corrected
			n_samples <- results$n_samples
			n_comparisons <- length(stats$values)
			
			# Calculate improvement
			orig_stats <- results$original
			mean_change <- stats$mean - orig_stats$mean
			median_change <- stats$median - orig_stats$median
			min_change <- stats$min - orig_stats$min
			
			mean_arrow <- if (mean_change > 0.01) "↑" else if (mean_change < -0.01) "↓" else "→"
			median_arrow <- if (median_change > 0.01) "↑" else if (median_change < -0.01) "↓" else "→"
			min_arrow <- if (min_change > 0.01) "↑" else if (min_change < -0.01) "↓" else "→"
			
			HTML(paste0(
				"<div style='padding: 15px; background-color: #d4edda; border-radius: 5px;'>",
				"<strong>Number of Samples:</strong> ", n_samples, "<br>",
				"<strong>Number of Comparisons:</strong> ", format(n_comparisons, big.mark = ","), "<br><br>",
				"<strong>Correlation Statistics:</strong><br>",
				"<table style='margin-left: 20px;'>",
				"<tr><td>Mean:</td><td style='padding-left: 10px;'><strong>", round(stats$mean, 4), 
				"</strong> <span style='color: ", if (mean_change > 0) "green" else if (mean_change < 0) "red" else "gray", ";'>", 
				mean_arrow, " ", sprintf("%+.4f", mean_change), "</span></td></tr>",
				"<tr><td>Median:</td><td style='padding-left: 10px;'><strong>", round(stats$median, 4), 
				"</strong> <span style='color: ", if (median_change > 0) "green" else if (median_change < 0) "red" else "gray", ";'>", 
				median_arrow, " ", sprintf("%+.4f", median_change), "</span></td></tr>",
				"<tr><td>Minimum:</td><td style='padding-left: 10px;'><strong>", round(stats$min, 4), 
				"</strong> <span style='color: ", if (min_change > 0) "green" else if (min_change < 0) "red" else "gray", ";'>", 
				min_arrow, " ", sprintf("%+.4f", min_change), "</span></td></tr>",
				"<tr><td>Maximum:</td><td style='padding-left: 10px;'><strong>", round(stats$max, 4), "</strong></td></tr>",
				"<tr><td>Std Dev:</td><td style='padding-left: 10px;'><strong>", round(stats$sd, 4), "</strong></td></tr>",
				"</table>",
				"</div>"
			))
		})
		
		# Comparison table
		output$sample_correlation_comparison <- DT::renderDataTable({
			req(sample_correlation_results())
			
			results <- sample_correlation_results()
			orig <- results$original
			
			comparison_df <- data.frame(
				Statistic = c("Mean", "Median", "Minimum", "Maximum", "Std Dev"),
				Original = c(orig$mean, orig$median, orig$min, orig$max, orig$sd)
			)
			
			if (! is.null(results$corrected)) {
				corr <- results$corrected
				comparison_df$Corrected <- c(corr$mean, corr$median, corr$min, corr$max, corr$sd)
				comparison_df$Change <- comparison_df$Corrected - comparison_df$Original
				comparison_df$Percent_Change <- (comparison_df$Change / comparison_df$Original) * 100
			}
			
			DT::datatable(
				comparison_df,
				options = list(
					dom = 't',
					pageLength = 10
				),
				rownames = FALSE
			) %>%
				DT::formatRound(columns = c('Original', 'Corrected', 'Change'), digits = 4) %>%
				DT::formatRound(columns = 'Percent_Change', digits = 2) %>%
				{
					if ('Change' %in% colnames(comparison_df)) {
						DT::formatStyle(
							.,
							'Change',
							backgroundColor = DT::styleInterval(c(-0.01, 0.01), c('lightcoral', 'lightyellow', 'lightgreen'))
						)
					} else {
						. 
					}
				}
		})
		
		# Histograms
		output$sample_correlation_hist_original <- renderPlot({
			req(sample_correlation_results())
			
			stats <- sample_correlation_results()$original
			
			hist(
				stats$values,
				breaks = 50,
				main = "Original Data",
				xlab = "Pearson Correlation",
				ylab = "Frequency",
				col = "coral",
				border = "white",
				xlim = c(min(stats$values, 0), 1)
			)
			abline(v = stats$mean, col = "red", lwd = 2, lty = 2)
			abline(v = stats$median, col = "blue", lwd = 2, lty = 2)
			legend("topleft", 
						 legend = c("Mean", "Median"),
						 col = c("red", "blue"),
						 lty = 2,
						 lwd = 2,
						 bty = "n")
		})
		
		output$sample_correlation_hist_corrected <- renderPlot({
			req(sample_correlation_results())
			req(sample_correlation_results()$corrected)
			
			stats <- sample_correlation_results()$corrected
			
			hist(
				stats$values,
				breaks = 50,
				main = "Corrected Data",
				xlab = "Pearson Correlation",
				ylab = "Frequency",
				col = "steelblue",
				border = "white",
				xlim = c(min(stats$values, 0), 1)
			)
			abline(v = stats$mean, col = "red", lwd = 2, lty = 2)
			abline(v = stats$median, col = "blue", lwd = 2, lty = 2)
			legend("topleft", 
						 legend = c("Mean", "Median"),
						 col = c("red", "blue"),
						 lty = 2,
						 lwd = 2,
						 bty = "n")
		})
		
		##  Sample correlation by batch ####
		output$sample_correlation_by_batch <- renderPlot({
			req(sample_correlation_results())
			req(input$color_by)
			
			results <- sample_correlation_results()
			meta <- analysis_results()$original$meta
			
			# Get non-TR samples
			tr_info <- tr_data()
			all_samples <- colnames(analysis_results()$original$expr)
			non_tr_samples <- setdiff(all_samples, tr_info$tr_samples)
			
			plot_sample_correlation_by_batch(
				results,
				meta,
				batch_column = input$color_by,
				samples = non_tr_samples
			)
		})
		
		# Sample correlation intra vs inter batch
		output$sample_correlation_intra_inter <- renderPlot({
			req(sample_correlation_results())
			req(input$color_by)
			
			results <- sample_correlation_results()
			meta <- analysis_results()$original$meta
			
			# Get non-TR samples
			tr_info <- tr_data()
			all_samples <- colnames(analysis_results()$original$expr)
			non_tr_samples <- setdiff(all_samples, tr_info$tr_samples)
			
			plot_sample_correlation_intra_inter(
				results,
				meta,
				batch_column = input$color_by,
				samples = non_tr_samples
			)
		})
		
		output$sample_correlation_intra_inter_table <- DT::renderDataTable({
			req(sample_correlation_results())
			req(input$color_by)
			
			results <- sample_correlation_results()
			meta <- analysis_results()$original$meta
			
			# Get non-TR samples
			tr_info <- tr_data()
			all_samples <- colnames(analysis_results()$original$expr)
			non_tr_samples <- setdiff(all_samples, tr_info$tr_samples)
			
			create_sample_correlation_intra_inter_table(
				results,
				meta,
				batch_column = input$color_by,
				samples = non_tr_samples
			)
		})
		
		# Dendrogram - Original ####
		output$dendrogram_original <- renderPlot({
			req(analysis_results()) 
			req(input$color_by)
			req(input$qc_column)
			
			results <- analysis_results()$original
			
			
		
			plot_dendrogram(
				hclust_obj = results$hclust,
				meta = results$meta,
				color_by = input$label_column,
				qc_column = input$qc_column,
				title = "Original Data - Hierarchical Clustering"
			)
		})
		
		# Dendrogram - Corrected ####
		output$dendrogram_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			req(input$qc_column)
			
			results <- analysis_results()$corrected
			
			plot_dendrogram(
				results$hclust,
				results$meta,
				color_by = input$label_column,
				qc_column = input$qc_column,
				title = "ComBat Corrected Data - Hierarchical Clustering"
			)
		})
		
		# Dendrogram - Original ####
		output$dendrogram_original_2 <- renderPlot({
			req(analysis_results()) 
			req(input$color_by)
			req(input$qc_column)
			
			results <- analysis_results()$original
			
			
			
			plot_dendrogram(
				hclust_obj = results$hclust,
				meta = results$meta,
				color_by = input$label_column,
				qc_column = input$qc_column,
				title = "Original Data - Hierarchical Clustering"
			)
		})
		
		# Dendrogram - Corrected ####
		output$dendrogram_corrected_2 <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			req(input$qc_column)
			
			results <- analysis_results()$corrected
			
			plot_dendrogram(
				results$hclust,
				results$meta,
				color_by = input$label_column,
				qc_column = input$qc_column,
				title = "ComBat Corrected Data - Hierarchical Clustering"
			)
		})
		
		# t-SNE #####
		# t-SNE - Original
		output$tsne_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_tsne(
				results$tsne,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "Original Data"
			)
		})
		
		# t-SNE - Corrected
		output$tsne_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_tsne(
				results$tsne,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "ComBat Corrected Data"
			)
		})
		
		# t-SNE - Comparison
		output$tsne_comparison <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			orig <- analysis_results()$original
			corr <- analysis_results()$corrected
			
			plot_tsne_comparison(
				orig$tsne, corr$tsne,
				orig$meta,
				color_by = input$color_by,
				shape_by = input$shape_by
			)
		})
		
		# PCA - Original
		output$pca_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_pca(
				results$pca,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "Original Data"
			)
		})
		
		# PCA - Corrected
		output$pca_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_pca(
				results$pca,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "ComBat Corrected Data"
			)
		})
		
		# PCA Variance
		output$pca_variance <- renderPlot({
			req(analysis_results())
			
			orig_var <- analysis_results()$original$pca$sdev^2 / sum(analysis_results()$original$pca$sdev^2)
			
			if (! is.null(analysis_results()$corrected)) {
				corr_var <- analysis_results()$corrected$pca$sdev^2 / sum(analysis_results()$corrected$pca$sdev^2)
				plot_pca_variance_comparison(orig_var, corr_var)
			} else {
				plot_pca_variance_single(orig_var)
			}
		})
		
		# Heatmap - Original
		output$heatmap_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_distance_heatmap(
				results$dist,
				results$meta,
				color_by = input$color_by,
				title = "Original Data"
			)
		})
		
		# Heatmap - Corrected
		output$heatmap_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_distance_heatmap(
				results$dist,
				results$meta,
				color_by = input$color_by,
				title = "ComBat Corrected Data"
			)
		})
	})
}