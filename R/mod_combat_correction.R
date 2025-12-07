#' ComBat Batch Correction Module - UI
#'
#' Select batch factors and run ComBat correction
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
#' ComBat Batch Correction Module - UI
#'
#' Select batch factors and run ComBat correction
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_combat_correction_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		uiOutput(ns("debug_ui")),
		# Batch Correction Settings ####
		fluidRow(
			fluidRow(
			column(12,
			box(
				title = "Batch Factor Selection",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				
				# Debug UI
				#uiOutput(ns("debug_ui")),
				
				column(4,
					uiOutput(ns("combat_selector_ui")),
					fluidRow(
						box(
							width = 12,
							collapsible = T,
							collapsed = F,
							p("Select batch factors to correct using ComBat.  "),
							p(strong("Safe factors:"), "Strong batch effect (p < 0.05) and NOT confounded with sample groups (p > 0.05).  "),
							
							# Batch factor selector
							#uiOutput(ns("combat_selector_ui")),
							
							# ✅ Multi-factor correction strategy (shows only when multiple factors selected)
							uiOutput(ns("multi_factor_options")),
							
							# ✅ Batch combination preview
							uiOutput(ns("batch_preview"))
						)
					)
			
				),
			
			column(
				width = 4,
				selectInput(
					ns("par_prior"),
					"Parametric Prior:",
					choices = c("Parametric" = TRUE, "Non-parametric" = FALSE),
					selected = TRUE
				),
				helpText("Parametric is faster and works well for most cases.  Use non-parametric for non-normal data.")
			),
			column(
				width = 4,
				radioButtons(
					ns("combat_model"),
					"ComBat Model:",
					choices = c(
						"Null Model (~1)" = "null",
						"Preserve Sample Groups" = "preserve"
					),
					selected = "null"
				),
				actionLink(
					ns("show_model_help"),
					HTML("<i class='fa fa-question-circle'></i> Which model should I use?"),
					style = "font-size: 12px;"
				)
			)))
		),
		column(12,
		tabsetPanel(
	
			## Single ####
			tabPanel('Single',
							 mod_eset_selector_standalone_ui("combat_data",T,T,T,T),
		fluidRow(
			box(
				title = "ComBat Batch Correction",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				# Debug UI
				
				
				# p("Select batch factors to correct using ComBat. "),
				# p(strong("Safe factors:"), "Strong batch effect (p < 0.05) and NOT confounded with sample groups (p > 0.05). "),
				# 
				# # Batch factor selector
				# uiOutput(ns("combat_selector_ui")),
				# 
				# # ✅ NEW: Multi-factor correction strategy (shows only when multiple factors selected)
				# uiOutput(ns("multi_factor_options")),
				# 
				# # ✅ NEW: Batch combination preview
				# uiOutput(ns("batch_preview")),
				# 
				# hr(),
				
				# ComBat Settings
				fluidRow(

					column(
						width = 4,
						br(), br(),
						actionButton(
							ns("run_combat"),
							"Run ComBat Correction",
							icon = icon("play"),
							class = "btn-success btn-lg",
							style = "width: 100%;"
						)
					)
				),
				
				br(),
				uiOutput(ns("correction_status"))
			)
		),
		
		### Model Help Modal ####
		bsModal(
			ns("model_help_modal"),
			"ComBat Model Selection Guide",
			trigger = ns("show_model_help"),
			size = "large",
			
			h4(icon("info-circle"), " Understanding ComBat Models"),
			
			hr(),
			
			h5(tags$span(style = "color: #2c3e50;", icon("calculator"), " Null Model (~1)")),
			p(strong("What it does:"), "Removes ALL variation associated with the batch factor. "),
			p(strong("Mathematical model:"), tags$code("Expression ~ 1 (intercept only)")),
			
			tags$ul(
				tags$li(strong("Pros:"),
								tags$ul(
									tags$li("Maximum batch effect removal"),
									tags$li("Simplest approach"),
									tags$li("Matches original ComBat methodology")
								)
				),
				tags$li(strong("Cons:"),
								tags$ul(
									tags$li(style = "color: #e74c3c;",
													strong("RISK:"),
													"May remove real biological differences if batch is confounded with sample groups")
								)
				),
				tags$li(strong("Use when:"),
								tags$ul(
									tags$li("Batch factors are purely technical (instrument, date, operator)"),
									tags$li("You have verified NO confounding with sample groups (Fisher's test p > 0.05)"),
									tags$li("Technical replicates are present to validate correction")
								)
				)
			),
			
			hr(),
			
			h5(tags$span(style = "color: #2c3e50;", icon("shield-alt"), " Preserve Sample Groups")),
			p(strong("What it does:"), "Removes batch effects WHILE protecting differences between your biological sample groups."),
			p(strong("Mathematical model:"), tags$code("Expression ~ Sample_Group")),
			
			tags$ul(
				tags$li(strong("Pros:"),
								tags$ul(
									tags$li("Protects biological signal from removal"),
									tags$li("Safer when some confounding exists"),
									tags$li("Explicitly preserves group differences")
								)
				),
				tags$li(strong("Cons:"),
								tags$ul(
									tags$li("May leave some batch effects if heavily confounded"),
									tags$li("Slightly more complex model")
								)
				),
				tags$li(strong("Use when:"),
								tags$ul(
									tags$li("Some confounding exists between batch and sample groups"),
									tags$li("You want to be cautious about preserving biology"),
									tags$li("Downstream analysis focuses on sample group comparisons")
								)
				)
			),
			
			hr(),
			
			div(
				class = "alert alert-info",
				h5(icon("lightbulb"), " Decision Guide"),
				tags$table(
					class = "table table-bordered",
					style = "background-color: white;",
					tags$thead(
						tags$tr(
							tags$th("Scenario"),
							tags$th("Recommended Model"),
							tags$th("Reason")
						)
					),
					tags$tbody(
						tags$tr(
							tags$td("Fisher p-value > 0.05"),
							tags$td(tags$span(style = "color: #27ae60; font-weight: bold;", "Null Model")),
							tags$td("No confounding detected - safe to use maximum correction")
						),
						tags$tr(
							tags$td("Fisher p-value < 0.05"),
							tags$td(tags$span(style = "color: #e67e22; font-weight: bold;", "Preserve Groups")),
							tags$td("Confounding present - protect biological differences")
						),
						tags$tr(
							tags$td("Technical replicates available"),
							tags$td(tags$span(style = "color: #27ae60; font-weight: bold;", "Null Model")),
							tags$td("Can validate that biology is preserved")
						),
						tags$tr(
							tags$td("Unsure about confounding"),
							tags$td(tags$span(style = "color: #3498db; font-weight: bold;", "Preserve Groups")),
							tags$td("Safer default - won't remove real biology")
						)
					)
				)
			),
			
			div(
				class = "alert alert-warning",
				h5(icon("exclamation-triangle"), " Important Notes"),
				tags$ul(
					tags$li("Always check the 'Combined Batch Analysis' BEFORE running ComBat"),
					tags$li("Only correct factors that are in the", tags$strong("'Safe to Correct'"), "category"),
					tags$li("Visualize results with dendrograms and t-SNE to verify correction quality"),
					tags$li("When in doubt, use 'Preserve Sample Groups' - it's the safer option")
				)
			)
		),
		
		### Correction Results ####
		fluidRow(
			box(
				title = "Correction Results",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				verbatimTextOutput(ns("correction_summary"))
			)
		),
		
		
	),
	
	## MULTIPLE #####
	tabPanel('Multiple',
					 # In mod_combat_correction_ui
					 fluidRow(
					 	box(
					 		title = "Batch Correction for Multiple Assays",
					 		width = 12,
					 		status = "info",
					 		solidHeader = TRUE,
					 		collapsible = TRUE,
					 		collapsed = F,
					 		
					 		p("Apply the selected batch correction settings to multiple assays at once."),
					 		
					 		fluidRow(
					 			column(
					 				width = 6,
					 				pickerInput(
					 					ns("target_assays"),
					 					"Target Assays:",
					 					choices = NULL,
					 					selected = NULL,
					 					multiple = TRUE,
					 					options = pickerOptions(
					 						actionsBox = TRUE,
					 						selectedTextFormat = "count > 2",
					 						liveSearch = TRUE,
					 						title = "Select one or more assays to correct"
					 					)
					 				),
					 				helpText("Select which assays to apply batch correction to.  You can select multiple.")
					 			),
					 			column(
					 				width = 6,
					 				textInput(
					 					ns("combat_assay_suffix"),
					 					"Corrected Assay Suffix:",
					 					value = "_ComBat",
					 					placeholder = "_ComBat"
					 				),
					 				helpText("Corrected assays will be added with this suffix")
					 			)
					 		),
					 		
					 		fluidRow(
					 			column(
					 				width = 6,
					 				h5("Selected Assays:"),
					 				htmlOutput(ns("target_assays_text"))
					 			),
					 			column(
					 				width = 6,
					 				h5("Will Create:"),
					 				htmlOutput(ns("target_assays_combat_text"))
					 			),
					 			column(
					 				width = 12,
					 				uiOutput(ns('batch_correct_column_ui'))
					 			)
					 		),
					 		
					 		hr(),
					 		### run ####
					 		actionButton(
					 			ns("apply_multi_batch_correction"),
					 			"Apply Batch Correction to All Selected Assays",
					 			icon = icon("magic"),
					 			class = "btn-success btn-lg",
					 			style = "width: 100%;"
					 		),
					 		
					 		br(),
					 		
					 		uiOutput(ns("multi_correction_status"))
					 	)
					 )

		),
	## Batch Test #####
	tabPanel('Batch Test',
					 mod_batch_combined_analysis_ui("batch_analysis", debug = run_debug)
	)
		)
		)
		)
	)
	
}
# UI END ####

#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param sample_group_column Reactive returning sample group column name
#' @param combined_results Reactive data frame from combined batch analysis
#' @param ExpSet_list Reactive returning the full ExpressionSet list (optional)
#' @param selected_expset_name Reactive returning the name of selected ExpSet (optional)
#' @param update_expset_list Function to update the ExpSet_list (optional)
#' @param debug Enable debug mode (default FALSE)
#' @export
mod_combat_correction_server <- function(id,
																				 eset,
																				 sample_group_column,
																				 
																				 ExpSet_list = reactive(NULL),  # ✅ Add ExpSet_list
																				 update_ExpSet_list = NULL,     # ✅ Add update function
																			
																				 
																				 combined_results = reactive(NULL),
																				 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns 
		
		# Debug UI
		output$debug_ui <- renderUI({
			if (isTRUE(debug)) {
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
		
		# Debug observer
		if (isTRUE(debug)) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - ComBat Correction Module")
				message("═══════════════════════════════════════════════")
				browser()
			})
		}
		

		
		safe_batch_factors <- reactive({
			# ✅ Don't require - just check if it exists
			if (is.null(combined_results) || is.function(combined_results) == FALSE) {
				return(character(0))
			}
			
			df <- tryCatch({
				combined_results()
			}, error = function(e) {
				message("Could not get combined_results: ", e$message)
				return(NULL)
			})
			
			if (is.null(df) || nrow(df) == 0) {
				return(character(0))
			}
			
			# Dynamically get the batch effect column name
			batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
			
			if (is.na(batch_col_name)) {
				warning("Could not find batch effect p-value column")
				return(character(0))
			}
			
			# Filter for: Batch p < 0.05 (batch effect) AND Fisher p > 0.05 (not confounded)
			safe <- df %>%
				rename(batch_p = !!sym(batch_col_name)) %>%
				filter(batch_p < 0.05, Fisher_p_value > 0.05) %>%
				arrange(batch_p) %>%
				pull(Batch_Column)
			
			as.character(safe)
		})
		
		# Render batch factor selector UI ####
		output$combat_selector_ui <- renderUI({
			ns <- session$ns
			req(eset())
			
			# Get all available pData columns
			all_columns <- colnames(Biobase::pData(eset()))
			
			# Get safe factors (pre-selected)
			safe_factors <- safe_batch_factors()
			
			# Determine default selection
			default_selection <- if (! is.null(safe_factors) && length(safe_factors) > 0) {
				safe_factors  # Pre-select safe factors
			} else {
				NULL  # No pre-selection
			}
			
			tagList(
				# Batch factor selector (supports multiple)
				selectInput(
					ns("batch_factors"),
					"Select Batch Factor(s) to Correct:",
					choices = all_columns,
					selected = default_selection,
					multiple = TRUE
				),
				# Show which are safe vs unsafe
				uiOutput(ns("factor_safety_info")),
				fluidRow(
					box(
						width = 12,
						collapsible = T,
						collapsed = T,
						if (! is.null(safe_factors) && length(safe_factors) > 0) {
							div(
								class = "alert alert-success",
								icon("check-circle"),
								strong(sprintf(" %d safe batch factor(s) identified and pre-selected", length(safe_factors))),
								p("These factors have significant batch effects and are NOT confounded with sample groups.")
							)
						} else {
							div(
								class = "alert alert-info",
								icon("info-circle"),
								strong(" No safe batch factors identified"),
								p("You can still select factors manually, but be cautious of confounding with biological groups.")
							)
						},
						
						
						
						
						
						helpText(
							icon("lightbulb"),
							strong("Multiple factors:"),
							"ComBat can correct for multiple batch factors simultaneously.",
							br(),
							strong("Order matters:"),
							"Factors are corrected in the order selected (most important first)."
						)
					)
				)
				# Status message
	
			)
		})
		
		# Add safety information display ####
		output$factor_safety_info <- renderUI({
			req(input$batch_factors)
			
			selected <- input$batch_factors
			safe <- safe_batch_factors()
			
			if (length(selected) == 0) return(NULL)
			
			# Categorize selected factors
			safe_selected <- intersect(selected, safe)
			unsafe_selected <- setdiff(selected, safe)
			
			tagList(
				if (length(safe_selected) > 0) {
					div(
						style = "background-color: #d4edda; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
						strong(icon("check-circle", class = "text-success"), " Safe factors:"),
						tags$ul(
							lapply(safe_selected, function(x) tags$li(x))
						)
					)
				},
				
				if (length(unsafe_selected) > 0) {
					div(
						style = "background-color: #fff3cd; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
						strong(icon("exclamation-triangle", class = "text-warning"), " Caution - Unverified factors:"),
						tags$ul(
							lapply(unsafe_selected, function(x) tags$li(x))
						),
						p(
							style = "margin-bottom: 0;",
							em("These factors were not identified as 'safe'.   They may be confounded with sample groups or have no significant batch effect.")
						)
					)
				}
			)
		})
		
		# Store corrected ExpressionSet
		corrected_eset <- reactiveVal(NULL)
		
		# Auto-generate suggested name for ComBat assay ####
		observe({
			req(input$batch_factors)
			req(eset())
			
			# Get current assay name
			current_assay <- Biobase::assayDataElementNames(eset())[1]
			
			# Generate suggested name
			suggested_name <- paste0(current_assay, "_combat")
			
			updateTextInput(session, "combat_assay_name", value = suggested_name)
		})
		
		# Reactive for selected batch factors
		selected_batch_factors <- reactive({
			input$batch_factors
		})
		
		plot_batch_factors = reactive({
			c(selected_batch_factors(),'ComBat')
		})
		
		all_columns = reactive({
			colnames(Biobase::pData(eset()))
		})
		
		# ✅ NEW: Show multi-factor options only when multiple factors are selected
		output$multi_factor_options <- renderUI({
			req(input$batch_factors)
			
			if (length(input$batch_factors) <= 1) {
				return(NULL)
			}
			
			fluidRow(
				box(
					title = "Multi-Factor Correction Strategy",
					width = 12,
					status = "warning",
					solidHeader = TRUE,
					
					radioButtons(
						ns("correction_strategy"),
						"How should multiple batch factors be handled?",
						choices = c(
							"Combined (Recommended) - Treat factor combinations as unique batches" = "combined",
							"Sequential - Correct factors one at a time" = "sequential"
						),
						selected = "combined"
					),
					
					helpText(
						icon("info-circle"),
						strong("Combined:"),
						"Creates unique batches for each combination (e.g., Batch1_Date1, Batch1_Date2).  ",
						"More statistically rigorous and accounts for interaction effects."
					),
					
					helpText(
						icon("info-circle"),
						strong("Sequential:"),
						"Corrects for first factor, then second, etc. ",
						"Order matters - results depend on correction sequence."
					),
					
					# Show order selection for sequential
					conditionalPanel(
						condition = "input.correction_strategy == 'sequential'",
						ns = ns,
						helpText(
							icon("exclamation-triangle"),
							strong("Note:"),
							"Factors will be corrected in the order shown above. ",
							"The first factor receives the strongest correction."
						)
					)
				)
			)
		})
		
		# ✅ NEW: Batch combination preview with validation
		output$batch_preview <- renderUI({
			req(input$batch_factors)
			req(eset())
			
			if (length(input$batch_factors) <= 1) {
				return(NULL)
			}
			
			# Get strategy (default to combined if not set yet)
			strategy <- input$correction_strategy
			if (is.null(strategy)) {
				strategy <- "combined"
			}
			
			ExpSet <- eset()
			meta <- Biobase::pData(ExpSet)
			batch_factors <- input$batch_factors
			
			# Validate factors exist
			missing <- setdiff(batch_factors, colnames(meta))
			if (length(missing) > 0) {
				return(
					box(
						title = "Batch Preview",
						width = 12,
						status = "danger",
						solidHeader = TRUE,
						div(
							class = "alert alert-danger",
							icon("times-circle"),
							strong(" Error: "),
							"Batch factors not found in metadata: ",
							paste(missing, collapse = ", ")
						)
					)
				)
			}
			
			if (strategy == "combined") {
				# ✅ COMBINED STRATEGY PREVIEW
				
				# Create combined batch variable
				batch_data <- lapply(batch_factors, function(f) as.factor(meta[[f]]))
				combined_batch <- do.call(interaction, c(batch_data, list(drop = TRUE, sep = "_")))
				
				# Analyze batch sizes
				batch_table <- table(combined_batch)
				n_combinations <- length(batch_table)
				batch_sizes <- as.data.frame(batch_table)
				colnames(batch_sizes) <- c("Batch_Combination", "N_Samples")
				batch_sizes <- batch_sizes[order(batch_sizes$N_Samples), ]
				
				# Identify issues
				small_batches <- batch_sizes$Batch_Combination[batch_sizes$N_Samples < 3]
				single_sample_batches <- batch_sizes$Batch_Combination[batch_sizes$N_Samples == 1]
				
				# Determine status
				if (length(single_sample_batches) > 0) {
					status <- "danger"
					icon_symbol <- "times-circle"
					status_text <- "Critical Issue"
					status_class <- "alert-danger"
				} else if (length(small_batches) > 0) {
					status <- "warning"
					icon_symbol <- "exclamation-triangle"
					status_text <- "Warning"
					status_class <- "alert-warning"
				} else {
					status <- "success"
					icon_symbol <- "check-circle"
					status_text <- "All Clear"
					status_class <- "alert-success"
				}
				
				box(
					title = "Batch Combination Preview",
					width = 12,
					status = status,
					solidHeader = TRUE,
					collapsible = TRUE,
					
					div(
						class = paste("alert", status_class),
						icon(icon_symbol),
						strong(paste0(" ", status_text)),
						tags$ul(
							tags$li(strong("Total unique batch combinations: "), n_combinations),
							tags$li(strong("Factors combined: "), paste(batch_factors, collapse = " × ")),
							tags$li(strong("Sample size range: "), 
											min(batch_sizes$N_Samples), " - ", max(batch_sizes$N_Samples))
						)
					),
					
					# Show issues if any
					if (length(single_sample_batches) > 0) {
						div(
							class = "alert alert-danger",
							icon("ban"),
							strong(" CRITICAL: ", length(single_sample_batches), " batch combination(s) have only 1 sample"),
							p("ComBat cannot correct batches with only 1 sample.  These combinations:"),
							tags$ul(
								lapply(head(single_sample_batches, 10), function(x) tags$li(as.character(x)))
							),
							if (length(single_sample_batches) > 10) {
								p(em("... and ", length(single_sample_batches) - 10, " more"))
							},
							p(strong("Recommendation: "), "Use sequential correction or exclude problematic factors")
						)
					},
					
					if (length(small_batches) > 0 && length(single_sample_batches) == 0) {
						div(
							class = "alert alert-warning",
							icon("exclamation-triangle"),
							strong(" Warning: ", length(small_batches), " batch combination(s) have < 3 samples"),
							p("Small batches may lead to unstable correction. These combinations:"),
							tags$ul(
								lapply(head(small_batches, 10), function(x) {
									n <- batch_sizes$N_Samples[batch_sizes$Batch_Combination == x]
									tags$li(as.character(x), " (n=", n, ")")
								})
							),
							if (length(small_batches) > 10) {
								p(em("... and ", length(small_batches) - 10, " more"))
							},
							p(strong("Recommendation: "), "Proceed with caution or use sequential correction")
						)
					},
					
					if (status == "success") {
						div(
							class = "alert alert-success",
							icon("check-circle"),
							strong(" All batch combinations have adequate sample sizes (≥ 3)")
						)
					},
					
					# Show table of batch sizes
					hr(),
					h5("Batch Combination Sizes:"),
					div(
						style = "max-height: 300px; overflow-y: auto;",
						renderTable({
							head(batch_sizes, 20)
						})
					),
					if (nrow(batch_sizes) > 20) {
						p(em("Showing first 20 of ", nrow(batch_sizes), " combinations"))
					}
				)
				
			} else if (strategy == "sequential") {
				# ✅ SEQUENTIAL STRATEGY PREVIEW
				
				# Analyze each factor individually
				factor_info <- lapply(batch_factors, function(f) {
					vals <- meta[[f]]
					batch_sizes <- table(vals)
					
					list(
						factor = f,
						n_groups = length(unique(vals)),
						min_size = min(batch_sizes),
						max_size = max(batch_sizes),
						sizes = batch_sizes
					)
				})
				
				box(
					title = "Sequential Correction Preview",
					width = 12,
					status = "info",
					solidHeader = TRUE,
					collapsible = TRUE,
					
					div(
						class = "alert alert-info",
						icon("info-circle"),
						strong(" Sequential correction will process factors in this order:")
					),
					
					lapply(seq_along(factor_info), function(i) {
						info <- factor_info[[i]]
						
						tagList(
							h5(paste0(i, ".  ", info$factor)),
							tags$ul(
								tags$li(strong("Number of groups: "), info$n_groups),
								tags$li(strong("Sample size range: "), info$min_size, " - ", info$max_size)
							),
							if (info$min_size == 1) {
								div(
									class = "alert alert-danger",
									icon("ban"),
									strong(" CRITICAL: This factor has group(s) with only 1 sample")
								)
							} else if (info$min_size < 3) {
								div(
									class = "alert alert-warning",
									icon("exclamation-triangle"),
									strong(" Warning: This factor has group(s) with < 3 samples")
								)
							},
							hr()
						)
					}),
					
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" Note: "),
						"Order matters!  The first factor will receive the strongest correction.  ",
						"Consider which batch effect is most important to remove."
					)
				)
			}
		})
		
		
		# ✅ NEW: Create preview ComBat column when multiple factors selected
		eset_with_combat_preview <- reactive({
			req(eset())
			req(input$batch_factors)
			
			ExpSet <- eset()
			meta <- Biobase::pData(ExpSet)
			batch_factors <- input$batch_factors
			
			# Only create ComBat column if multiple factors selected
			if (length(batch_factors) <= 1) {
				return(ExpSet)
			}
			
			# Get strategy (default to combined)
			strategy <- input$correction_strategy
			if (is.null(strategy)) {
				strategy <- "combined"
			}
			
			# Validate factors exist
			missing <- setdiff(batch_factors, colnames(meta))
			if (length(missing) > 0) {
				warning("Batch factors not found: ", paste(missing, collapse = ", "))
				return(ExpSet)
			}
			
			# Create ComBat column based on strategy
			if (strategy == "combined") {
				# Create interaction of all factors
				batch_data <- lapply(batch_factors, function(f) as.factor(meta[[f]]))
				combat_column <- do.call(interaction, c(batch_data, list(drop = TRUE, sep = "_")))
				meta$ComBat <- as.character(combat_column)
				
				message("📊 Created preview 'ComBat' column (COMBINED): ", 
								length(unique(meta$ComBat)), " unique combinations")
				
			} else if (strategy == "sequential") {
				# Concatenate all factors (same end result for preview)
				batch_values <- lapply(batch_factors, function(f) as.character(meta[[f]]))
				meta$ComBat <- do.call(paste, c(batch_values, list(sep = "_")))
				
				message("📊 Created preview 'ComBat' column (SEQUENTIAL): ", 
								length(unique(meta$ComBat)), " unique combinations")
			}
			
			# Update phenoData
			Biobase::pData(ExpSet) <- meta
			
			return(ExpSet)
		})
		
		# Run ComBat correction ####
		
		
	
		observeEvent(input$run_combat, {
			req(eset()) 
			req(input$batch_factors)
			req(sample_group_column())
			
			batch_factors <- input$batch_factors
			combat_model <- input$combat_model
			
			if (length(batch_factors) == 0) {
				showNotification("⚠️ Please select at least one batch factor", type = "warning", duration = 5)
				return()
			}
			
			# Validation warning for confounded factors with null model
			if (combat_model == "null") {
				# Check if any selected factors are confounded
				if (! is.null(combined_results())) {
					df <- combined_results()
					batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
					
					unsafe_factors <- df %>%
						rename(batch_p = !! sym(batch_col_name)) %>%
						filter(Batch_Column %in% batch_factors, Fisher_p_value < 0.05) %>%
						pull(Batch_Column)
					
					if (length(unsafe_factors) > 0) {
						showModal(
							modalDialog(
								title = tags$span(icon("exclamation-triangle"), " Warning: Confounding Detected"),
								size = "l",
								
								p(strong("The following batch factors are confounded with sample groups:")),
								tags$ul(
									lapply(unsafe_factors, function(x) tags$li(x))
								),
								
								p(style = "color: #e74c3c;",
									strong("Using the Null Model may remove real biological differences!")),
								
								p("Recommendations:"),
								tags$ol(
									tags$li("Switch to 'Preserve Sample Groups' model (safer), OR"),
									tags$li("Only correct factors with Fisher p > 0.05, OR"),
									tags$li("Proceed with caution if you understand the risks")
								),
								
								footer = tagList(
									actionButton(session$ns("cancel_combat"), "Cancel", class = "btn-default"),
									actionButton(session$ns("proceed_combat"), "Proceed Anyway", class = "btn-danger")
								)
							)
						)
						return()
					}
				}
			}
			
			# If validation passed, run ComBat
			run_combat_correction()
		})
		
		# Handle proceed button from warning modal
		observeEvent(input$proceed_combat, {
			removeModal()
			run_combat_correction()
		})
		
		# Handle cancel button
		observeEvent(input$cancel_combat, {
			removeModal()
		})
		
		
		run_combat_correction <- function() {
			showNotification("Running ComBat correction...", type = "message", duration = NULL, id = "combat_progress")
			
			tryCatch({
				ExpSet <- eset()
				batch_factors <- input$batch_factors
				combat_model <- input$combat_model
				sample_group <- sample_group_column()
				par_prior <- as.logical(input$par_prior)
				
				# Get strategy (default to combined for single factor)
				strategy <- if (length(batch_factors) > 1 && ! is.null(input$correction_strategy)) {
					input$correction_strategy
				} else {
					"single"
				}
				
				# Get data
				expr_data <- Biobase::exprs(ExpSet)
				meta <- Biobase::pData(ExpSet)
				
				# Validate
				missing_factors <- setdiff(batch_factors, colnames(meta))
				if (length(missing_factors) > 0) {
					stop("Batch factors not found in metadata: ", paste(missing_factors, collapse = ", "))
				}
				
				if (! sample_group %in% colnames(meta)) {
					stop("Sample group column not found in metadata")
				}
				
				# Create model matrix
				if (combat_model == "null") {
					modcombat <- model.matrix(~1, data = meta)
					model_description <- "Null model (~1)"
					message("Using NULL model for ComBat")
				} else {
					modcombat <- model.matrix(~ as.factor(meta[[sample_group]]))
					model_description <- paste0("Preserve model (~", sample_group, ")")
					message("Using PRESERVE model for ComBat with sample group: ", sample_group)
				}
				
				combat_column_value <- NULL
				
				# Apply correction based on strategy
				if (strategy == "combined") {
					# ✅ COMBINED STRATEGY
					message("Using COMBINED strategy for ", length(batch_factors), " factors")
					
					batch_data <- lapply(batch_factors, function(f) as.factor(meta[[f]]))
					combined_batch <- do.call(interaction, c(batch_data, list(drop = TRUE, sep = "_")))
					
					combat_column_value <- as.character(combined_batch)
					
					batch_table <- table(combined_batch)
					message("Created ", length(batch_table), " unique batch combinations")
					
					# Check for single-sample batches
					single_sample <- names(batch_table)[batch_table == 1]
					if (length(single_sample) > 0) {
						stop("Cannot use combined strategy: ", length(single_sample), 
								 " batch combination(s) have only 1 sample.  Use sequential strategy instead.")
					}
					
					corrected_data <- sva::ComBat(
						dat = expr_data,
						batch = combined_batch,
						mod = modcombat,
						par.prior = par_prior
					)
					
					correction_log <- list(
						method = "combined",
						batch_factors = batch_factors,
						n_combinations = length(batch_table),
						batch_sizes = as.list(batch_table),
						model = model_description
					)
					
				} else if (strategy == "sequential") {
					# ✅ SEQUENTIAL STRATEGY
					message("Using SEQUENTIAL strategy for ", length(batch_factors), " factors")
					
					batch_values <- lapply(batch_factors, function(f) as.character(meta[[f]]))
					combat_column_value <- do.call(paste, c(batch_values, list(sep = "_")))
					
					corrected_data <- expr_data
					correction_log <- list(
						method = "sequential",
						batch_factors = batch_factors,
						corrections = list()
					)
					
					for (i in seq_along(batch_factors)) {
						batch_factor <- batch_factors[i]
						message(sprintf("ComBat correction %d/%d: %s", i, length(batch_factors), batch_factor))
						
						batch <- meta[[batch_factor]]
						
						corrected_data <- sva::ComBat(
							dat = corrected_data,
							batch = batch,
							mod = modcombat,
							par.prior = par_prior
						)
						
						correction_log$corrections[[batch_factor]] <- list(
							order = i,
							n_groups = length(unique(batch))
						)
					}
					
					correction_log$model <- model_description
					
				} else {
					# ✅ SINGLE FACTOR
					batch_factor <- batch_factors[1]
					message("Single factor correction: ", batch_factor)
					
					combat_column_value <- as.character(meta[[batch_factor]])
					
					corrected_data <- sva::ComBat(
						dat = expr_data,
						batch = meta[[batch_factor]],
						mod = modcombat,
						par.prior = par_prior
					)
					
					correction_log <- list(
						method = "single",
						batch_factor = batch_factor,
						n_groups = length(unique(meta[[batch_factor]])),
						model = model_description
					)
				}
				
				if (! is.null(combat_column_value)) {
					meta$ComBat <- combat_column_value
					message("✅ Added 'ComBat' column to corrected ExpressionSet metadata")
				}
				
				# Create corrected ExpressionSet
				corrected_ExpSet <- ExpSet
				Biobase::exprs(corrected_ExpSet) <- corrected_data
				
				Biobase::pData(corrected_ExpSet) <- meta
				
				# Add notes
				notes <- Biobase::notes(corrected_ExpSet)
				notes$combat_correction <- c(correction_log, list(
					sample_group_preserved = if (combat_model == "preserve") sample_group else NA,
					par_prior = par_prior,
					correction_date = Sys.time()
				))
				
				notes$combat_correction$source_eset_name <- tryCatch({
					eset_metadata <- Biobase::notes(eset())
					if (! is.null(eset_metadata$name)) {
						eset_metadata$name
					} else {
						"unknown"
					}
				}, error = function(e) "unknown")
				
				notes$name <- notes$combat_correction$source_eset_name
				
				Biobase::notes(corrected_ExpSet) <- notes
				
				# Store result
				corrected_eset(corrected_ExpSet)
				
				removeNotification("combat_progress")
				
				# Success message
				success_msg <- switch(strategy,
															"combined" = sprintf("✅ ComBat correction complete!\nStrategy: Combined\nFactors: %s\nUnique batches: %d",
																									 paste(batch_factors, collapse = " × "),
																									 correction_log$n_combinations),
															"sequential" = sprintf("✅ ComBat correction complete!\nStrategy: Sequential\nOrder: %s",
																										 paste(batch_factors, collapse = " → ")),
															sprintf("✅ ComBat correction complete!\nFactor: %s", batch_factors[1])
				)
				
				showNotification(success_msg, type = "message", duration = 10)
				
			}, error = function(e) {
				removeNotification("combat_progress")
				showNotification(paste("❌ ComBat failed:", e$message), type = "error", duration = 15)
				message("ComBat error: ", e$message)
			})
		}
		
		# Correction status ####
		output$correction_status <- renderUI({
			if (! is.null(corrected_eset())) {
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" Correction complete!  "),
					p("Corrected ExpressionSet available for download.")
				)
			}
		})
		
		# Correction summary ####
		output$correction_summary <- renderPrint({
			req(corrected_eset())
			
			ExpSet_orig <- eset()
			ExpSet_corrected <- corrected_eset()
			
			notes <- Biobase::notes(ExpSet_corrected)
			combat_info <- notes$combat_correction
			
			cat("═══════════════════════════════════════════════\n")
			cat("COMBAT CORRECTION SUMMARY\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			if (! is.null(combat_info)) {
				cat("Model used:", combat_info$model, "\n")
				
				if (! is.na(combat_info$sample_group_preserved)) {
					cat("Protected group:", combat_info$sample_group_preserved, "\n")
				}
				
				cat("\nCorrected for", length(combat_info$batch_factors), "batch factor(s):\n")
				for (i in seq_along(combat_info$batch_factors)) {
					cat(sprintf("  %d.%s\n", i, combat_info$batch_factors[i]))
				}
				cat("\n")
				
				cat("Parametric prior:", combat_info$par_prior, "\n")
				cat("Row centered:", combat_info$row_centered, "\n\n")
			}
			
			cat("Original data range:",
					round(min(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "to",
					round(max(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "\n")
			
			cat("Corrected data range:",
					round(min(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "to",
					round(max(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "\n\n")
			
			cat("Dimensions:", nrow(ExpSet_corrected), "features ×", ncol(ExpSet_corrected), "samples\n")
			
			cat("\n═══════════════════════════════════════════════\n")
		})
		
	
		# MULTI #####
		
		# ✅ Display selected batch factors in Multiple tab
		output$batch_factors_display <- renderUI({
			if (is.null(input$batch_factors) || length(input$batch_factors) == 0) {
				div(
					class = "alert alert-warning",
					icon("exclamation-triangle"),
					strong(" No batch factors selected"),
					p("Please select batch factors above to proceed.")
				)
			} else {
				div(
					class = "alert alert-info",
					icon("check-circle"),
					strong(" Will correct for these batch factors:"),
					tags$ul(
						lapply(input$batch_factors, function(x) tags$li(x))
					),
					if (length(input$batch_factors) > 1) {
						p(strong("Strategy: "), 
							if (! is.null(input$correction_strategy)) {
								toupper(input$correction_strategy)
							} else {
								"COMBINED (default)"
							})
					}
				)
			}
		})
		
		observe({
			req(ExpSet_list())
			
			choices <- get_expset_assay_names(ExpSet_list())
			selected = c('sample_loess_normalised','clinical_loess_normalised','clinical_loess_normalised_PN')
			
			updatePickerInput(session, "target_assays", choices = choices,
												selected = selected)
			updateSelectInput(session, "export_individual_expset", choices = names(choices))
		})
		
		output$target_assays_text = renderText({
			paste(input$target_assays,collapse = "<br>")
		})
		
		output$target_assays_ComBat_text = renderText({
			paste(paste0(input$target_assays,'_ComBat'),collapse = "<br>")
		})
		
		
		# # ✅ Auto-populate target_assays choices from ExpSet_list
		# observe({
		# 	req(ExpSet_list())
		# 	
		# 	choices <- get_expset_assay_names(ExpSet_list())
		# 	
		# 	updatePickerInput(
		# 		session, 
		# 		"target_assays", 
		# 		choices = choices,
		# 		selected = NULL  # Or set default selections
		# 	)
		# })
		
		# ✅ Display selected assays
		output$target_assays_text <- renderText({
			req(input$target_assays)
			paste(input$target_assays, collapse = "<br>")
		})
		
		# ✅ Display corrected assay names
		output$target_assays_combat_text <- renderText({
			req(input$target_assays)
			req(input$combat_assay_suffix)
			
			paste(paste0(input$target_assays, input$combat_assay_suffix), collapse = "<br>")
		})
		
		
				
				
				# ✅ NEW: Apply batch correction to multiple assays
				observeEvent(input$apply_multi_batch_correction, { 
					req(input$target_assays) 
					req(input$batch_factors)
					req(input$combat_assay_suffix)
					req(ExpSet_list())
					
					showNotification(
						"Starting batch correction for multiple assays...", 
						type = "message", 
						duration = NULL, 
						id = "batch_multi_progress"
					)
					
					tryCatch({
						expset_list <- ExpSet_list()
						batch_factors <- input$batch_factors
						combat_model <- input$combat_model
						sample_group <- sample_group_column()
						par_prior <- as.logical(input$par_prior)
						suffix <- input$combat_assay_suffix
						
						# Get strategy
						strategy <- if (length(batch_factors) > 1 && ! is.null(input$correction_strategy)) {
							input$correction_strategy
						} else {
							"single"
						}
						
						# Track results
						corrected_count <- 0
						failed_assays <- character()
						updated_expsets <- list()
						
						# Process each target assay
						i = 1
						for (i in seq_along(input$target_assays)) {
							(assay_full_name <- input$target_assays[i])
							print(assay_full_name)
							
							message("\n========================================")
							message(sprintf("Processing %d/%d: %s", i, length(input$target_assays), assay_full_name))
							
							showNotification(
								sprintf("Processing %d/%d: %s", i, length(input$target_assays), assay_full_name),
								type = "message", 
								duration = 2, 
								id = "current_assay"
							)
							
							(expset_name = get_ExpSet_name(assay_full_name,ExpSet_list()))
							(assay_name = assay_full_name)
							# ✅ Parse the assay name to find which ExpressionSet it belongs to
							# assay_full_name format: "ExpSet_Name:assay_name"
							# parts <- strsplit(assay_full_name, ":")[[1]]
							# if (length(parts) != 2) {
							# 	warning("Invalid assay name format: ", assay_full_name)
							# 	failed_assays <- c(failed_assays, paste0(assay_full_name, " (invalid format)"))
							# 	next
							# }
							
							#expset_name <- parts[1]
							#assay_name <- parts[2]
							
							# Get the ExpressionSet for this assay
							if (! expset_name %in% names(expset_list)) {
								warning("ExpressionSet '", expset_name, "' not found")
								failed_assays <- c(failed_assays, paste0(assay_full_name, " (ExpSet not found)"))
								next
							}
							
							ExpSet <- expset_list[[expset_name]]
							
							# Check if assay exists
							if (! assay_name %in% Biobase::assayDataElementNames(ExpSet)) {
								warning("Assay '", assay_name, "' not found in '", expset_name, "'")
								failed_assays <- c(failed_assays, paste0(assay_full_name, " (assay not found)"))
								next
							}
							
							# Get expression data and metadata
							expr_data <- Biobase::assayDataElement(ExpSet, assay_name)
							meta <- Biobase::pData(ExpSet)
							
							# Validate batch factors
							missing_factors <- setdiff(batch_factors, colnames(meta))
							if (length(missing_factors) > 0) {
								warning("Batch factors not found in '", expset_name, "': ", paste(missing_factors, collapse = ", "))
								failed_assays <- c(failed_assays, paste0(assay_full_name, " (missing batch factors)"))
								next
							}
							
							# Create model matrix
							if (combat_model == "null") {
								modcombat <- model.matrix(~1, data = meta)
								model_description <- "Null model (~1)"
							} else if (! is.null(sample_group) && sample_group %in% colnames(meta)) {
								modcombat <- model.matrix(~ as.factor(meta[[sample_group]]))
								model_description <- paste0("Preserve model (~", sample_group, ")")
							} else {
								modcombat <- model.matrix(~1, data = meta)
								model_description <- "Null model (~1)"
							}
							
							# Create ComBat column and apply correction based on strategy
							combat_column_value <- NULL
							corrected_data <- NULL
							
							if (strategy == "combined") {
								batch_data <- lapply(batch_factors, function(f) as.factor(meta[[f]]))
								combined_batch <- do.call(interaction, c(batch_data, list(drop = TRUE, sep = "_")))
								combat_column_value <- as.character(combined_batch)
								
								batch_table <- table(combined_batch)
								single_sample <- names(batch_table)[batch_table == 1]
								if (length(single_sample) > 0) {
									warning("Cannot correct '", assay_full_name, "': batches with 1 sample")
									failed_assays <- c(failed_assays, paste0(assay_full_name, " (single-sample batches)"))
									next
								}
								
								corrected_data <- sva::ComBat(
									dat = expr_data,
									batch = combined_batch,
									mod = modcombat,
									par.prior = par_prior
								)
								
							} else if (strategy == "sequential") {
								batch_values <- lapply(batch_factors, function(f) as.character(meta[[f]]))
								combat_column_value <- do.call(paste, c(batch_values, list(sep = "_")))
								
								corrected_data <- expr_data
								for (j in seq_along(batch_factors)) {
									batch_factor <- batch_factors[j]
									batch <- meta[[batch_factor]]
									
									corrected_data <- sva::ComBat(
										dat = corrected_data,
										batch = batch,
										mod = modcombat,
										par.prior = par_prior
									)
								}
								
							} else {
								# Single factor
								batch_factor <- batch_factors[1]
								combat_column_value <- as.character(meta[[batch_factor]])
								
								corrected_data <- sva::ComBat(
									dat = expr_data,
									batch = meta[[batch_factor]],
									mod = modcombat,
									par.prior = par_prior
								)
							}
							
							# ✅ Add ComBat column to metadata (only once per ExpressionSet)
							if (! expset_name %in% names(updated_expsets)) {
								meta$ComBat <- combat_column_value
								Biobase::pData(ExpSet) <- meta
								message("  ✅ Added ComBat column to '", expset_name, "' metadata")
							}
							
							# ✅ Add corrected assay to ExpressionSet
							corrected_assay_name <- paste0(assay_name, suffix)
							Biobase::assayDataElement(ExpSet, corrected_assay_name) <- corrected_data
							
							message("  ✅ Created: ", expset_name, ":", corrected_assay_name)
							
							# Store updated ExpressionSet
							expset_list[[expset_name]] <- ExpSet
							updated_expsets[[expset_name]] <- TRUE
							
							corrected_count <- corrected_count + 1
						}
						
						# ✅ Update ExpSet_list
						if (is.function(update_ExpSet_list)) {
							update_ExpSet_list(expset_list)
						} else if (is.function(ExpSet_list)) {
							# If it's a reactiveVal
							ExpSet_list(expset_list)
						}
						
						removeNotification("batch_multi_progress")
						removeNotification("current_assay")
						
						# Build success message
						success_msg <- sprintf(
							"✅ Batch correction complete!\n%d/%d assays corrected\nSuffix: %s\nStrategy: %s\nFactors: %s\nComBat column added to %d ExpressionSet(s)",
							corrected_count,
							length(input$target_assays),
							suffix,
							strategy,
							paste(batch_factors, collapse = " × "),
							length(updated_expsets)
						)
						
						if (length(failed_assays) > 0) {
							success_msg <- paste0(
								success_msg,
								"\n\n⚠️ Failed assays:\n",
								paste(failed_assays, collapse = "\n")
							)
						}
						
						showNotification(success_msg, type = "message", duration = 20)
						
						# Update the assay selector to show new assays
						choices <- get_expset_assay_names(expset_list)
						updatePickerInput(session, "target_assays", choices = choices)
						
						# ✅ Update ExpSet_list (inside the observeEvent)
						if (is.function(update_ExpSet_list)) {
							update_ExpSet_list(expset_list)  # ✅ This updates ExpSet_list_val
							message("✅ Updated ExpSet_list_val with corrected assays")
						} else {
							warning("update_ExpSet_list is not a function")
						}
						
						
					}, error = function(e) {
						removeNotification("batch_multi_progress")
						removeNotification("current_assay")
						showNotification(
							paste("❌ Batch correction failed:", e$message), 
							type = "error", 
							duration = 20
						)
						message("Batch correction error: ", e$message)
						print(traceback())
					})
				})
				


		
		
		# Return values ####
				
				
				return(list(
					corrected_eset = corrected_eset,
					preview_eset = eset_with_combat_preview,
					selected_batch_factors = selected_batch_factors,
					plot_batch_factors = plot_batch_factors,
					all_columns = all_columns,
					safe_factors = safe_batch_factors
				))

	})
}

