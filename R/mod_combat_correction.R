#' ComBat Correction Selector Module - UI
#'
#' Minimal dropdown for batch factor selection with expandable settings
#'
#' @param id Character.  Module namespace ID. 
#' @param show_info Logical. Show info bubble (default TRUE).
#' @param debug Logical. Show debug button (default FALSE).
#' @export
mod_combat_correction_selector_ui <- function(id, 
																							show_info = TRUE,
																							debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		# ✅ Minimal inline selector with optional info icon
		fluidRow(
			column(
				width = if (show_info) 10 else 12,
				uiOutput(ns("combat_selector_ui"))
			),
			if (show_info) {
				column(
					width = 2,
					style = "padding-top: 25px;",
					actionLink(
						ns("toggle_details"),
						icon("info-circle", class = "fa-lg"),
						style = "color: #337ab7;"
					)
				)
			}
		),
		
		# ✅ Collapsible details
		if (show_info) {
			conditionalPanel(
				condition = "input.toggle_details % 2 == 1",
				ns = ns,
				
				fluidRow(
					column(
						width = 12,
						box(
							width = NULL,
							
							
							
							# Factor Safety Info
							box(
								title = "Factor Safety",
								width = 12,
								collapsible = TRUE,
								collapsed = FALSE,
								
								uiOutput(ns("factor_safety_info")),
								
								hr(),
								
								p(strong("Safe factors: "), "Strong batch effect (p < 0.05) and NOT confounded with sample groups (p > 0.05)."),
								
								helpText(
									icon("lightbulb"),
									strong("Multiple factors: "),
									"ComBat can correct for multiple batch factors simultaneously.",
									br(),
									strong("Order matters:"),
									"Factors are corrected in the order selected (most important first)."
								)
							),
							
							# ComBat Settings
							box(
								title = "ComBat Settings",
								width = 12,
								collapsible = TRUE,
								collapsed = TRUE,
								
								fluidRow(
									column(
										width = 6,
										selectInput(
											ns("par_prior"),
											"Parametric Prior:",
											choices = c("Parametric" = TRUE, "Non-parametric" = FALSE),
											selected = TRUE
										),
										helpText("Parametric is faster and works well for most cases.  Use non-parametric for non-normal data."),
										textInput(
											inputId = ns("assay_suffix"),
											label = "Corrected Assay Suffix",
											value = "_ComBat",
											placeholder = "e.g., _ComBat, _corrected"
										),
									),
									
									column(
										width = 6,
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
									)
								),
								# ✅ AUTO-RUN CONTROL
								fluidRow(
									column(
										width = 6,
										shinyWidgets:: materialSwitch(
											inputId = ns("auto_run_combat"),
											label = "Auto-Run ComBat Correction",
											value = TRUE,
											status = "success"
										),
										helpText(
											icon("sync", style = "color:  #337ab7;"),
											tags$small("Automatically run when settings change")
										)
									),
									column(
										width = 6,
										uiOutput(ns("manual_run_ui"))
									)
								)
							),
							
							# Multi-factor options
							uiOutput(ns("multi_factor_options")),
							
							# Batch preview
							uiOutput(ns("batch_preview")),
							
							# Debug button
							if (debug) {
								fluidRow(
									column(
										width = 12,
										style = "margin-top: 10px;",
										actionButton(
											ns("debug"),
											"Debug:  mod_combat_correction_selector",
											icon = icon("bug"),
											class = "btn-warning btn-sm",
											style = "width: 100%;"
										)
									)
								)
							}
						)
					)
				)
			)
		},
		
		### Model Help Modal ####
		bsModal(
			ns("model_help_modal"),
			"ComBat Model Selection Guide",
			trigger = ns("show_model_help"),
			size = "large",
			
			h4(icon("info-circle"), " Understanding ComBat Models"),
			hr(),
			
			h5(tags$span(style = "color: #2c3e50;", icon("calculator"), " Null Model (~1)")),
			p(strong("What it does:"), "Removes ALL variation associated with the batch factor."),
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
							tags$td(tags$span(style = "color:  #27ae60; font-weight: bold;", "Null Model")),
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
			),
			# ✅ Add custom CSS for color-coded options
			tags$head(
				tags$style(HTML("
				. selectize-dropdown-content . safe-factor {
					background-color: #d4edda ! important;
					color: #155724 !important;
					font-weight: bold;
				}
				.selectize-dropdown-content .unsafe-factor {
					background-color: #fff3cd !important;
					color: #856404 !important;
				}
				.selectize-dropdown-content .unknown-factor {
					background-color: #f8f9fa !important;
					color: #6c757d !important;
				}
			"))
			)
		)
	)
}

#' ComBat Correction Selector Module - Server
#'
#' @param id Character. Module namespace ID.
#' @param eset Reactive ExpressionSet. 
#' @param combined_results Reactive data frame from combined batch analysis.
#' @param debug Logical. Enable debug mode (default FALSE).
#' @export
mod_combat_correction_selector_server <- function(id,
																									eset,
																									combined_results = reactive(NULL),
																									debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# ✅ Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("🔍 DEBUG MODE - mod_combat_correction_selector")
				message("  • Selected factors: ", paste(input$batch_factors, collapse = ", "))
				message("  • Model: ", input$combat_model)
				message("  • Strategy: ", input$correction_strategy)
				browser()
			})
		}
		
		# ✅ Manual run UI (shows when auto-run is OFF)
		output$manual_run_ui <- renderUI({
			if (! isTRUE(input$auto_run_combat)) {
				div(
					style = "text-align: center; padding-top: 5px;",
					actionButton(
						ns("run_combat_manual"),
						"Run ComBat Correction",
						icon = icon("play"),
						class = "btn-success btn-lg",
						style = "width: 90%;"
					)
				)
			}
		})
		
		# ✅ Analysis status reactive
		analysis_ready <- reactive({
			! is.null(eset()) && !is.null(combined_results)
		})
		
		# Identify safe batch factors
		safe_batch_factors <- reactive({
			if (is.null(combined_results) || ! is.function(combined_results)) {
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
			
			batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
			
			if (is.na(batch_col_name)) {
				warning("Could not find batch effect p-value column")
				return(character(0))
			}
			
			safe <- df %>%
				rename(batch_p = !!sym(batch_col_name)) %>%
				filter(batch_p < 0.05, Fisher_p_value > 0.05) %>%
				arrange(batch_p) %>%
				pull(Batch_Column)
			
			as.character(safe)
		})
		
		# ✅ Identify unsafe batch factors (confounded)
		unsafe_batch_factors <- reactive({
			if (is.null(combined_results) || !is.function(combined_results)) {
				return(character(0))
			}
			
			df <- tryCatch({
				combined_results()
			}, error = function(e) NULL)
			
			if (is.null(df) || nrow(df) == 0) {
				return(character(0))
			}
			
			batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
			
			if (is.na(batch_col_name)) {
				return(character(0))
			}
			
			unsafe <- df %>%
				rename(batch_p = !!sym(batch_col_name)) %>%
				filter(batch_p < 0.05, Fisher_p_value < 0.05) %>%  # Batch effect AND confounded
				pull(Batch_Column)
			
			as.character(unsafe)
		})
		
		# ✅ Render batch factor selector with loading state and color coding
		output$combat_selector_ui <- renderUI({
			
			# ✅ Check 1: Is eset available?
			if (is.null(eset())) {
				return(
					tagList(
						div(
							style = "padding: 10px 15px; text-align: center; background-color: #f8f9fa; border: 1px dashed #dee2e6; border-radius: 4px;",
							icon("database", style = "color: #adb5bd; margin-right: 8px;"),
							tags$span(style = "color: #6c757d;", "Waiting for data...")
						)
					)
				)
			}
			
			# ✅ Check 2: Is batch analysis running/pending?
			if (is.null(combined_results) || is.function(combined_results)) {
				# Try to get results
				results <- tryCatch({
					combined_results()
				}, error = function(e) NULL)
				
				# If no results yet, show analyzing
				if (is.null(results) || nrow(results) == 0) {
					return(
						tagList(
							div(
								style = "padding: 10px 15px; text-align: center; background-color: #e7f3ff; border: 1px solid #b3d9ff; border-radius: 4px;",
								icon("spinner", class = "fa-spin", style = "color: #337ab7; margin-right: 8px;"),
								tags$span(style = "color: #004085;", "Analyzing batch factors...")
							)
						)
					)
				}
			}
			
			# ✅ Check 3: Get metadata columns
			all_columns <- tryCatch({
				colnames(Biobase::pData(eset()))
			}, error = function(e) {
				character(0)
			})
			
			if (length(all_columns) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No metadata columns available"),
						p("Cannot identify batch factors without sample metadata.")
					)
				)
			}
			
			# ✅ Now we have data - get factor categorizations
			safe_factors <- safe_batch_factors()
			unsafe_factors <- unsafe_batch_factors()
			
			default_selection <- if (length(safe_factors) > 0) {
				safe_factors
			} else {
				NULL
			}
			
			# ✅ Categorize factors
			safe_list <- intersect(all_columns, safe_factors)
			unsafe_list <- intersect(all_columns, unsafe_factors)
			unknown_list <- setdiff(all_columns, c(safe_list, unsafe_list))
			
			# ✅ Build choice list dynamically (only include non-empty groups)
			choice_groups <- list()
			
			if (length(safe_list) > 0) {
				choice_groups[["✓ Safe Factors (Recommended)"]] <- setNames(
					safe_list,
					paste0("✓ ", safe_list)
				)
			}
			
			if (length(unsafe_list) > 0) {
				choice_groups[["⚠ Confounded Factors (Caution)"]] <- setNames(
					unsafe_list,
					paste0("⚠ ", unsafe_list)
				)
			}
			
			if (length(unknown_list) > 0) {
				choice_groups[["○ Other Factors"]] <- setNames(
					unknown_list,
					paste0("○ ", unknown_list)
				)
			}
			
			# ✅ Fallback:  if no groups created, use plain list
			if (length(choice_groups) == 0) {
				choice_groups <- setNames(all_columns, all_columns)
			}
			
			# ✅ SUCCESS - Render the selector
			tagList(
				# Selector input
				shinyWidgets::pickerInput(
					ns("batch_factors"),
					"Select Batch Factor(s):",
					choices = choice_groups,
					selected = default_selection,
					multiple = TRUE,
					options = pickerOptions(
						actionsBox = TRUE,
						selectedTextFormat = "count > 2",
						liveSearch = TRUE,
						style = "btn-default",
						title = "Choose batch factors..."
					),
					width = "100%"
				),
				
				# ✅ Status indicator bar
				tags$div(
					style = "margin-top: 5px;",
					if (length(safe_factors) > 0) {
						div(
							style = "padding:  8px 12px; background-color: #d4edda; border-left: 4px solid #28a745; border-radius: 4px;",
							icon("check-circle", style = "color: #28a745;"),
							tags$span(
								strong(sprintf("%d safe factor(s) available", length(safe_factors))),
								style = "color: #155724; margin-left: 8px;"
							)
						)
					} else if (length(unsafe_factors) > 0) {
						div(
							style = "padding: 8px 12px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
							icon("exclamation-triangle", style = "color:  #ffc107;"),
							tags$span(
								strong("No safe factors identified"),
								style = "color:  #856404; margin-left: 8px;"
							),
							tags$small(
								" (confounded factors available)",
								style = "color: #856404; margin-left: 5px;"
							)
						)
					} else {
						div(
							style = "padding:  8px 12px; background-color: #d1ecf1; border-left:  4px solid #17a2b8; border-radius: 4px;",
							icon("info-circle", style = "color:  #17a2b8;"),
							tags$span(
								strong("Analysis complete - no categorized factors"),
								style = "color: #0c5460; margin-left: 8px;"
							)
						)
					}
				),
				
				# Help text
				helpText("Select one or more batch factors to correct using ComBat")
			)
		})
		
		
		# Factor safety information
		output$factor_safety_info <- renderUI({
			req(input$batch_factors)
			
			selected <- input$batch_factors
			safe <- safe_batch_factors()
			
			if (length(selected) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No factors selected"),
						p("Please select at least one batch factor to proceed.")
					)
				)
			}
			
			safe_selected <- intersect(selected, safe)
			unsafe_selected <- setdiff(selected, safe)
			
			tagList(
				if (length(safe_selected) > 0) {
					div(
						class = "alert alert-success",
						icon("check-circle"),
						strong(sprintf(" %d safe factor(s) selected", length(safe_selected))),
						tags$ul(
							lapply(safe_selected, function(x) tags$li(x))
						),
						p(style = "margin-bottom: 0;", "These factors have significant batch effects and are NOT confounded with sample groups.")
					)
				},
				
				if (length(unsafe_selected) > 0) {
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(sprintf(" %d unverified factor(s) selected", length(unsafe_selected))),
						tags$ul(
							lapply(unsafe_selected, function(x) tags$li(x))
						),
						p(style = "margin-bottom: 0;", 
							em("These factors were not identified as 'safe'.  They may be confounded with sample groups or have no significant batch effect.")
						)
					)
				},
				
				if (length(safe) == 0 && length(selected) > 0) {
					div(
						class = "alert alert-info",
						icon("info-circle"),
						strong(" No safe batch factors identified from analysis"),
						p("You can still proceed, but be cautious of potential confounding with biological groups.")
					)
				}
			)
		})
		
		# Multi-factor options
		output$multi_factor_options <- renderUI({
			req(input$batch_factors)
			
			if (length(input$batch_factors) <= 1) {
				return(NULL)
			}
			
			box(
				title = "Multi-Factor Correction Strategy",
				width = 12,
				collapsible = TRUE,
				collapsed = TRUE,
				
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
					strong("Combined:  "),
					"Creates unique batches for each combination (e.g., Batch1_Date1, Batch1_Date2). More statistically rigorous."
				),
				
				helpText(
					icon("info-circle"),
					strong("Sequential:  "),
					"Corrects for first factor, then second, etc. Order matters."
				),
				
				conditionalPanel(
					condition = "input.correction_strategy == 'sequential'",
					ns = ns,
					helpText(
						icon("exclamation-triangle"),
						strong("Note: "),
						"Factors will be corrected in the order shown above."
					)
				)
			)
		})
		
		# Batch preview
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
						collapsible = TRUE,
						collapsed = FALSE,
						
						div(
							class = "alert alert-danger",
							icon("times-circle"),
							strong(" Error: "),
							"Batch factors not found in metadata:  ",
							paste(missing, collapse = ", ")
						)
					)
				)
			}
			
			if (strategy == "combined") {
				# Combined strategy preview
				batch_data <- lapply(batch_factors, function(f) as.factor(meta[[f]]))
				combined_batch <- do.call(interaction, c(batch_data, list(drop = TRUE, sep = "_")))
				
				batch_table <- table(combined_batch)
				n_combinations <- length(batch_table)
				batch_sizes <- as.data.frame(batch_table)
				colnames(batch_sizes) <- c("Batch_Combination", "N_Samples")
				batch_sizes <- batch_sizes[order(batch_sizes$N_Samples), ]
				
				small_batches <- batch_sizes$Batch_Combination[batch_sizes$N_Samples < 3]
				single_sample_batches <- batch_sizes$Batch_Combination[batch_sizes$N_Samples == 1]
				
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
					collapsible = TRUE,
					collapsed = FALSE,
					
					div(
						class = paste("alert", status_class),
						icon(icon_symbol),
						strong(paste0(" ", status_text)),
						tags$ul(
							tags$li(strong("Total unique batch combinations:  "), n_combinations),
							tags$li(strong("Factors combined: "), paste(batch_factors, collapse = " × ")),
							tags$li(strong("Sample size range: "), 
											min(batch_sizes$N_Samples), " - ", max(batch_sizes$N_Samples))
						)
					),
					
					if (length(single_sample_batches) > 0) {
						div(
							class = "alert alert-danger",
							icon("ban"),
							strong(" CRITICAL: ", length(single_sample_batches), " batch combination(s) have only 1 sample"),
							p("ComBat cannot correct batches with only 1 sample. "),
							p(strong("Recommendation:  "), "Use sequential correction or exclude problematic factors")
						)
					},
					
					if (length(small_batches) > 0 && length(single_sample_batches) == 0) {
						div(
							class = "alert alert-warning",
							icon("exclamation-triangle"),
							strong(" Warning: ", length(small_batches), " batch combination(s) have < 3 samples"),
							p("Small batches may lead to unstable correction. "),
							p(strong("Recommendation: "), "Proceed with caution or use sequential correction")
						)
					},
					
					if (status == "success") {
						div(
							class = "alert alert-success",
							icon("check-circle"),
							strong(" All batch combinations have adequate sample sizes (≥ 3)")
						)
					}
				)
				
			} else {
				# Sequential strategy preview
				factor_info <- lapply(batch_factors, function(f) {
					vals <- meta[[f]]
					batch_sizes <- table(vals)
					
					list(
						factor = f,
						n_groups = length(unique(vals)),
						min_size = min(batch_sizes),
						max_size = max(batch_sizes)
					)
				})
				
				box(
					title = "Sequential Correction Preview",
					width = 12,
					collapsible = TRUE,
					collapsed = FALSE,
					
					div(
						class = "alert alert-info",
						icon("info-circle"),
						strong(" Sequential correction will process factors in this order:")
					),
					
					lapply(seq_along(factor_info), function(i) {
						info <- factor_info[[i]]
						
						tagList(
							h5(paste0(i, ". ", info$factor)),
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
							if (i < length(factor_info)) hr()
						)
					})
				)
			}
		})
		
		### Return with defaults ####
		return(list(
			batch_factors = reactive({
				#input$batch_factors %||% character(0)
				#input$batch_factors %||% "Labels"
				input$batch_factors
			}),
			
			par_prior = reactive({
				if (is.null(input$par_prior)) {
					TRUE  # Default to parametric
				} else {
					as.logical(input$par_prior)
				}
			}),
			
			assay_suffix = reactive({
				input$assay_suffix %||% "_ComBat"
			}),
			
			combat_model = reactive({
				input$combat_model %||% "null"
			}),
			
			correction_strategy = reactive({
				# Only relevant if multiple factors selected
				n_factors <- length(input$batch_factors %||% character(0))
				if (n_factors > 1) {
					input$correction_strategy %||% "combined"
				} else {
					"combined"  # Default for single factor (doesn't matter)
				}
			}),
			
			safe_factors = safe_batch_factors,
			
			auto_run_combat = reactive({
				# Default to TRUE if not set
				if (is.null(input$auto_run_combat)) {
					TRUE
				} else {
					isTRUE(input$auto_run_combat)
				}
			}),
			
			run_combat_manual = reactive({
				input$run_combat_manual %||% 0
			})
		))
	})
}


#, #####
# SINGLE COMBAT CREATION #####
#' Single ComBat Correction Module - UI
#'
#' Displays correction status and results with expandable advanced details
#'
#' @param id Module namespace ID
#' @param show_auto_run_toggle Logical.  Show auto-run toggle switch (default TRUE).
#' @param debug Show debug button
#' @export
mod_combat_single_ui <- function(id, 
																 show_auto_run_toggle = TRUE,
																 debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(

			# 	# ✅ Advanced Options button at bottom
				uiOutput(ns("results_box_ui")),

		)
	)
}

#' Single ComBat Correction Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param sample_group_column Reactive sample group column name
#' @param combined_results Reactive batch analysis results
#' @param selector ComBat selector module return object
#' @param show_auto_run_toggle Logical. Enable auto-run toggle
#' @param debug Enable debug mode
#' @export
mod_combat_single_server <- function(id,
																		 eset,
																		 sample_group_column,
																		 combined_results = reactive(NULL),
																		 selector = NULL,
																		 show_auto_run_toggle = TRUE,
																		 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# Debug
		if (isTRUE(debug)) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - ComBat Single Correction Module")
				message("═══════════════════════════════════════════════")
				browser()
			})
		}
		
		# ✅ Smart results box with dynamic title
		output$results_box_ui <- renderUI({
	
			
			# Determine state
			state <- if (correction_in_progress()) {
				"running"
			} else if (! is.null(correction_error())) {
				"failed"  # ✅ NEW STATE
			} else if (! is.null(corrected_eset())) {
				"success"
			} else if (! is.null(eset()) && length(selector$batch_factors()) == 0) {
				"no_factors"
			} else if (!is.null(eset()) && is.null(sample_group_column())) {
				"no_sample_group"
			} else if (is.null(eset())) {
				"no_data"
			} else {
				"not_run"
			}
			
			# Set title and status based on state
			box_config <- switch(state,
													 "running" = list(
													 	title = "⏳ ComBat Currently Running.. .",
													 	status = "info",
													 	icon_name = "spinner fa-spin"
													 ),
													 "failed" = list(  # ✅ NEW
													 	title = "❌ ComBat Failed",
													 	status = "danger",
													 	icon_name = "times-circle"
													 ),
													 "success" = list(
													 	title = "✅ ComBat Completed Successfully",
													 	status = "success",
													 	icon_name = "check-circle"
													 ),
													 "no_factors" = list(
													 	title = "⚠️ ComBat Unable to Run - No Batch Factors Selected",
													 	status = "warning",
													 	icon_name = "exclamation-triangle"
													 ),
													 "no_sample_group" = list(
													 	title = "⚠️ ComBat Unable to Run - No Sample Group Selected",
													 	status = "warning",
													 	icon_name = "exclamation-triangle"
													 ),
													 "no_data" = list(
													 	title = "⚠️ ComBat Unable to Run - No Data Available",
													 	status = "warning",
													 	icon_name = "exclamation-triangle"
													 ),
													 list(
													 	title = "⏸️ ComBat Not Run",
													 	status = "primary",  # ✅ Use valid status
													 	icon_name = "pause-circle"
													 )
			)
			
			box(
				title = box_config$title,
				width = 12,
				status = box_config$status,
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				verbatimTextOutput(ns("correction_summary")),
				
				if (debug) {
					div(
						style = "margin-top: 15px;",
						actionButton(
							ns("debug"),
							"Debug:  mod_combat_single",
							icon = icon("bug"),
							class = "btn-warning btn-sm",
							style = "width: 100%;"
						)
					)
				}
			)
		})
		
		# Store corrected ExpressionSet
		corrected_eset <- reactiveVal(NULL)
		correction_in_progress <- reactiveVal(FALSE)
		correction_error <- reactiveVal(NULL)
		
		# ✅ Status display (thin banner)
		output$correction_status_display <- renderUI({
			if (correction_in_progress()) {
				# Show progress - thin banner
				div(
					class = "alert alert-info",
					style = "margin-bottom: 15px; padding: 10px 15px;",
					icon("spinner", class = "fa-spin"),
					strong(" Running ComBat correction..."),
					tags$small(" This may take a moment", style = "margin-left: 10px; color: #6c757d;")
				)
			} else if (! is.null(corrected_eset())) {
				# Show success banner - thin
				div(
					class = "alert alert-success",
					style = "margin-bottom: 15px; padding: 10px 15px;",
					icon("check-circle"),
					strong(" Batch Correction Complete!"),
					tags$small(
						sprintf(" %d features × %d samples corrected", 
										nrow(corrected_eset()), 
										ncol(corrected_eset())),
						style = "margin-left: 10px;"
					)
				)
			} else {
				# Show waiting state - thin
				div(
					class = "alert alert-warning",
					style = "margin-bottom: 15px; padding:  10px 15px;",
					icon("hourglass-half"),
					strong(" Ready to correct"),
					tags$small(" Waiting for batch factor selection.. .", style = "margin-left: 10px; color: #6c757d;")
				)
			}
		})

		
		# ✅ ComBat correction wrapper - ####
		run_combat_correction_wrapper <- function() {
			correction_in_progress(TRUE)
			correction_error(NULL)
			showNotification("Running ComBat correction...", type = "message", duration = NULL, id = "combat_progress")
			
			tryCatch({
				# Get current settings
				ExpSet <- eset()
				batch_factors <- selector$batch_factors()
				combat_model <- selector$combat_model()
				sample_group <- sample_group_column()
				par_prior <- selector$par_prior()
				assay_suffix <- selector$assay_suffix()  # ✅ NEW:  Get suffix from selector
				strategy <- selector$correction_strategy()
			
				
				# ✅ Call the standalone function
				corrected_ExpSet <- run_combat_correction(
					eset = ExpSet,
					batch_factors = batch_factors,
					sample_group = sample_group,
					strategy = strategy,
					combat_model = combat_model,
					par_prior = par_prior,
					assay_suffix = assay_suffix,  # ✅ NEW: Pass suffix to function
					debug = debug
				)
				
				# Update reactive values
				corrected_eset(corrected_ExpSet)
				correction_in_progress(FALSE)
				
				removeNotification("combat_progress")
				showNotification("✅ ComBat correction complete!", type = "message", duration = 10)
				
			}, error = function(e) {
				correction_in_progress(FALSE)
				corrected_eset(NULL)
				correction_error(e$message)
				removeNotification("combat_progress")
				showNotification(paste("❌ ComBat failed:", e$message), type = "error", duration = 15)
				
				message("❌ ComBat error:  ", e$message)
			})
		}
		
		### ✅ Auto-run logic 1  ####
		observeEvent(list(
			eset(),
			selector$batch_factors(),
			selector$combat_model(),
			selector$par_prior(),
			selector$assay_suffix(),
			selector$correction_strategy(),
			sample_group_column(),
			selector$auto_run_combat()
		), {
			if (isTRUE(selector$auto_run_combat())) {  # ✅ Use selector's toggle
				req(eset())
				req(selector$batch_factors())
				req(sample_group_column())
				
				batch_factors <- selector$batch_factors()
				combat_model <- selector$combat_model()
				
				if (length(batch_factors) == 0) {
					return()
				}
				
				# Validation warning for confounded factors
				if (combat_model == "null") {
					if (! is.null(combined_results())) {
						df <- combined_results()
						batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
						
						unsafe_factors <- df %>%
							rename(batch_p = !!  sym(batch_col_name)) %>%
							filter(Batch_Column %in% batch_factors, Fisher_p_value < 0.05) %>%
							pull(Batch_Column)
						
						if (length(unsafe_factors) > 0) {
							showModal(
								modalDialog(
									title = tags$span(icon("exclamation-triangle"), " Warning:   Confounding Detected"),
									size = "l",
									
									p(strong("The following batch factors are confounded with sample groups:")),
									tags$ul(lapply(unsafe_factors, function(x) tags$li(x))),
									
									p(style = "color: #e74c3c;", strong("Using the Null Model may remove real biological differences!")),
									
									p("Recommendations:"),
									tags$ol(
										tags$li("Switch to 'Preserve Sample Groups' model (safer), OR"),
										tags$li("Only correct factors with Fisher p > 0.05, OR"),
										tags$li("Proceed with caution if you understand the risks")
									),
									
									footer = tagList(
										actionButton(ns("cancel_combat"), "Cancel", class = "btn-default"),
										actionButton(ns("proceed_combat"), "Proceed Anyway", class = "btn-danger")
									)
								)
							)
							return()
						}
					}
				}
				
				run_combat_correction_wrapper()
			}
		}, ignoreInit = TRUE)
		
		# ✅ Manual run
		observeEvent(selector$run_combat_manual(), { 
			req(eset())
			req(selector$batch_factors())
			req(sample_group_column())
			
			if (length(selector$batch_factors()) == 0) {
				showNotification("⚠️ Please select at least one batch factor", type = "warning", duration = 5)
				return()
			}
			
			run_combat_correction_wrapper()
		})
		
		# Modal handlers
		observeEvent(input$proceed_combat, {
			removeModal()
			run_combat_correction_wrapper()
		})
		
		observeEvent(input$cancel_combat, {
			removeModal()
		})
		
		# ✅ Correction summary #####
		output$correction_summary <- renderPrint({
			# Show why ComBat can't run
			
			# ✅ Show error if failed
			if (! is.null(correction_error())) {
				cat("═══════════════════════════════════════════════\n")
				cat("COMBAT CORRECTION FAILED\n")
				cat("═══════════════════════════════════════════════\n\n")
				cat("Error:", correction_error(), "\n\n")
				cat("Current settings:\n")
				cat("  Batch factors:", paste(selector$batch_factors(), collapse = ", "), "\n")
				cat("  Model:", selector$combat_model(), "\n")
				cat("  Sample group:", sample_group_column(), "\n\n")
				cat("Please check your data and settings.\n")
				return()
			}
			
			if (is.null(eset())) {
				cat("═══════════════════════════════════════════════\n")
				cat("UNABLE TO RUN COMBAT\n")
				cat("═══════════════════════════════════════════════\n\n")
				cat("Reason: No data loaded\n")
				cat("Action: Please load an ExpressionSet\n")
				return()
			}
			
			if (length(selector$batch_factors()) == 0) {
				cat("═══════════════════════════════════════════════\n")
				cat("UNABLE TO RUN COMBAT\n")
				cat("═══════════════════════════════════════════════\n\n")
				cat("Reason: No batch factors selected\n")
				cat("Action: Please select at least one batch factor from the selector above\n")
				return()
			}
			
			if (is.null(sample_group_column())) {
				cat("═══════════════════════════════════════════════\n")
				cat("UNABLE TO RUN COMBAT\n")
				cat("═══════════════════════════════════════════════\n\n")
				cat("Reason: No sample group column selected\n")
				cat("Action: Please select a sample group column in the Batch Analysis tab\n")
				return()
			}
			
			if (correction_in_progress()) {
				cat("═══════════════════════════════════════════════\n")
				cat("COMBAT CORRECTION IN PROGRESS\n")
				cat("═══════════════════════════════════════════════\n\n")
				cat("Please wait.. .\n")
				return()
			}
			
			if (is.null(corrected_eset())) {
				cat("═══════════════════════════════════════════════\n")
				cat("COMBAT NOT YET RUN\n")
				cat("═══════════════════════════════════════════════\n\n")
				cat("Current settings:\n")
				cat("  Batch factors:", paste(selector$batch_factors(), collapse = ", "), "\n")
				cat("  Model:", selector$combat_model(), "\n")
				cat("  Sample group:", sample_group_column(), "\n\n")
				cat("Ready to run when auto-run is enabled or run button is clicked.\n")
				return()
			}
			
			# Show success summary
			ExpSet_corrected <- corrected_eset()
			notes <- Biobase::notes(ExpSet_corrected)
			(combat_info <- notes$combat_correction)
			
			cat("═══════════════════════════════════════════════\n")
			cat("COMBAT CORRECTION SUMMARY\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			if (! is.null(combat_info)) {
				cat("Strategy:", combat_info$method, "\n")
				cat("Model used:", combat_info$model, "\n")
				
				# ✅ Handle both singular and plural batch factor names
				batch_factors_used <- if (! is.null(combat_info$batch_factors)) {
					combat_info$batch_factors
				} else if (!is.null(combat_info$batch_factor)) {
					combat_info$batch_factor 
				} else {
					"Unknown"
				}
				cat("Batch factors:", paste(batch_factors_used, collapse = ", "), "\n")
				
				cat("Par. prior:", combat_info$par_prior, "\n")
				cat("Correction date:", as.character(combat_info$correction_date), "\n\n")
			}
			
			cat("Dimensions:", nrow(ExpSet_corrected), "features ×", ncol(ExpSet_corrected), "samples\n")
		})
		
		# Return values
		return(list(
			corrected_eset = corrected_eset,
			correction_complete = reactive(! is.null(corrected_eset()))
		))
	})
}

#. #####
# COMBAT MULTI #####
#' Multi-Assay Batch Correction UI
#'
#' @param id Module ID
#' @export
mod_combat_multi_assay_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Batch Correction for Multiple Assays",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,  # ✅ Starts collapsed
				
				p("Apply batch correction settings to multiple assays at once. "),
				p(
					icon("info-circle", style = "color: #337ab7;"),
					strong("Note: "), 
					"Uses the current ComBat settings from the Batch Correction tab."
				),
				
				hr(),
				
				# ✅ Auto-run toggle at top
				fluidRow(
					column(
						width = 6,
						shinyWidgets::materialSwitch(
							inputId = ns("auto_run_multi"),
							label = "Auto-Run on Settings Change",
							value = TRUE,
							status = "success"
						),
						helpText(
							icon("sync", style = "color: #337ab7;"),
							tags$small("Automatically process when batch settings or target assays change")
						)
					),
					column(
						width = 6,
						# ✅ Manual button (only shows when auto-run is OFF)
						conditionalPanel(
							condition = "!input. auto_run_multi",
							ns = ns,
							actionButton(
								ns("apply_multi_batch_correction"),
								"Apply Batch Correction to Selected Assays",
								icon = icon("play-circle"),
								class = "btn-success btn-lg",
								style = "width: 100%; margin-top:  0px;"
							)
						)
					)
				),
				
				hr(),
				
				# Target assay selection
				fluidRow(
					column(
						width = 6,
						shinyWidgets::pickerInput(
							ns("target_assays"),
							"Target Assays:",
							choices = NULL,
							selected = NULL,
							multiple = TRUE,
							options = shinyWidgets::pickerOptions(
								actionsBox = TRUE,
								selectedTextFormat = "count > 2",
								liveSearch = TRUE,
								title = "Select one or more assays to correct"
							)
						),
						helpText("Select which assays to apply batch correction to")
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
				
				# Preview of what will be created
				fluidRow(
					column(
						width = 6,
						div(
							style = "padding: 10px; background-color: #f8f9fa; border-radius: 4px; min-height: 100px;",
							h5(icon("list"), " Selected Assays: "),
							htmlOutput(ns("target_assays_text"))
						)
					),
					column(
						width = 6,
						div(
							style = "padding: 10px; background-color: #e7f3ff; border-radius:  4px; min-height:  100px;",
							h5(icon("magic"), " Will Create:"),
							htmlOutput(ns("target_assays_combat_text"))
						)
					)
				),
				
				hr(),
				
				# Debug button
				if (debug) {
					fluidRow(
						column(
							width = 12,
							actionButton(
								ns("debug"),
								"Debug:  Multi-Assay Batch Correction",
								icon = icon("bug"),
								class = "btn-warning btn-sm",
								style = "width: 100%;"
							)
						)
					)
				}
			)
		),
		
		# ✅ Status Banner (always visible - outside collapsed box)
		fluidRow(
			column(
				width = 12,
				uiOutput(ns("multi_correction_status"))
			)
		),
		
		# ✅ Detailed Log (collapsible, outside main box)
		fluidRow(
			column(
				width = 12,
				uiOutput(ns("correction_log"))
			)
		)
	)
}

#' Multi-Assay Batch Correction Server
#'
#' @param id Module ID
#' @param ExpSet_list Reactive containing list of ExpressionSets
#' @param update_ExpSet_list Function to update the ExpSet_list
#' @param selector Reactive list with combat settings (batch_factors, combat_model, par_prior, strategy, sample_group)
#' @param debug Logical, enable debug mode
#' @export
mod_combat_multi_assay_server <- function(id,
																					ExpSet_list,
																					update_ExpSet_list,
																					sample_group_column,
																					selector,
																					default_target_assays = NULL,
																					debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns 
		 
		observeEvent(input$debug, {
			message("🔍 DEBUG MODE - mod_combat_multi_assay_server")
			browser()
		})
		
		# Reactive values for tracking
		correction_log_data <- reactiveVal(NULL)
		processing_status <- reactiveVal("idle")  # idle, running, complete, error
		
		# Update assay choices when ExpSet_list changes
		initialized <- reactiveVal(FALSE)
		
		# ✅ Only trigger when ExpSet_list changes
		observeEvent(ExpSet_list(), {
			message("🔍 Observer triggered by ExpSet_list")
			
			choices <- unlist(get_expset_assay_names(ExpSet_list()))
			message("  - Available choices: ", length(choices))
			message("  - Initialized: ", initialized())
			
			# Read current selection
			current_selection <- isolate(input$target_assays)
			message("  - Current selection: ", 
							if (is.null(current_selection) || length(current_selection) == 0) "(none)" 
							else paste(current_selection, collapse = ", "))
			
			# ✅ Determine what to select
			if (!initialized() || is.null(current_selection) || length(current_selection) == 0) {
				message("  - Applying defaults (first time or selection is empty)")
				
				desired_defaults <- if (is.reactive(default_target_assays)) {
					default_target_assays()
				} else {
					default_target_assays
				}
				
				message("  - Desired defaults: ", paste(desired_defaults, collapse = ", "))
				
				if (!is.null(desired_defaults) && length(desired_defaults) > 0) {
					default_selection <- intersect(desired_defaults, choices)
					message("  - Matched defaults: ", length(default_selection), " of ", length(desired_defaults))
				} else {
					default_selection <- NULL
					message("  - No defaults provided")
				}
				
				initialized(TRUE)
				
			} else {
				message("  - Preserving user selection")
				default_selection <- current_selection
			}
			
			message("  - Updating picker with:  ", 
							if (is.null(default_selection) || length(default_selection) == 0) "(none)" 
							else paste(default_selection, collapse = ", "))
			
			shinyWidgets::updatePickerInput(
				session, 
				"target_assays", 
				choices = choices,
				selected = default_selection
			)
			
			message("  - Update complete\n")
		}, ignoreNULL = FALSE)
		
		# Display selected assays
		output$target_assays_text <- renderText({
			req(input$target_assays)
			if (length(input$target_assays) == 0) {
				return("<em>No assays selected</em>")
			}
			paste(input$target_assays, collapse = "<br>")
		})
		
		# Display corrected assay names
		output$target_assays_combat_text <- renderText({
			req(input$target_assays)
			req(input$combat_assay_suffix)
			if (length(input$target_assays) == 0) {
				return("<em>No assays selected</em>")
			}
			paste(paste0(input$target_assays, input$combat_assay_suffix), collapse = "<br>")
		})
		
		# ✅ Check if all conditions are met to run correction
		ready_to_run <- reactive({
			# Check all required inputs
			has_expset <- !is.null(ExpSet_list()) && length(ExpSet_list()) > 0
			has_batch <- !is.null(selector$batch_factors()) && length(selector$batch_factors()) > 0
			has_assays <- !is.null(input$target_assays) && length(input$target_assays) > 0
			has_suffix <- !is.null(input$combat_assay_suffix) && nchar(input$combat_assay_suffix) > 0
			
			all(has_expset, has_batch, has_assays, has_suffix)
		})
		
		# ✅ CORE FUNCTION:  Extract correction logic into reusable function
		run_multi_correction <- function() {
			req(ExpSet_list())
			req(selector)
			req(input$target_assays)
			req(input$combat_assay_suffix)
			
			processing_status("running")
			
			tryCatch({
				expset_list <- ExpSet_list()
				
				# Extract settings
				batch_factors <- selector$batch_factors()
				combat_model <- selector$combat_model()
				sample_group <- sample_group_column()
				par_prior <- selector$par_prior()
				strategy <- selector$correction_strategy()
				suffix <- input$combat_assay_suffix
				
				# Validation
				if (is.null(batch_factors) || length(batch_factors) == 0) {
					stop("No batch factors selected")
				}
				
				# Track results
				corrected_count <- 0
				failed_assays <- character()
				updated_expsets <- list()
				log_details <- character()
				
				# Process each target assay
				for (i in seq_along(input$target_assays)) {
					assay_full_name <- input$target_assays[i]
					
					log_details <- c(log_details, "========================================")
					log_details <- c(log_details, sprintf("Processing %d/%d: %s", i, length(input$target_assays), assay_full_name))
					
					# Parse assay name
					expset_name <- get_ExpSet_name(assay_full_name, ExpSet_list())
					assay_name <- assay_full_name
					
					# Validate ExpressionSet exists
					if (! expset_name %in% names(expset_list)) {
						msg <- paste0(assay_full_name, " (ExpSet not found)")
						failed_assays <- c(failed_assays, msg)
						log_details <- c(log_details, paste("  ❌ FAILED:", msg))
						next
					}
					
					ExpSet <- expset_list[[expset_name]]
					
					# Validate assay exists
					if (!assay_name %in% Biobase::assayDataElementNames(ExpSet)) {
						msg <- paste0(assay_full_name, " (assay not found)")
						failed_assays <- c(failed_assays, msg)
						log_details <- c(log_details, paste("  ❌ FAILED:", msg))
						next
					}
					
					# Create a temporary ExpressionSet with the target assay as exprs
					temp_ExpSet <- ExpSet
					Biobase::exprs(temp_ExpSet) <- Biobase::assayDataElement(ExpSet, assay_name)
					
					# Add note about which assay we're correcting
					notes <- Biobase::notes(temp_ExpSet)
					notes$current_assay <- assay_name
					Biobase::notes(temp_ExpSet) <- notes
					
					# Call the standalone run_combat_correction function
					corrected_ExpSet <- tryCatch({
						run_combat_correction(
							eset = temp_ExpSet,
							batch_factors = batch_factors,
							sample_group = sample_group,
							strategy = strategy,
							combat_model = combat_model,
							par_prior = par_prior,
							assay_suffix = suffix,
							debug = debug
						)
					}, error = function(e) {
						msg <- paste0(assay_full_name, " (", e$message, ")")
						failed_assays <<- c(failed_assays, msg)
						log_details <<- c(log_details, paste("  ❌ FAILED:", msg))
						return(NULL)
					})
					
					if (is.null(corrected_ExpSet)) {
						next
					}
					
					# Extract the corrected assay and add it to the original ExpressionSet
					corrected_assay_name <- paste0(assay_name, suffix)
					
					# Get the corrected data from the corrected ExpressionSet
					all_assays <- Biobase::assayDataElementNames(corrected_ExpSet)
					if (corrected_assay_name %in% all_assays) {
						corrected_data <- Biobase:: assayDataElement(corrected_ExpSet, corrected_assay_name)
					} else {
						corrected_data <- Biobase::exprs(corrected_ExpSet)
					}
					
					# Add corrected assay to original ExpressionSet
					ExpSet <- Biobase::assayDataElementReplace(
						ExpSet,
						corrected_assay_name,
						corrected_data,
						validate = FALSE
					)
					
					# Update metadata with ComBat column (only once per ExpressionSet)
					if (!expset_name %in% names(updated_expsets)) {
						updated_meta <- Biobase::pData(corrected_ExpSet)
						if ("ComBat" %in% colnames(updated_meta)) {
							Biobase::pData(ExpSet) <- updated_meta
							log_details <- c(log_details, paste("  ✅ Added ComBat column to", expset_name))
						}
					}
					
					# Store updated ExpressionSet
					expset_list[[expset_name]] <- ExpSet
					updated_expsets[[expset_name]] <- TRUE
					
					log_details <- c(log_details, paste("  ✅ Created:", corrected_assay_name))
					corrected_count <- corrected_count + 1
				}
				
				# Update ExpSet_list
				if (! is.null(update_ExpSet_list) && is.function(update_ExpSet_list)) {
					update_ExpSet_list(expset_list)
					log_details <- c(log_details, "✅ Updated ExpSet_list")
				} else {
					warning("update_ExpSet_list is not a function - ExpSet_list was not updated")
					log_details <- c(log_details, "⚠️ Could not update ExpSet_list (not a function)")
				}
				
				# Update correction log
				correction_log_data(list(
					corrected_count = corrected_count,
					total_count = length(input$target_assays),
					failed_assays = failed_assays,
					suffix = suffix,
					strategy = strategy,
					batch_factors = batch_factors,
					updated_expsets_count = length(updated_expsets),
					details = log_details,
					timestamp = Sys.time()
				))
				
				processing_status("complete")
				
				# Return summary
				list(
					success = corrected_count > 0,
					corrected_count = corrected_count,
					total_count = length(input$target_assays),
					failed_count = length(failed_assays)
				)
				
			}, error = function(e) {
				processing_status("error")
				
				correction_log_data(list(
					corrected_count = 0,
					total_count = length(input$target_assays),
					failed_assays = paste("Error:", e$message),
					suffix = input$combat_assay_suffix,
					strategy = selector$correction_strategy(),
					batch_factors = selector$batch_factors(),
					updated_expsets_count = 0,
					details = paste("❌ Error:", e$message),
					timestamp = Sys.time()
				))
				
				stop(e)
			})
		}
		
		# ✅ AUTO-RUN:  Observer that triggers when conditions are met
		observeEvent(list(
			selector$batch_factors(),
			selector$combat_model(),
			selector$correction_strategy(),
			selector$par_prior(),
			input$target_assays,
			input$combat_assay_suffix
		), {
			# Only run if auto-run is enabled
			if (! isTRUE(input$auto_run_multi)) {
				message("⏸️ Auto-run disabled - skipping")
				return()
			}
			
			# Only run if all conditions are met
			if (!ready_to_run()) {
				message("⏸️ Not ready to run - missing inputs")
				return()
			}
			
			message("🚀 Auto-run triggered - processing multi-assay correction")
			
			showNotification(
				"Auto-running batch correction for multiple assays.. .",
				type = "message",
				duration = 3,
				id = "multi_auto_run"
			)
			
			run_multi_correction()
			
		}, ignoreInit = TRUE, ignoreNULL = TRUE)
		
		# ✅ MANUAL RUN: Button handler
		observeEvent(input$apply_multi_batch_correction, {
			message("🎯 Manual run triggered")
			
			showNotification(
				"Starting batch correction for multiple assays...",
				type = "message",
				duration = NULL,
				id = "batch_multi_progress"
			)
			
			result <- run_multi_correction()
			
			removeNotification("batch_multi_progress")
			
			# Show final notification
			if (result$success) {
				showNotification(
					sprintf("✅ Corrected %d/%d assays", result$corrected_count, result$total_count),
					type = "message",
					duration = 10
				)
			} else {
				showNotification(
					"❌ No assays were corrected.  Check the log for details.",
					type = "error",
					duration = 10
				)
			}
		})
		
		# ✅ REACTIVE EXPRESSION: For use in pipelines
		corrected_ExpSet_list <- reactive({
			# This can be called by other modules as a pipeline step
			if (isTRUE(input$auto_run_multi) && ready_to_run()) {
				# Auto-run mode:  return corrected list
				ExpSet_list()  # This will be updated by the observer
			} else {
				# Manual mode: return original until button is clicked
				ExpSet_list()
			}
		})
		
		# Status display
		output$multi_correction_status <- renderUI({
			log <- correction_log_data()
			status <- processing_status()
			
			# ✅ RUNNING STATE
			if (status == "running") {
				return(
					div(
						class = "alert alert-info",
						icon("spinner fa-spin"),
						strong(" Processing batch correction..."),
						p("Please wait while assays are being corrected.")
					)
				)
			}
			
			# ✅ ERROR STATE
			if (status == "error") {
				error_msg <- if (! is.null(log) && ! is.null(log$details)) {
					log$details
				} else {
					"An unknown error occurred"
				}
				
				return(
					div(
						class = "alert alert-danger",
						icon("exclamation-circle"),
						strong(" Error during batch correction"),
						p(error_msg),
						p(tags$small("Check the console for detailed error messages. "))
					)
				)
			}
			
			# ✅ NOT READY STATE - Show what's missing
			if (is.null(log)) {
				# Check each condition and report what's missing
				has_expset <- !is.null(ExpSet_list()) && length(ExpSet_list()) > 0
				has_batch <- !is.null(selector$batch_factors()) && length(selector$batch_factors()) > 0
				has_assays <- !is.null(input$target_assays) && length(input$target_assays) > 0
				has_suffix <- !is.null(input$combat_assay_suffix) && nchar(input$combat_assay_suffix) > 0
				
				# ✅ Ready to run
				if (has_expset && has_batch && has_assays && has_suffix) {
					return(
						div(
							class = "alert alert-info",
							icon("check-circle"),
							strong(" Ready to run batch correction"),
							tags$ul(
								tags$li(sprintf("%d assays selected", length(input$target_assays))),
								tags$li(sprintf("Batch factors: %s", paste(selector$batch_factors(), collapse = ", "))),
								tags$li(sprintf("Strategy: %s", selector$correction_strategy())),
								tags$li(sprintf("Suffix: %s", input$combat_assay_suffix))
							),
							if (isTRUE(input$auto_run_multi)) {
								p(
									icon("sync", style = "color: #337ab7;"),
									tags$em("Auto-run is enabled - will process automatically when settings change")
								)
							} else {
								p(
									icon("hand-pointer", style = "color: #f0ad4e;"),
									tags$em("Click 'Apply Batch Correction' button to start")
								)
							}
						)
					)
				}
				
				# ✅ Not ready - show what's missing
				missing_items <- c()
				
				if (!has_expset) {
					missing_items <- c(missing_items, "ExpressionSet data not loaded")
				}
				
				if (!has_batch) {
					missing_items <- c(missing_items, "No batch factors selected (configure in Batch Correction tab)")
				}
				
				if (!has_assays) {
					missing_items <- c(missing_items, "No target assays selected")
				}
				
				if (!has_suffix) {
					missing_items <- c(missing_items, "No assay suffix specified")
				}
				
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" Not ready to run - missing requirements: "),
						tags$ul(
							lapply(missing_items, function(item) {
								tags$li(icon("times", style = "color: #d9534f;"), " ", item)
							})
						),
						p(tags$small("Complete the requirements above to enable batch correction. "))
					)
				)
			}
			
			# ✅ COMPLETED STATES (existing code)
			
			# No assays corrected
			if (log$corrected_count == 0) {
				return(
					div(
						class = "alert alert-danger",
						icon("times-circle"),
						strong(" No assays were corrected"),
						if (length(log$failed_assays) > 0) {
							tagList(
								p("Reasons: "),
								tags$ul(
									lapply(log$failed_assays, function(x) {
										tags$li(icon("exclamation-circle"), " ", x)
									})
								),
								p(tags$small("Check the Correction Log below for detailed information."))
							)
						} else {
							p("No errors reported.  This may indicate a configuration issue.")
						}
					)
				)
			}
			
			# Partial success
			if (length(log$failed_assays) > 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(sprintf(" Partially completed: %d/%d assays corrected", 
													 log$corrected_count, log$total_count)),
						tags$ul(
							tags$li("Suffix: ", tags$code(log$suffix)),
							tags$li("Strategy: ", tags$code(log$strategy)),
							tags$li("Batch factors:  ", tags$code(paste(log$batch_factors, collapse = " × "))),
							tags$li("Updated ExpressionSets:  ", log$updated_expsets_count),
							tags$li("Completed: ", format(log$timestamp, "%Y-%m-%d %H:%M:%S"))
						),
						hr(),
						p(strong(icon("times-circle"), " Failed assays:")),
						tags$ul(
							lapply(log$failed_assays, function(x) {
								tags$li(tags$code(x))
							})
						),
						p(tags$small("Check the Correction Log below for detailed error messages."))
					)
				)
			}
			
			# Complete success
			div(
				class = "alert alert-success",
				icon("check-circle"),
				strong(sprintf(" Success!  %d/%d assays corrected", 
											 log$corrected_count, log$total_count)),
				tags$ul(
					tags$li("Suffix: ", tags$code(log$suffix)),
					tags$li("Strategy: ", tags$code(log$strategy)),
					tags$li("Batch factors: ", tags$code(paste(log$batch_factors, collapse = " × "))),
					tags$li("Updated ExpressionSets: ", log$updated_expsets_count),
					tags$li("Completed: ", format(log$timestamp, "%Y-%m-%d %H:%M:%S"))
				),
				p(
					icon("download", style = "color: #5cb85c;"),
					tags$em("Corrected ExpressionSets are ready for export.")
				)
			)
		})
		
		
		
		# Detailed correction log
		output$correction_log <- renderUI({
			log <- correction_log_data()
			
			if (is.null(log) || length(log$details) == 0) {
				return(NULL)
			}
			
			box(
				title = "Correction Log",
				width = 12,
				#status = "primary",
				#solidHeader = FALSE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				tags$pre(
					style = "max-height: 400px; overflow-y: auto; background-color: #f5f5f5; padding: 10px;",
					paste(log$details, collapse = "\n")
				)
			)
		})
		
		# ✅ RETURN:  Expose reactive values for pipeline use
		return(list(
			corrected_ExpSet_list = corrected_ExpSet_list,
			ready_to_run = ready_to_run,
			processing_status = reactive(processing_status()),
			correction_log = reactive(correction_log_data()),
			run_correction = run_multi_correction  # Expose function for manual triggering
		))
	})
}
