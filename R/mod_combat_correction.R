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
				width = if (show_info) 11 else 12,
				uiOutput(ns("combat_selector_ui"))
			),
			if (show_info) {
				column(
					width = 1,
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
										helpText("Parametric is faster and works well for most cases.  Use non-parametric for non-normal data.")
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
		
		# ✅ Render batch factor selector with loading state and color coding
		# output$combat_selector_ui <- renderUI({
		# 	# Check if data is ready
		# 	if (!analysis_ready()) {
		# 		return(
		# 			tagList(
		# 				div(
		# 					style = "padding: 20px; text-align: center; background-color: #f8f9fa; border-radius: 4px;",
		# 					icon("spinner", class = "fa-spin fa-2x", style = "color: #337ab7;"),
		# 					h4(style = "margin-top: 15px; color: #6c757d;", "Analyzing batch factors..."),
		# 					p("Please wait while batch effect analysis completes.")
		# 				)
		# 			)
		# 		)
		# 	}
		# 	
		# 	req(eset())
		# 	
		# 	all_columns <- colnames(Biobase::pData(eset()))
		# 	safe_factors <- safe_batch_factors()
		# 	unsafe_factors <- unsafe_batch_factors()
		# 	
		# 	# If no columns available
		# 	if (length(all_columns) == 0) {
		# 		return(
		# 			div(
		# 				class = "alert alert-warning",
		# 				icon("exclamation-triangle"),
		# 				strong(" No metadata columns available"),
		# 				p("Cannot identify batch factors without sample metadata.")
		# 			)
		# 		)
		# 	}
		# 	
		# 	default_selection <- if (length(safe_factors) > 0) {
		# 		safe_factors
		# 	} else {
		# 		NULL
		# 	}
		# 	
		# 	# ✅ Categorize factors
		# 	safe_list <- intersect(all_columns, safe_factors)
		# 	unsafe_list <- intersect(all_columns, unsafe_factors)
		# 	unknown_list <- setdiff(all_columns, c(safe_list, unsafe_list))
		# 	
		# 	# ✅ Build choice list dynamically (only include non-empty groups)
		# 	choice_groups <- list()
		# 	
		# 	if (length(safe_list) > 0) {
		# 		choice_groups[["✓ Safe Factors (Recommended)"]] <- setNames(
		# 			safe_list,
		# 			paste0("✓ ", safe_list)
		# 		)
		# 	}
		# 	
		# 	if (length(unsafe_list) > 0) {
		# 		choice_groups[["⚠ Confounded Factors (Caution)"]] <- setNames(
		# 			unsafe_list,
		# 			paste0("⚠ ", unsafe_list)
		# 		)
		# 	}
		# 	
		# 	if (length(unknown_list) > 0) {
		# 		choice_groups[["○ Other Factors"]] <- setNames(
		# 			unknown_list,
		# 			paste0("○ ", unknown_list)
		# 		)
		# 	}
		# 	
		# 	# ✅ Fallback:  if no groups created, use plain list
		# 	if (length(choice_groups) == 0) {
		# 		choice_groups <- setNames(all_columns, all_columns)
		# 	}
		# 	
		# 	tagList(
		# 		# ✅ Use shinyWidgets:: pickerInput for better styling
		# 		shinyWidgets::pickerInput(
		# 			ns("batch_factors"),
		# 			"Select Batch Factor(s):",
		# 			choices = choice_groups,
		# 			selected = default_selection,
		# 			multiple = TRUE,
		# 			options = pickerOptions(
		# 				actionsBox = TRUE,
		# 				selectedTextFormat = "count > 2",
		# 				liveSearch = TRUE,
		# 				style = "btn-default",
		# 				title = "Choose batch factors..."
		# 			),
		# 			width = "100%"
		# 		),
		# 		
		# 		# ✅ Status indicator
		# 		if (length(safe_factors) > 0) {
		# 			div(
		# 				style = "margin-top: 5px; padding: 5px 10px; background-color: #d4edda; border-left: 3px solid #28a745; border-radius: 3px;",
		# 				icon("check-circle", style = "color: #28a745;"),
		# 				strong(sprintf(" %d safe factor(s) available", length(safe_factors)), style = "color: #155724; margin-left: 5px;")
		# 			)
		# 		} else if (length(unsafe_factors) > 0) {
		# 			div(
		# 				style = "margin-top:  5px; padding: 5px 10px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 3px;",
		# 				icon("exclamation-triangle", style = "color: #ffc107;"),
		# 				strong(" No safe factors identified (confounded factors available)", style = "color: #856404; margin-left: 5px;")
		# 			)
		# 		} else {
		# 			div(
		# 				style = "margin-top:  5px; padding: 5px 10px; background-color: #d1ecf1; border-left:  3px solid #17a2b8; border-radius: 3px;",
		# 				icon("info-circle", style = "color: #17a2b8;"),
		# 				strong(" Batch analysis not yet completed", style = "color: #0c5460; margin-left: 5px;")
		# 			)
		# 		},
		# 		
		# 		helpText("Select one or more batch factors to correct using ComBat")
		# 	)
		# })
		
		# ✅ Render batch factor selector with loading state and color coding
		# output$combat_selector_ui <- renderUI({
		# 	# Check if data is ready
		# 	if (!analysis_ready()) {
		# 		return(
		# 			tagList(
		# 				div(
		# 					style = "padding: 20px; text-align: center; background-color: #f8f9fa; border-radius: 4px;",
		# 					icon("spinner", class = "fa-spin fa-2x", style = "color: #337ab7;"),
		# 					h4(style = "margin-top: 15px; color: #6c757d;", "Analyzing batch factors..."),
		# 					p("Please wait while batch effect analysis completes.")
		# 				)
		# 			)
		# 		)
		# 	}
		# 	
		# 	req(eset())
		# 	
		# 	all_columns <- colnames(Biobase::pData(eset()))
		# 	safe_factors <- safe_batch_factors()
		# 	unsafe_factors <- unsafe_batch_factors()
		# 	
		# 	default_selection <- if (length(safe_factors) > 0) {
		# 		safe_factors
		# 	} else {
		# 		NULL
		# 	}
		# 	
		# 	# ✅ Create color-coded choice list
		# 	choice_list <- setNames(all_columns, all_columns)
		# 	
		# 	# Build optgroup structure for color coding
		# 	safe_list <- intersect(all_columns, safe_factors)
		# 	unsafe_list <- intersect(all_columns, unsafe_factors)
		# 	unknown_list <- setdiff(all_columns, c(safe_list, unsafe_list))
		# 	
		# 	tagList(
		# 		# ✅ Use shinyWidgets:: pickerInput for better styling
		# 		shinyWidgets::pickerInput(
		# 			ns("batch_factors"),
		# 			"Select Batch Factor(s):",
		# 			choices = list(
		# 				"✓ Safe Factors (Recommended)" = setNames(safe_list, paste0("✓ ", safe_list)),
		# 				"⚠ Confounded Factors (Caution)" = setNames(unsafe_list, paste0("⚠ ", unsafe_list)),
		# 				"○ Other Factors" = setNames(unknown_list, paste0("○ ", unknown_list))
		# 			),
		# 			selected = default_selection,
		# 			multiple = TRUE,
		# 			options = pickerOptions(
		# 				actionsBox = TRUE,
		# 				selectedTextFormat = "count > 2",
		# 				liveSearch = TRUE,
		# 				style = "btn-default",
		# 				title = "Choose batch factors..."
		# 			),
		# 			width = "100%"
		# 		),
		# 		
		# 		# ✅ Status indicator
		# 		if (length(safe_factors) > 0) {
		# 			div(
		# 				style = "margin-top: 5px; padding: 5px 10px; background-color: #d4edda; border-left: 3px solid #28a745; border-radius: 3px;",
		# 				icon("check-circle", style = "color: #28a745;"),
		# 				strong(sprintf(" %d safe factor(s) available", length(safe_factors)), style = "color: #155724; margin-left: 5px;")
		# 			)
		# 		} else {
		# 			div(
		# 				style = "margin-top:  5px; padding: 5px 10px; background-color: #fff3cd; border-left: 3px solid #ffc107; border-radius: 3px;",
		# 				icon("exclamation-triangle", style = "color: #ffc107;"),
		# 				strong(" No safe factors identified", style = "color: #856404; margin-left: 5px;")
		# 			)
		# 		},
		# 		
		# 		helpText("Select one or more batch factors to correct using ComBat")
		# 	)
		# })
		
		# Render batch factor selector
		# output$combat_selector_ui <- renderUI({
		# 	req(eset())
		# 	
		# 	all_columns <- colnames(Biobase::pData(eset()))
		# 	safe_factors <- safe_batch_factors()
		# 	
		# 	default_selection <- if (length(safe_factors) > 0) {
		# 		safe_factors
		# 	} else {
		# 		NULL
		# 	}
		# 	
		# 	tagList(
		# 		selectInput(
		# 			ns("batch_factors"),
		# 			"Select Batch Factor(s):",
		# 			choices = all_columns,
		# 			selected = default_selection,
		# 			multiple = TRUE,
		# 			width = "100%"
		# 		),
		# 		helpText("Select one or more batch factors to correct using ComBat")
		# 	)
		# })
		
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
		
		# Return selected values
		return(list(
			batch_factors = reactive(input$batch_factors),
			par_prior = reactive(as.logical(input$par_prior)),
			combat_model = reactive(input$combat_model),
			correction_strategy = reactive(input$correction_strategy),
			safe_factors = safe_batch_factors
		))
	})
}

# RUN CORRECTION ####



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
		
		# fluidRow(
		# 	# Batch Factor Selection (separate module)
		# 	mod_combat_correction_selector_ui(ns("selector"), debug = debug)
		# ),
		
		fluidRow(
			column(12,
						 tabsetPanel(
						 	
						 	## Single Tab ####
						 	tabPanel(
						 		'Single',
						 		
						 		# Data selector
						 		
						 		
						 		# ComBat execution box
						 		fluidRow(
						 			box(
						 				title = "ComBat Batch Correction",
						 				width = 12,
						 				status = "success",
						 				solidHeader = TRUE,
						 				collapsible = TRUE,
						 				
						 				fluidRow(
						 					column(
						 						width = 12,
						 						align = "center",
						 						br(),
						 						actionButton(
						 							ns("run_combat"),
						 							"Run ComBat Correction",
						 							icon = icon("play"),
						 							class = "btn-success btn-lg",
						 							style = "width: 50%;"
						 						),
						 						br(), br()
						 					)
						 				),
						 				
						 				uiOutput(ns("correction_status"))
						 			)
						 		),
						 		
						 		# Correction Results
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
						 		)
						 	),
						 	
						 	## Multiple Tab ####
						 	tabPanel(
						 		'Multiple',
						 		
						 		fluidRow(
						 			box(
						 				title = "Batch Correction for Multiple Assays",
						 				width = 12,
						 				status = "info",
						 				solidHeader = TRUE,
						 				collapsible = TRUE,
						 				collapsed = FALSE,
						 				
						 				p("Apply the selected batch correction settings to multiple assays at once. "),
						 				
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
						 						helpText("Select which assays to apply batch correction to.")
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
						 					)
						 				),
						 				
						 				hr(),
						 				
						 				actionButton(
						 					ns("apply_multi_batch_correction"),
						 					"Apply Batch Correction to All Selected Assays",
						 					icon = icon("magic"),
						 					class = "btn-success btn-lg",
						 					style = "width: 100%;"
						 				),
						 				
						 				br(), br(),
						 				
						 				uiOutput(ns("multi_correction_status"))
						 			)
						 		)
						 	),
						 	
						 	## Batch Test Tab ####
						 	tabPanel(
						 		'Batch Test',
						 		mod_batch_combined_analysis_ui("batch_analysis", debug = debug)
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
																				 ExpSet_list = reactive(NULL),
																				 update_ExpSet_list = NULL,
																				 combined_results = reactive(NULL),
																				 selector = NULL,
																				 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# Debug UI
		output$debug_ui <- renderUI({
			if (isTRUE(debug)) {
				tagList(
					actionButton(ns("debug"), "Debug", icon = icon("bug"), class = "btn-warning btn-sm"),
					hr()
				)
			}
		})
		
		if (isTRUE(debug)) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - ComBat Correction Module")
				message("═══════════════════════════════════════════════")
				browser()
			})
		}
		
		# ✅ Call selector module
		# selector <- mod_combat_correction_selector_server(
		# 	"selector",
		# 	eset = eset,
		# 	combined_results = combined_results,
		# 	debug = debug
		# )
		
		# Store corrected ExpressionSet
		corrected_eset <- reactiveVal(NULL)
		
		# ✅ Helper reactive:  plot batch factors (adds ComBat column)
		plot_batch_factors <- reactive({
			req(selector$batch_factors())
			c(selector$batch_factors(), 'ComBat')
		})
		
		# ✅ Helper reactive: all available columns
		all_columns <- reactive({
			req(eset())
			colnames(Biobase::pData(eset()))
		})
		
		# ✅ Run ComBat correction (Single tab)
		observeEvent(input$run_combat, {
			req(eset())
			req(selector$batch_factors())
			req(sample_group_column())
			
			batch_factors <- selector$batch_factors()
			combat_model <- selector$combat_model()
			
			if (length(batch_factors) == 0) {
				showNotification("⚠️ Please select at least one batch factor", type = "warning", duration = 5)
				return()
			}
			
			# Validation warning for confounded factors with null model
			if (combat_model == "null") {
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
								title = tags$span(icon("exclamation-triangle"), " Warning:  Confounding Detected"),
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
			
			run_combat_correction()
		})
		
		observeEvent(input$proceed_combat, {
			removeModal()
			run_combat_correction()
		})
		
		observeEvent(input$cancel_combat, {
			removeModal()
		})
		
		# ✅ ComBat correction logic
		run_combat_correction <- function() {
			showNotification("Running ComBat correction...", type = "message", duration = NULL, id = "combat_progress")
			
			tryCatch({
				ExpSet <- eset()
				batch_factors <- selector$batch_factors()
				combat_model <- selector$combat_model()
				sample_group <- sample_group_column()
				par_prior <- selector$par_prior()
				
				strategy <- if (length(batch_factors) > 1 && ! is.null(selector$correction_strategy())) {
					selector$correction_strategy()
				} else {
					"single"
				}
				
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
				} else {
					modcombat <- model.matrix(~ as.factor(meta[[sample_group]]))
					model_description <- paste0("Preserve model (~", sample_group, ")")
				}
				
				combat_column_value <- NULL
				
				# Apply correction based on strategy
				if (strategy == "combined") {
					message("Using COMBINED strategy for ", length(batch_factors), " factors")
					
					batch_data <- lapply(batch_factors, function(f) as.factor(meta[[f]]))
					combined_batch <- do.call(interaction, c(batch_data, list(drop = TRUE, sep = "_")))
					combat_column_value <- as.character(combined_batch)
					
					batch_table <- table(combined_batch)
					single_sample <- names(batch_table)[batch_table == 1]
					if (length(single_sample) > 0) {
						stop("Cannot use combined strategy:  ", length(single_sample), 
								 " batch combination(s) have only 1 sample.")
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
						model = model_description
					)
					
				} else if (strategy == "sequential") {
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
						batch <- meta[[batch_factor]]
						
						corrected_data <- sva::ComBat(
							dat = corrected_data,
							batch = batch,
							mod = modcombat,
							par.prior = par_prior
						)
						
						correction_log$corrections[[batch_factor]] <- list(order = i)
					}
					
					correction_log$model <- model_description
					
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
					
					correction_log <- list(
						method = "single",
						batch_factor = batch_factor,
						model = model_description
					)
				}
				
				# Add ComBat column to metadata
				if (! is.null(combat_column_value)) {
					meta$ComBat <- combat_column_value
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
				Biobase::notes(corrected_ExpSet) <- notes
				
				corrected_eset(corrected_ExpSet)
				
				removeNotification("combat_progress")
				showNotification("✅ ComBat correction complete!", type = "message", duration = 10)
				
			}, error = function(e) {
				removeNotification("combat_progress")
				showNotification(paste("❌ ComBat failed:", e$message), type = "error", duration = 15)
			})
		}
		
		# Correction status
		output$correction_status <- renderUI({
			if (! is.null(corrected_eset())) {
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" Correction complete! "),
					p("Corrected ExpressionSet available.")
				)
			}
		})
		
		# Correction summary
		output$correction_summary <- renderPrint({
			req(corrected_eset())
			
			ExpSet_corrected <- corrected_eset()
			notes <- Biobase::notes(ExpSet_corrected)
			combat_info <- notes$combat_correction
			
			cat("═══════════════════════════════════════════════\n")
			cat("COMBAT CORRECTION SUMMARY\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			if (! is.null(combat_info)) {
				cat("Model used:", combat_info$model, "\n")
				cat("Corrected for", length(combat_info$batch_factors), "batch factor(s)\n\n")
			}
			
			cat("Dimensions:", nrow(ExpSet_corrected), "features ×", ncol(ExpSet_corrected), "samples\n")
		})
		
		# ✅ Multiple assays correction
		observe({
			req(ExpSet_list())
			choices <- get_expset_assay_names(ExpSet_list())
			updatePickerInput(session, "target_assays", choices = choices)
		})
		
		output$target_assays_text <- renderText({
			req(input$target_assays)
			paste(input$target_assays, collapse = "<br>")
		})
		
		output$target_assays_combat_text <- renderText({
			req(input$target_assays)
			req(input$combat_assay_suffix)
			paste(paste0(input$target_assays, input$combat_assay_suffix), collapse = "<br>")
		})
		
		observeEvent(input$apply_multi_batch_correction, {
			# ...  your existing multi-correction code
			# Replace input$batch_factors with selector$batch_factors()
			# Replace input$par_prior with selector$par_prior()
			# Replace input$combat_model with selector$combat_model()
			# Replace input$correction_strategy with selector$correction_strategy()
		})
		
		# Return values
		return(list(
			corrected_eset = corrected_eset,
			selected_batch_factors = selector$batch_factors,
			plot_batch_factors = plot_batch_factors,
			all_columns = all_columns,
			safe_factors = selector$safe_factors
		))
	})
}

