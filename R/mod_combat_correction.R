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
		# Batch Correction ####
		fluidRow(
			box(
				title = "ComBat Batch Correction",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				uiOutput(ns("debug_ui")),
				uiOutput(ns("combat_selector_ui")),
				column(
					width = 12,
					h4("Run Correction"),
					actionButton(
						ns("run_combat"),
						"Run ComBat Correction",
						icon = icon("play"),
						class = "btn-success btn-lg"
					),
					br(), br(),
					uiOutput(ns("correction_status"))
				),
				
				p("Select batch factors to correct using ComBat."),
				p(strong("Only 'safe' factors are shown:"), "Strong batch effect (p < 0.05) and NOT confounded with sample groups (p > 0.05)."),
				
				#uiOutput(ns("debug_ui")),
				
				#uiOutput(ns("combat_selector_ui")),
				
				hr(),
				
				fluidRow(
					column(
						width = 6,
						h4("ComBat Settings"),
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
					),
				
				# Add help modal
				bsModal(
					ns("model_help_modal"),
					"ComBat Model Selection Guide",
					trigger = ns("show_model_help"),
					size = "large",
					
					h4(icon("info-circle"), " Understanding ComBat Models"),
					
					hr(),
					
					h5(tags$span(style = "color: #2c3e50;", icon("formula"), " Null Model (~1)")),
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
				
				
				)
			),
		
		# Correction Results ####
		fluidRow(
			box(
				title = "Correction Results",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				verbatimTextOutput(ns("correction_summary")),
				
				hr(),
				
				h4("Download Corrected Data"),
				downloadButton(ns("download_corrected_eset"), "Download Corrected ExpressionSet (.rds)"),
				downloadButton(ns("download_corrected_csv"), "Download Corrected Matrix (.csv)")
			)
		),
	
		
		# Update ####
		fluidRow(
			box(
				title = "Update ExpressionSet List",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = FALSE,
				
				p("Add the ComBat-corrected data to your ExpressionSet list as a new assayData element."),
				
				fluidRow(
					column(
						width = 6,
						textInput(
							ns("combat_assay_name"),
							"New assayData Name:",
							value = "",
							placeholder = "e.g., exprs_combat or combat_corrected"
						),
						helpText("This will be added to the selected ExpressionSet")
					),
					column(
						width = 6,
						checkboxInput(
							ns("add_rc_version"),
							"Also add row-centered version",
							value = TRUE
						),
						helpText("Adds a '_RC' version with row centering")
					)
				),
				
				fluidRow(
					column(
						width = 12,
						actionButton(
							ns("update_expset_list"),
							"Update ExpressionSet List",
							icon = icon("check-circle"),
							class = "btn-success btn-lg",
							style = "width: 100%;"
						)
					)
				),
				
				br(),
				
				uiOutput(ns("update_status"))
			)
		)
	)
}


#' ComBat Correction Module - Server
#'
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
																				 combined_results = reactive(NULL),
																				 ExpSet_list = reactive(NULL),
																				 selected_expset_name = reactive(NULL),
																				 update_expset_list = NULL,
																				 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		# Debug UI - MUST be inside moduleServer
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
		
		# ...  rest of your existing code ...
		
		# Reactive for corrected ExpressionSet
		corrected_eset <- reactiveVal(NULL)
		
		# Auto-generate suggested name for ComBat assay - MUST be inside moduleServer
		observe({
			req(input$batch_factors)
			req(eset())
			
			# Get current assay name
			current_assay <- Biobase::assayDataElementNames(eset())[1]
			
			# Generate suggested name
			suggested_name <- paste0(current_assay, "_combat")
			
			updateTextInput(session, "combat_assay_name", value = suggested_name)
		})
		
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
				# Status message
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
				
				helpText(
					icon("lightbulb"),
					strong("Multiple factors:"),
					"ComBat can correct for multiple batch factors simultaneously.",
					br(),
					strong("Order matters:"),
					"Factors are corrected in the order selected (most important first)."
				)
			)
		})
		
		# Reactive for selected batch factors
		selected_batch_factors <- reactive({
			input$batch_factors
		})
		
		# ...  your existing ComBat correction code ...
		
		# Update ExpressionSet list
		update_success <- reactiveVal(FALSE)
		
		observeEvent(input$update_expset_list, {
			req(corrected_eset())
			req(input$combat_assay_name)
			
			# Check if ExpSet_list functionality is available
			if (is.null(ExpSet_list) || is.null(update_expset_list)) {
				showNotification(
					"⚠️ ExpSet list update not available in this context",
					type = "warning",
					duration = 5
				)
				return()
			}
			
			req(ExpSet_list())
			req(selected_expset_name())
			
			assay_name <- input$combat_assay_name
			
			# Validate name
			if (assay_name == "" || grepl("^\\s*$", assay_name)) {
				showNotification("⚠️ Please provide a valid assay name", type = "warning", duration = 5)
				return()
			}
			
			showNotification("Updating ExpressionSet list.. .", id = "update_progress", duration = NULL, type = "message")
			
			tryCatch({
				# Get the corrected ExpressionSet
				corrected_ExpSet <- corrected_eset()
				corrected_data <- Biobase::exprs(corrected_ExpSet)
				
				# Get the original ExpressionSet from the list
				expset_list <- ExpSet_list()
				selected_name <- selected_expset_name()
				
				if (! selected_name %in% names(expset_list)) {
					stop("Selected ExpressionSet not found in list")
				}
				
				original_ExpSet <- expset_list[[selected_name]]
				
				# Add ComBat-corrected data as new assayData element
				Biobase::assayDataElement(original_ExpSet, assay_name) <- corrected_data
				
				added_names <- assay_name
				
				# Optionally add row-centered version
				if (isTRUE(input$add_rc_version)) {
					rc_name <- paste0(assay_name, "_RC")
					corrected_data_rc <- row_scale_function(corrected_data)
					Biobase::assayDataElement(original_ExpSet, rc_name) <- corrected_data_rc
					added_names <- c(added_names, rc_name)
				}
				
				# Update the ComBat column in pData if it was created
				corrected_meta <- Biobase::pData(corrected_ExpSet)
				original_meta <- Biobase::pData(original_ExpSet)
				
				# Add any new columns from correction (e.g., ComBat batch variable)
				new_cols <- setdiff(colnames(corrected_meta), colnames(original_meta))
				if (length(new_cols) > 0) {
					for (col in new_cols) {
						original_meta[[col]] <- corrected_meta[[col]]
					}
					Biobase::pData(original_ExpSet) <- original_meta
				}
				
				# Update experimentData with correction info
				exp_data <- Biobase::experimentData(original_ExpSet)
				if (is.null(exp_data@other$combat_corrections)) {
					exp_data@other$combat_corrections <- list()
				}
				
				exp_data@other$combat_corrections[[assay_name]] <- list(
					batch_factors = input$batch_factors,
					model = input$combat_model,
					par_prior = input$par_prior,
					correction_date = Sys.time(),
					assay_names = added_names
				)
				
				Biobase::experimentData(original_ExpSet) <- exp_data
				
				# Update the list
				expset_list[[selected_name]] <- original_ExpSet
				
				# Call the update function to update the main ExpSet_list
				if (! is.null(update_expset_list)) {
					update_expset_list(expset_list)
				}
				
				removeNotification("update_progress")
				
				update_success(TRUE)
				
				showNotification(
					HTML(paste0(
						"✅ ExpressionSet updated successfully! <br>",
						"Added assayData elements:<br>",
						paste("  •", added_names, collapse = "<br>")
					)),
					type = "message",
					duration = 10
				)
				
			}, error = function(e) {
				removeNotification("update_progress")
				update_success(FALSE)
				showNotification(
					paste("❌ Failed to update ExpressionSet:", e$message),
					type = "error",
					duration = 10
				)
				message("Update error: ", e$message)
			})
		})
		
		# Show update status - MUST be inside moduleServer
		output$update_status <- renderUI({
			if (update_success()) {
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" ExpressionSet successfully updated! "),
					p("The ComBat-corrected data has been added to your ExpressionSet list. "),
					p("You can now:"),
					tags$ul(
						tags$li("View it in the 'ExpSet Viewer' tab"),
						tags$li("Use it in downstream analyses"),
						tags$li("Save the entire list using the export function")
					)
				)
			}
		})
		
		# Return values
		return(list(
			corrected_eset = corrected_eset,
			selected_batch_factors = selected_batch_factors
		))
	})
}


#' ComBat Batch Correction Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param sample_group_column Reactive character.  Sample grouping column (to preserve)
#' @param combined_results Reactive data.frame. Results from combined batch analysis
#' @param debug Enable debug mode
#' @export
# mod_combat_correction_server <- function(id,
# 																				 eset,
# 																				 sample_group_column,
# 																				 combined_results = reactive(NULL),
# 																				 ExpSet_list = reactive(NULL),
# 																				 selected_expset_name = reactive(NULL),
# 																				 update_expset_list = NULL) {
# 	moduleServer(id, function(input, output, session) {
# 		
# 		# Render debug button
# 		output$debug_ui <- renderUI({
# 			if (debug) {
# 				tagList(
# 					actionButton(
# 						session$ns("debug"),
# 						"Debug",
# 						icon = icon("bug"),
# 						class = "btn-warning btn-sm"
# 					),
# 					hr()
# 				)
# 			}
# 		})
# 		
# 		# Debug observer
# 		if (debug) {
# 			observeEvent(input$debug, {
# 				message("\n═══════════════════════════════════════════════")
# 				message("🔍 DEBUG MODE - ComBat Correction Module")
# 				message("═══════════════════════════════════════════════")
# 				message("\nAvailable objects:")
# 				message("  • eset() - Selected ExpressionSet")
# 				message("  • sample_group_column() - Sample grouping")
# 				message("  • safe_batch_factors() - Safe factors to correct")
# 				message("  • corrected_eset() - Corrected ExpressionSet")
# 				message("\nUseful commands:")
# 				message("  safe_batch_factors()")
# 				message("  input$batch_factor")
# 				message("  str(corrected_eset())")
# 				message("═══════════════════════════════════════════════\n")
# 				browser()
# 			})
# 		}
# 		
# 		# Identify safe batch factors ####
# 		# safe_batch_factors <- reactive({
# 		# 	req(combined_results())
# 		# 
# 		# 	df <- combined_results()
# 		# 
# 		# 	# Filter for: ANOVA p < 0.05 (batch effect) AND Fisher p > 0.05 (not confounded)
# 		# 	safe <- df %>%
# 		# 		filter(ANOVA_p_value < 0.05, Fisher_p_value > 0.05) %>%
# 		# 		arrange(ANOVA_p_value) %>%  # Sort by strongest batch effect
# 		# 		pull(Batch_Column)
# 		# 
# 		# 	as.character(safe)
# 		# })
# 		
# 		# Identify safe batch factors
# 		safe_batch_factors <- reactive({
# 			req(combined_results())
# 
# 			df <- combined_results()
# 
# 			# Dynamically get the batch effect column name
# 			batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
# 
# 			if (is.na(batch_col_name)) {
# 				warning("Could not find batch effect p-value column")
# 				return(character(0))
# 			}
# 
# 			# Filter for: Batch p < 0.05 (batch effect) AND Fisher p > 0.05 (not confounded)
# 			safe <- df %>%
# 				rename(batch_p = !!sym(batch_col_name)) %>%  # Rename to generic name
# 				filter(batch_p < 0.05, Fisher_p_value > 0.05) %>%
# 				arrange(batch_p) %>%  # Sort by strongest batch effect
# 				pull(Batch_Column)
# 
# 			as.character(safe)
# 		})
# 		
# 		# Render batch factor selector ####
# 		
# 		
# 		# output$combat_selector_ui <- renderUI({
# 		# 	ns <- session$ns
# 		# 	
# 		# 	safe_factors <- safe_batch_factors()
# 		# 	
# 		# 	if (is.null(safe_factors) || length(safe_factors) == 0) {
# 		# 		return(
# 		# 			div(
# 		# 				class = "alert alert-warning",
# 		# 				icon("exclamation-triangle"),
# 		# 				strong(" No safe batch factors identified"),
# 		# 				p("All tested batch factors are either:"),
# 		# 				tags$ul(
# 		# 					tags$li("Not significant (no batch effect), or"),
# 		# 					tags$li("Confounded with sample groups (unsafe to correct)")
# 		# 				),
# 		# 				p("Run the Combined Batch Analysis first, or adjust your batch column selection.")
# 		# 			)
# 		# 		)
# 		# 	}
# 		# 	
# 		# 	tagList(
# 		# 		div(
# 		# 			class = "alert alert-success",
# 		# 			icon("check-circle"),
# 		# 			strong(sprintf(" %d safe batch factor(s) identified", length(safe_factors)))
# 		# 		),
# 		# 		
# 		# 		selectInput(
# 		# 			ns("batch_factor"),
# 		# 			"Select Batch Factor to Correct:",
# 		# 			choices = safe_factors,
# 		# 			selected = safe_factors[1],  # Default to strongest effect
# 		# 		),
# 		# 		
# 		# 		helpText(
# 		# 			"Listed in order of batch effect strength (strongest first).  ",
# 		# 			"ComBat will correct for this factor while preserving biological variation."
# 		# 		)
# 		# 	)
# 		# })
# 		
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
				# Status message
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

				helpText(
					icon("lightbulb"),
					strong("Multiple factors:"),
					"ComBat can correct for multiple batch factors simultaneously.",
					br(),
					strong("Order matters:"),
					"Factors are corrected in the order selected (most important first)."
				)
			)
		})
# 		
# 		# Add safety information display
# 		output$factor_safety_info <- renderUI({
# 			req(input$batch_factors)
# 			
# 			selected <- input$batch_factors
# 			safe <- safe_batch_factors()
# 			
# 			if (length(selected) == 0) return(NULL)
# 			
# 			# Categorize selected factors
# 			safe_selected <- intersect(selected, safe)
# 			unsafe_selected <- setdiff(selected, safe)
# 			
# 			tagList(
# 				if (length(safe_selected) > 0) {
# 					div(
# 						style = "background-color: #d4edda; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
# 						strong(icon("check-circle", class = "text-success"), " Safe factors:"),
# 						tags$ul(
# 							lapply(safe_selected, function(x) tags$li(x))
# 						)
# 					)
# 				},
# 				
# 				if (length(unsafe_selected) > 0) {
# 					div(
# 						style = "background-color: #fff3cd; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
# 						strong(icon("exclamation-triangle", class = "text-warning"), " Caution - Unverified factors:"),
# 						tags$ul(
# 							lapply(unsafe_selected, function(x) tags$li(x))
# 						),
# 						p(
# 							style = "margin-bottom: 0;",
# 							em("These factors were not identified as 'safe'.  They may be confounded with sample groups or have no significant batch effect.")
# 						)
# 					)
# 				}
# 			)
# 		})
# 		
# 		# Store corrected ExpressionSet
# 		corrected_eset <- reactiveVal(NULL)
# 		
# 		# Run ComBat correction ####
# 		# Run ComBat correction
# 		observeEvent(input$run_combat, {
# 			req(eset()) 
# 			req(input$batch_factors)
# 			req(sample_group_column())
# 			
# 			batch_factors <- input$batch_factors
# 			combat_model <- input$combat_model
# 			
# 			if (length(batch_factors) == 0) {
# 				showNotification("⚠️ Please select at least one batch factor", type = "warning", duration = 5)
# 				return()
# 			}
# 			
# 			# Validation warning for confounded factors with null model
# 			if (combat_model == "null") {
# 				# Check if any selected factors are confounded
# 				if (! is.null(combined_results())) {
# 					df <- combined_results()
# 					batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
# 					
# 					unsafe_factors <- df %>%
# 						rename(batch_p = !!sym(batch_col_name)) %>%
# 						filter(Batch_Column %in% batch_factors, Fisher_p_value < 0.05) %>%
# 						pull(Batch_Column)
# 					
# 					if (length(unsafe_factors) > 0) {
# 						showModal(
# 							modalDialog(
# 								title = tags$span(icon("exclamation-triangle"), " Warning: Confounding Detected"),
# 								size = "large",
# 								
# 								p(strong("The following batch factors are confounded with sample groups:")),
# 								tags$ul(
# 									lapply(unsafe_factors, function(x) tags$li(x))
# 								),
# 								
# 								p(style = "color: #e74c3c;",
# 									strong("Using the Null Model may remove real biological differences!")),
# 								
# 								p("Recommendations:"),
# 								tags$ol(
# 									tags$li("Switch to 'Preserve Sample Groups' model (safer), OR"),
# 									tags$li("Only correct factors with Fisher p > 0.05, OR"),
# 									tags$li("Proceed with caution if you understand the risks")
# 								),
# 								
# 								footer = tagList(
# 									actionButton(ns("cancel_combat"), "Cancel", class = "btn-default"),
# 									actionButton(ns("proceed_combat"), "Proceed Anyway", class = "btn-danger")
# 								)
# 							)
# 						)
# 						return()
# 					}
# 				}
# 			}
# 			
# 			# If validation passed or user clicked proceed, run ComBat
# 			run_combat_correction()
# 		})
# 		
# 		# Handle proceed button from warning modal
# 		observeEvent(input$proceed_combat, {
# 			removeModal()
# 			run_combat_correction()
# 		})
# 		
# 		# Handle cancel button
# 		observeEvent(input$cancel_combat, {
# 			removeModal()
# 		})
# 		
# 		# Actual ComBat execution function
# 		run_combat_correction <- function() {
# 			showNotification("Running ComBat correction...", type = "message", duration = NULL, id = "combat_progress")
# 			
# 			tryCatch({
# 				ExpSet <- eset()
# 				batch_factors <- input$batch_factors
# 				combat_model <- input$combat_model
# 				sample_group <- sample_group_column()
# 				par_prior <- as.logical(input$par_prior)
# 				
# 				# Get data
# 				expr_data <- Biobase::exprs(ExpSet)
# 				meta <- Biobase::pData(ExpSet)
# 				
# 				# Validate
# 				missing_factors <- setdiff(batch_factors, colnames(meta))
# 				if (length(missing_factors) > 0) {
# 					stop("Batch factors not found in metadata: ", paste(missing_factors, collapse = ", "))
# 				}
# 				
# 				if (! sample_group %in% colnames(meta)) {
# 					stop("Sample group column not found in metadata")
# 				}
# 				
# 				# Create model matrix based on selection
# 				if (combat_model == "null") {
# 					modcombat <- model.matrix(~1, data = meta)
# 					model_description <- "Null model (~1) - Maximum batch removal"
# 					message("Using NULL model for ComBat")
# 				} else {
# 					modcombat <- model.matrix(~ as.factor(meta[[sample_group]]))
# 					model_description <- paste0("Preserve model (~", sample_group, ") - Protects biological groups")
# 					message("Using PRESERVE model for ComBat with sample group: ", sample_group)
# 				}
# 				
# 				# Sequential correction for multiple factors
# 				corrected_data <- expr_data
# 				correction_log <- list()
# 				
# 				for (i in seq_along(batch_factors)) {
# 					batch_factor <- batch_factors[i]
# 					
# 					message(sprintf("ComBat correction %d/%d: %s", i, length(batch_factors), batch_factor))
# 					
# 					# Get batch variable
# 					batch <- meta[[batch_factor]]
# 					
# 					# Run ComBat
# 					corrected_data <- sva::ComBat(
# 						dat = corrected_data,
# 						batch = batch,
# 						mod = modcombat,
# 						par.prior = par_prior
# 					)
# 					
# 					correction_log[[batch_factor]] <- list(
# 						order = i,
# 						n_groups = length(unique(batch)),
# 						model = model_description
# 					)
# 				}
# 				
# 				# Row center (matching original script)
# 				#corrected_data <- row_scale_function(corrected_data)
# 				
# 				# Create new ExpressionSet with corrected data
# 				corrected_ExpSet <- ExpSet
# 				Biobase::exprs(corrected_ExpSet) <- corrected_data
# 				
# 				# Add note to experimentData
# 				Biobase::notes(corrected_ExpSet) <- c(
# 					Biobase::notes(corrected_ExpSet),
# 					list(
# 						combat_correction = list(
# 							batch_factors = batch_factors,
# 							model = model_description,
# 							sample_group_preserved = if (combat_model == "preserve") sample_group else NA,
# 							row_centered = TRUE,
# 							par_prior = par_prior,
# 							correction_date = Sys.time(),
# 							correction_log = correction_log
# 						)
# 					)
# 				)
# 				
# 				# Store result
# 				corrected_eset(corrected_ExpSet)
# 				
# 				removeNotification("combat_progress")
# 				showNotification(
# 					sprintf("✅ ComBat correction complete!\nModel: %s\nCorrected for: %s", 
# 									model_description,
# 									paste(batch_factors, collapse = ", ")),
# 					type = "message", 
# 					duration = 10
# 				)
# 				
# 			}, error = function(e) {
# 				removeNotification("combat_progress")
# 				showNotification(paste("❌ ComBat failed:", e$message), type = "error", duration = 10)
# 				message("ComBat error: ", e$message)
# 			})
# 		}
# 		
# 		# Run ComBat correction
# 		# observeEvent(input$run_combat, {
# 		# 	req(eset())
# 		# 	req(input$batch_factors)
# 		# 	req(sample_group_column())
# 		# 	
# 		# 	batch_factors <- input$batch_factors
# 		# 	
# 		# 	if (length(batch_factors) == 0) {
# 		# 		showNotification("⚠️ Please select at least one batch factor", type = "warning", duration = 5)
# 		# 		return()
# 		# 	}
# 		# 	
# 		# 	# Show progress
# 		# 	showNotification("Running ComBat correction...", type = "message", duration = NULL, id = "combat_progress")
# 		# 	
# 		# 	tryCatch({
# 		# 		ExpSet <- eset()
# 		# 		sample_group <- sample_group_column()
# 		# 		par_prior <- as.logical(input$par_prior)
# 		# 		
# 		# 		# Get data
# 		# 		expr_data <- Biobase::exprs(ExpSet)
# 		# 		meta <- Biobase::pData(ExpSet)
# 		# 		
# 		# 		# Validate
# 		# 		missing_factors <- setdiff(batch_factors, colnames(meta))
# 		# 		if (length(missing_factors) > 0) {
# 		# 			stop("Batch factors not found in metadata: ", paste(missing_factors, collapse = ", "))
# 		# 		}
# 		# 		
# 		# 		if (!  sample_group %in% colnames(meta)) {
# 		# 			stop("Sample group column not found in metadata")
# 		# 		}
# 		# 		
# 		# 		# Sequential correction for multiple factors
# 		# 		corrected_data <- expr_data
# 		# 		correction_log <- list()
# 		# 		
# 		# 		for (i in seq_along(batch_factors)) {
# 		# 			batch_factor <- batch_factors[i]
# 		# 			
# 		# 			message(sprintf("ComBat correction %d/%d: %s", i, length(batch_factors), batch_factor))
# 		# 			
# 		# 			# Prepare batch and mod
# 		# 			batch <- meta[[batch_factor]]
# 		# 			mod <- model.matrix(~ as.factor(meta[[sample_group]]))
# 		# 			
# 		# 			# Run ComBat
# 		# 			corrected_data <- sva::ComBat(
# 		# 				dat = corrected_data,
# 		# 				batch = batch,
# 		# 				mod = mod,
# 		# 				par.prior = par_prior
# 		# 			)
# 		# 			
# 		# 			correction_log[[batch_factor]] <- list(
# 		# 				order = i,
# 		# 				n_groups = length(unique(batch))
# 		# 			)
# 		# 		}
# 		# 		
# 		# 		# Create new ExpressionSet with corrected data
# 		# 		corrected_ExpSet <- ExpSet
# 		# 		Biobase::exprs(corrected_ExpSet) <- corrected_data
# 		# 		
# 		# 		# Add note to experimentData
# 		# 		Biobase::notes(corrected_ExpSet) <- c(
# 		# 			Biobase::notes(corrected_ExpSet),
# 		# 			list(
# 		# 				combat_correction = list(
# 		# 					batch_factors = batch_factors,
# 		# 					preserved_group = sample_group,
# 		# 					par_prior = par_prior,
# 		# 					correction_date = Sys.time(),
# 		# 					correction_log = correction_log
# 		# 				)
# 		# 			)
# 		# 		)
# 		# 		
# 		# 		# Store result
# 		# 		corrected_eset(corrected_ExpSet)
# 		# 		
# 		# 		removeNotification("combat_progress")
# 		# 		showNotification(
# 		# 			sprintf("✅ ComBat correction complete! Corrected for %d factor(s): %s", 
# 		# 							length(batch_factors), 
# 		# 							paste(batch_factors, collapse = ", ")),
# 		# 			type = "message", 
# 		# 			duration = 8
# 		# 		)
# 		# 		
# 		# 	}, error = function(e) {
# 		# 		removeNotification("combat_progress")
# 		# 		showNotification(paste("❌ ComBat failed:", e$message), type = "error", duration = 10)
# 		# 		message("ComBat error: ", e$message)
# 		# 	})
# 		# })
# 		
# 		
# 		# observeEvent(input$run_combat, {
# 		# 	req(eset())
# 		# 	req(input$batch_factor)
# 		# 	req(sample_group_column())
# 		# 	
# 		# 	# Show progress
# 		# 	showNotification("Running ComBat correction.. .", type = "message", duration = NULL, id = "combat_progress")
# 		# 	
# 		# 	tryCatch({
# 		# 		ExpSet <- eset()
# 		# 		batch_factor <- input$batch_factor
# 		# 		sample_group <- sample_group_column()
# 		# 		par_prior <- as.logical(input$par_prior)
# 		# 		
# 		# 		# Get data
# 		# 		expr_data <- Biobase::exprs(ExpSet)
# 		# 		meta <- Biobase::pData(ExpSet)
# 		# 		
# 		# 		# Validate
# 		# 		if (! batch_factor %in% colnames(meta)) {
# 		# 			stop("Batch factor not found in metadata")
# 		# 		}
# 		# 		
# 		# 		if (! sample_group %in% colnames(meta)) {
# 		# 			stop("Sample group column not found in metadata")
# 		# 		}
# 		# 		
# 		# 		# Prepare batch and mod
# 		# 		batch <- meta[[batch_factor]]
# 		# 		mod <- model.matrix(~ as.factor(meta[[sample_group]]))
# 		# 		
# 		# 		# Run ComBat
# 		# 		message("Running ComBat correction...")
# 		# 		message("  Batch factor: ", batch_factor)
# 		# 		message("  Preserving: ", sample_group)
# 		# 		message("  Parametric prior: ", par_prior)
# 		# 		
# 		# 		corrected_data <- sva::ComBat(
# 		# 			dat = expr_data,
# 		# 			batch = batch,
# 		# 			mod = mod,
# 		# 			par.prior = par_prior
# 		# 		)
# 		# 		
# 		# 		# Create new ExpressionSet with corrected data
# 		# 		corrected_ExpSet <- ExpSet
# 		# 		Biobase::exprs(corrected_ExpSet) <- corrected_data
# 		# 		
# 		# 		# Add note to experimentData
# 		# 		Biobase::notes(corrected_ExpSet) <- c(
# 		# 			Biobase::notes(corrected_ExpSet),
# 		# 			list(
# 		# 				combat_correction = list(
# 		# 					batch_factor = batch_factor,
# 		# 					preserved_group = sample_group,
# 		# 					par_prior = par_prior,
# 		# 					correction_date = Sys.time()
# 		# 				)
# 		# 			)
# 		# 		)
# 		# 		
# 		# 		# Store result
# 		# 		corrected_eset(corrected_ExpSet)
# 		# 		
# 		# 		removeNotification("combat_progress")
# 		# 		showNotification("✅ ComBat correction complete!", type = "message", duration = 5)
# 		# 		
# 		# 	}, error = function(e) {
# 		# 		removeNotification("combat_progress")
# 		# 		showNotification(paste("❌ ComBat failed:", e$message), type = "error", duration = 10)
# 		# 		message("ComBat error: ", e$message)
# 		# 	})
# 		# })
# 		
# 		# Correction status
# 		output$correction_status <- renderUI({
# 			if (! is.null(corrected_eset())) {
# 				div(
# 					class = "alert alert-success",
# 					icon("check-circle"),
# 					strong(" Correction complete! "),
# 					p("Corrected ExpressionSet available for download.")
# 				)
# 			}
# 		})
# 		
# 		# Correction summary ####
# 		output$correction_summary <- renderPrint({
# 			req(corrected_eset())
# 			
# 			ExpSet_orig <- eset()
# 			ExpSet_corrected <- corrected_eset()
# 			
# 			notes <- Biobase::notes(ExpSet_corrected)
# 			combat_info <- notes$combat_correction
# 			
# 			cat("═══════════════════════════════════════════════\n")
# 			cat("COMBAT CORRECTION SUMMARY\n")
# 			cat("═══════════════════════════════════════════════\n\n")
# 			
# 			if (! is.null(combat_info)) {
# 				cat("Model used:", combat_info$model, "\n")
# 				
# 				if (! is.na(combat_info$sample_group_preserved)) {
# 					cat("Protected group:", combat_info$sample_group_preserved, "\n")
# 				}
# 				
# 				cat("\nCorrected for", length(combat_info$batch_factors), "batch factor(s):\n")
# 				for (i in seq_along(combat_info$batch_factors)) {
# 					cat(sprintf("  %d. %s\n", i, combat_info$batch_factors[i]))
# 				}
# 				cat("\n")
# 				
# 				cat("Parametric prior:", combat_info$par_prior, "\n")
# 				cat("Row centered:", combat_info$row_centered, "\n\n")
# 			}
# 			
# 			cat("Original data range:", 
# 					round(min(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "to",
# 					round(max(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "\n")
# 			
# 			cat("Corrected data range:", 
# 					round(min(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "to",
# 					round(max(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "\n\n")
# 			
# 			cat("Dimensions:", nrow(ExpSet_corrected), "features ×", ncol(ExpSet_corrected), "samples\n")
# 			
# 			cat("\n═══════════════════════════════════════════════\n")
# 		})
# 		# output$correction_summary <- renderPrint({
# 		# 	req(corrected_eset())
# 		# 	
# 		# 	ExpSet_orig <- eset()
# 		# 	ExpSet_corrected <- corrected_eset()
# 		# 	
# 		# 	cat("═══════════════════════════════════════════════\n")
# 		# 	cat("COMBAT CORRECTION SUMMARY\n")
# 		# 	cat("═══════════════════════════════════════════════\n\n")
# 		# 	
# 		# 	batch_factors <- input$batch_factors
# 		# 	cat("Corrected for", length(batch_factors), "batch factor(s):\n")
# 		# 	for (i in seq_along(batch_factors)) {
# 		# 		cat(sprintf("  %d.  %s\n", i, batch_factors[i]))
# 		# 	}
# 		# 	cat("\n")
# 		# 	
# 		# 	cat("Preserved biological group:", sample_group_column(), "\n")
# 		# 	cat("Parametric prior:", input$par_prior, "\n\n")
# 		# 	
# 		# 	cat("Original data range:", 
# 		# 			round(min(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "to",
# 		# 			round(max(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "\n")
# 		# 	
# 		# 	cat("Corrected data range:", 
# 		# 			round(min(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "to",
# 		# 			round(max(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "\n\n")
# 		# 	
# 		# 	cat("Dimensions:", nrow(ExpSet_corrected), "features ×", ncol(ExpSet_corrected), "samples\n")
# 		# 	
# 		# 	cat("\n═══════════════════════════════════════════════\n")
# 		# })
# 		# output$correction_summary <- renderPrint({
# 		# 	req(corrected_eset())
# 		# 	
# 		# 	ExpSet_orig <- eset()
# 		# 	ExpSet_corrected <- corrected_eset()
# 		# 	
# 		# 	cat("═══════════════════════════════════════════════\n")
# 		# 	cat("COMBAT CORRECTION SUMMARY\n")
# 		# 	cat("═══════════════════════════════════════════════\n\n")
# 		# 	
# 		# 	cat("Corrected for batch factor:", input$batch_factor, "\n")
# 		# 	cat("Preserved biological group:", sample_group_column(), "\n")
# 		# 	cat("Parametric prior:", input$par_prior, "\n\n")
# 		# 	
# 		# 	cat("Original data range:", 
# 		# 			round(min(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "to",
# 		# 			round(max(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "\n")
# 		# 	
# 		# 	cat("Corrected data range:", 
# 		# 			round(min(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "to",
# 		# 			round(max(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "\n\n")
# 		# 	
# 		# 	cat("Dimensions:", nrow(ExpSet_corrected), "features ×", ncol(ExpSet_corrected), "samples\n")
# 		# 	
# 		# 	cat("\n═══════════════════════════════════════════════\n")
# 		# })
# 		
# 		
# 		# Update ####
# 		# Auto-generate suggested name for ComBat assay
# 		observe({
# 			req(input$batch_factors)
# 			req(eset())
# 			
# 			# Get current assay name
# 			current_assay <- Biobase::assayDataElementNames(eset())[1]
# 			
# 			# Generate suggested name
# 			batch_suffix <- paste(input$batch_factors, collapse = "_")
# 			suggested_name <- paste0(current_assay, "_combat")
# 			
# 			updateTextInput(session, "combat_assay_name", value = suggested_name)
# 		})
# 		
# 		# Update ExpressionSet list
# 		update_success <- reactiveVal(FALSE)
# 		
# 		observeEvent(input$update_expset_list, {
# 			req(corrected_eset())
# 			req(input$combat_assay_name)
# 			req(ExpSet_list())
# 			
# 			assay_name <- input$combat_assay_name
# 			
# 			# Validate name
# 			if (assay_name == "" || grepl("^\\s*$", assay_name)) {
# 				showNotification("⚠️ Please provide a valid assay name", type = "warning", duration = 5)
# 				return()
# 			}
# 			
# 			showNotification("Updating ExpressionSet list...", id = "update_progress", duration = NULL, type = "message")
# 			
# 			tryCatch({
# 				# Get the corrected ExpressionSet
# 				corrected_ExpSet <- corrected_eset()
# 				corrected_data <- Biobase::exprs(corrected_ExpSet)
# 				
# 				# Get the original ExpressionSet from the list
# 				expset_list <- ExpSet_list()
# 				selected_name <- selected_expset_name()
# 				
# 				if (! selected_name %in% names(expset_list)) {
# 					stop("Selected ExpressionSet not found in list")
# 				}
# 				
# 				original_ExpSet <- expset_list[[selected_name]]
# 				
# 				# Add ComBat-corrected data as new assayData element
# 				Biobase::assayDataElement(original_ExpSet, assay_name) <- corrected_data
# 				
# 				added_names <- assay_name
# 				
# 				# Optionally add row-centered version
# 				if (input$add_rc_version) {
# 					rc_name <- paste0(assay_name, "_RC")
# 					corrected_data_rc <- row_scale_function(corrected_data)
# 					Biobase::assayDataElement(original_ExpSet, rc_name) <- corrected_data_rc
# 					added_names <- c(added_names, rc_name)
# 				}
# 				
# 				# Update the ComBat column in pData if it was created
# 				corrected_meta <- Biobase::pData(corrected_ExpSet)
# 				original_meta <- Biobase::pData(original_ExpSet)
# 				
# 				# Add any new columns from correction (e.g., ComBat batch variable)
# 				new_cols <- setdiff(colnames(corrected_meta), colnames(original_meta))
# 				if (length(new_cols) > 0) {
# 					for (col in new_cols) {
# 						original_meta[[col]] <- corrected_meta[[col]]
# 					}
# 					Biobase::pData(original_ExpSet) <- original_meta
# 				}
# 				
# 				# Update experimentData with correction info
# 				exp_data <- Biobase::experimentData(original_ExpSet)
# 				if (is.null(exp_data@other$combat_corrections)) {
# 					exp_data@other$combat_corrections <- list()
# 				}
# 				
# 				exp_data@other$combat_corrections[[assay_name]] <- list(
# 					batch_factors = input$batch_factors,
# 					model = input$combat_model,
# 					par_prior = input$par_prior,
# 					correction_date = Sys.time(),
# 					assay_names = added_names
# 				)
# 				
# 				Biobase::experimentData(original_ExpSet) <- exp_data
# 				
# 				# Update the list
# 				expset_list[[selected_name]] <- original_ExpSet
# 				
# 				# Update the reactive ExpSet_list
# 				update_expset_list(expset_list)
# 				
# 				removeNotification("update_progress")
# 				
# 				update_success(TRUE)
# 				
# 				showNotification(
# 					HTML(paste0(
# 						"✅ ExpressionSet updated successfully! <br>",
# 						"Added assayData elements:<br>",
# 						paste("  •", added_names, collapse = "<br>")
# 					)),
# 					type = "message",
# 					duration = 10
# 				)
# 				
# 			}, error = function(e) {
# 				removeNotification("update_progress")
# 				update_success(FALSE)
# 				showNotification(
# 					paste("❌ Failed to update ExpressionSet:", e$message),
# 					type = "error",
# 					duration = 10
# 				)
# 				message("Update error: ", e$message)
# 			})
# 		})
# 		
# 		# Show update status
# 		output$update_status <- renderUI({
# 			if (update_success()) {
# 				div(
# 					class = "alert alert-success",
# 					icon("check-circle"),
# 					strong(" ExpressionSet successfully updated! "),
# 					p("The ComBat-corrected data has been added to your ExpressionSet list."),
# 					p("You can now:"),
# 					tags$ul(
# 						tags$li("View it in the 'ExpSet Viewer' tab"),
# 						tags$li("Use it in downstream analyses"),
# 						tags$li("Save the entire list using the export function")
# 					)
# 				)
# 			}
# 		})
# 		
# 		# Download corrected ExpressionSet ####
# 		output$download_corrected_eset <- downloadHandler(
# 			filename = function() {
# 				paste0("corrected_eset_", input$batch_factor, "_", Sys.Date(), ".rds")
# 			},
# 			content = function(file) {
# 				req(corrected_eset())
# 				saveRDS(corrected_eset(), file)
# 			}
# 		)
# 		
# 		# Download corrected matrix
# 		output$download_corrected_csv <- downloadHandler(
# 			filename = function() {
# 				paste0("corrected_data_", input$batch_factor, "_", Sys.Date(), ".csv")
# 			},
# 			content = function(file) {
# 				req(corrected_eset())
# 				corrected_data <- Biobase::exprs(corrected_eset())
# 				write.csv(corrected_data, file, row.names = TRUE)
# 			}
# 		)
# 		
# 		# Store selected batch factors as reactive
# 		selected_batch_factors <- reactive({
# 			input$batch_factors
# 		})
# 		
# 		# Return corrected ExpressionSet
# 		# return(list(
# 		# 	corrected_eset = corrected_eset,
# 		# 	safe_factors = safe_batch_factors,
# 		# 	selected_batch_factors = selected_batch_factors  # NEW - return the input selection
# 		# 	
# 		# ))
# 		
# 		return(list(
# 			corrected_eset = corrected_eset,
# 			selected_batch_factors = selected_batch_factors,
# 			update_expset_list = function(new_list) {
# 				# This will be used to update the main ExpSet_list
# 				# Called from the observeEvent above
# 			}
# 		))
# 	})
# }