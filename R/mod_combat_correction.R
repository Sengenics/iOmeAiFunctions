# SELECTOR #####

#' ComBat Correction Selector Module - UI
#'
#' Select batch factors and correction settings for ComBat
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_combat_correction_selector_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		box(
			title = "Batch Factor Selection",
			width = 12,
			status = "primary",
			solidHeader = TRUE,
			
			if (debug) {
				actionButton(
					ns("debug"),
					"Debug",
					icon = icon("bug"),
					class = "btn-warning btn-sm"
				)
			},
			
			fluidRow(
				column(
					width = 4,
					uiOutput(ns("combat_selector_ui")),
					box(
						width = 12,
						collapsible = TRUE,
						collapsed = FALSE,
						
						p("Select batch factors to correct using ComBat."),
						p(strong("Safe factors:"), "Strong batch effect (p < 0.05) and NOT confounded with sample groups (p > 0.05)."),
						
						# Multi-factor correction strategy
						uiOutput(ns("multi_factor_options")),
						
						# Batch combination preview
						uiOutput(ns("batch_preview"))
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
				)
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
			)
		)
	)
}

#' ComBat Correction Selector Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param combined_results Reactive data frame from combined batch analysis
#' @param debug Enable debug mode
#' @export
mod_combat_correction_selector_server <- function(id,
																									eset,
																									combined_results = reactive(NULL),
																									debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - ComBat Correction Selector Module")
				message("═══════════════════════════════════════════════")
				browser()
			})
		}
		
		# Identify safe batch factors from combined results
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
			
			# Find batch effect p-value column
			batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
			
			if (is.na(batch_col_name)) {
				warning("Could not find batch effect p-value column")
				return(character(0))
			}
			
			# Filter for safe factors
			safe <- df %>%
				rename(batch_p = !!sym(batch_col_name)) %>%
				filter(batch_p < 0.05, Fisher_p_value > 0.05) %>%
				arrange(batch_p) %>%
				pull(Batch_Column)
			
			as.character(safe)
		})
		
		# Render batch factor selector
		output$combat_selector_ui <- renderUI({
			req(eset())
			
			all_columns <- colnames(Biobase::pData(eset()))
			safe_factors <- safe_batch_factors()
			
			default_selection <- if (length(safe_factors) > 0) {
				safe_factors
			} else {
				NULL
			}
			
			tagList(
				selectInput(
					ns("batch_factors"),
					"Select Batch Factor(s) to Correct:",
					choices = all_columns,
					selected = default_selection,
					multiple = TRUE
				),
				
				uiOutput(ns("factor_safety_info")),
				
				box(
					width = 12,
					collapsible = TRUE,
					collapsed = TRUE,
					
					if (length(safe_factors) > 0) {
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
						strong("Multiple factors: "),
						"ComBat can correct for multiple batch factors simultaneously.",
						br(),
						strong("Order matters:"),
						"Factors are corrected in the order selected (most important first)."
					)
				)
			)
		})
		
		# Factor safety information
		output$factor_safety_info <- renderUI({
			req(input$batch_factors)
			
			selected <- input$batch_factors
			safe <- safe_batch_factors()
			
			if (length(selected) == 0) return(NULL)
			
			safe_selected <- intersect(selected, safe)
			unsafe_selected <- setdiff(selected, safe)
			
			tagList(
				if (length(safe_selected) > 0) {
					div(
						style = "background-color: #d4edda; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
						strong(icon("check-circle", class = "text-success"), " Safe factors: "),
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
							em("These factors were not identified as 'safe'.  They may be confounded with sample groups or have no significant batch effect.")
						)
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
					strong("Combined: "),
					"Creates unique batches for each combination (e.g., Batch1_Date1, Batch1_Date2). More statistically rigorous."
				),
				
				helpText(
					icon("info-circle"),
					strong("Sequential: "),
					"Corrects for first factor, then second, etc. Order matters."
				),
				
				conditionalPanel(
					condition = "input.correction_strategy == 'sequential'",
					ns = ns,
					helpText(
						icon("exclamation-triangle"),
						strong("Note:"),
						"Factors will be corrected in the order shown above."
					)
				)
			)
		})
		
		# Batch preview (keep all your existing preview logic here)
		output$batch_preview <- renderUI({
			req(input$batch_factors)
			req(eset())
			
			if (length(input$batch_factors) <= 1) {
				return(NULL)
			}
			
			# ...  (keep all your existing batch preview rendering code)
			# This is the same as in your original file
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
		
		# Batch Factor Selection (now a separate module)
		mod_combat_correction_selector_ui(ns("selector"), debug = debug),
		
		fluidRow(
			column(12,
						 tabsetPanel(
						 	## Single ####
						 	tabPanel('Single',
						 					 mod_eset_selector_standalone_ui("combat_data", TRUE, TRUE, TRUE, TRUE),
						 					 # ... rest of Single tab
						 	),
						 	
						 	## Multiple ####
						 	tabPanel('Multiple',
						 					 # ... Multiple tab content
						 	),
						 	
						 	## Batch Test ####
						 	tabPanel('Batch Test',
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
		selector <- mod_combat_correction_selector_server(
			"selector",
			eset = eset,
			combined_results = combined_results,
			debug = debug
		)
		
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

