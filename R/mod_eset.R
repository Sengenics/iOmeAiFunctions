# eset_selector #####

#' ExpressionSet Selector Module UI
#'
#' @param id Character; module namespace ID
#'
#' @export
mod_eset_selector_ui <- function(id) {
	ns <- NS(id)
	tagList(
		uiOutput(ns("eset_select_ui"))
	)
}


#' ExpressionSet Selector Module Server
#'
#' @param id Character; module namespace ID
#' @param ExpSet_list Reactive; list of ExpressionSets
#' @param default_selection Character; default selection if available (e.g., "clinical_loess_normalised")
#'
#' @return Reactive ExpressionSet with exprs() set to selected assay
#' @export
mod_eset_selector_server <- function(id, ExpSet_list, default_selection = "clinical_loess_normalised") {
	moduleServer(id, function(input, output, session) {
		
		ns <- session$ns
		
		# Generate list of available ExpressionSets and their assays
		ExpSet_names <- reactive({
			get_expset_assay_names
			req(ExpSet_list())
			name_list = get_expset_assay_names(ExpSet_list())

			return(name_list)
		})
	
		
		# Render the selection UI
		output$eset_select_ui <- renderUI({
			req(ExpSet_names())
			
			choices <- ExpSet_names()
			
			# Safely handle default_selection (could be reactive, static, or NULL)
			default_val <- NULL
			
			if (is.function(default_selection)) {
				# It's a reactive - call it safely
				default_val <- tryCatch({
					default_selection()
				}, error = function(e) {
					NULL
				})
			} else if (! is.null(default_selection)) {
				# It's a static value
				default_val <- default_selection
			}
			
			# Try to select default, otherwise first available
			if (! is.null(default_val) && 
					length(default_val) > 0 && 
					is.character(default_val) && 
					default_val %in% unlist(choices)) {
				selected <- default_val
			} else {
				selected <- unlist(choices)[1]
			}
			
			selectInput(
				ns("eset_select"),
				"Select Expression Data:",
				choices = choices,
				selected = selected,
				width = 1200
			)
		})
		
		if (is.function(default_selection)) {
			observe({
				req(ExpSet_names())
				
				new_default <- tryCatch({
					default_selection()
				}, error = function(e) {
					NULL
				})
				
				if (!is.null(new_default) && 
						length(new_default) > 0 && 
						new_default %in% unlist(ExpSet_names())) {
					updateSelectInput(session, "eset_select", selected = new_default)
				}
			})
		}
		
		# Return selected ExpressionSet with exprs() set to selected assay
		selected_eset <- reactive({
			req(input$eset_select, ExpSet_list())
			
			ExpSet_select_function_2(ExpSet_list(), input$eset_select)
		})
		
		# Also return the selection name for reference
		selected_name <- reactive({
			req(input$eset_select)
			input$eset_select
		})
		
		return(list(
			eset = selected_eset,
			assay_names = ExpSet_names,
			name = selected_name
		))
	})
}

#. ####
## eset_selector_standalone #####
#' Standalone ExpressionSet Selector UI with Subset & Transform
#'
#' Complete data preparation pipeline: Select → Subset → Transform
#'
#' @param id Module namespace ID
#' @param show_summary Show summary box (default TRUE)
#' @param show_status Show status boxes (default TRUE)
#' @param show_subset Show subset module (default TRUE)
#' @param show_transform Show transform module (default TRUE)
#' @param debug Show debug buttons (default FALSE)
#' @export
mod_eset_selector_standalone_ui <- function(id, 
																						show_summary = TRUE,
																						show_subset = TRUE,
																						show_transform = TRUE,
																						show_info = TRUE,
																						debug = FALSE) {
	
	dstart("mod_eset_selector_standalone_ui", id = id, debug = debug)
	
	# ✅ Show configuration
	dmsg(paste("show_summary =", show_summary), debug = debug, level = 1, id = id)
	dmsg(paste("show_subset =", show_subset), debug = debug, level = 1, id = id)
	dmsg(paste("show_transform =", show_transform), debug = debug, level = 1, id = id)
	dmsg(paste("show_info =", show_info), debug = debug, level = 1, id = id)
	
	darrow("Creating namespace", debug = debug, level = 1, id = id)
	ns <- NS(id)
	dsuccess(paste("Namespace created:", id), debug = debug, level = 1, id = id)
	
	# ✅ Build UI structure
	darrow("Building UI structure", debug = debug, level = 1, id = id)
	
	ui_output <- tagList(
		# ✅ Selector row
		{
			darrow("Building selector row", debug = debug, level = 2, id = id)
			selector_row <- fluidRow(
				column(
					width = if (show_info) 11 else 12,
					{
						darrow("Adding mod_eset_selector_ui", debug = debug, level = 3, id = id)
						mod_eset_selector_ui(ns("eset_select"))
					}
				),
				if (show_info) {
					darrow("Adding info icon", debug = debug, level = 3, id = id)
					column(
						width = 1,
						style = "padding-top: 25px;",
						actionLink(
							ns("toggle_details"),
							icon("info-circle", class = "fa-lg"),
							style = "color: #337ab7;",
							title = "Show/hide data details and options"
						)
					)
				}
			)
			dsuccess("Selector row complete", debug = debug, level = 2, id = id)
			selector_row
		},
		
		# ✅ Collapsible details panel
		if (show_info) {
			darrow("Building collapsible details panel", debug = debug, level = 2, id = id)
			
			details_panel <- conditionalPanel(
				condition = "input.toggle_details % 2 == 1",
				ns = ns,
				
				fluidRow(
					column(
						width = 12,
						
						tagList(
							# ✅ Data Summary
							if (show_summary) {
								darrow("Adding summary box", debug = debug, level = 3, id = id)
								summary_ui <- box(
									title = "Data Summary",
									width = 12,
									status = "info",
									solidHeader = TRUE,
									collapsible = TRUE,
									collapsed = TRUE,
									verbatimTextOutput(ns("eset_summary"))
								)
								dsuccess("Summary box added", debug = debug, level = 3, id = id)
								summary_ui
							},
							
							# ✅ Subset Section
							if (show_subset) {
								darrow("Adding subset module UI", debug = debug, level = 3, id = id)
								subset_ui <- box(
									title = "Subset Data",
									width = 12,
									status = "primary",
									solidHeader = TRUE,
									collapsible = TRUE,
									collapsed = TRUE,
									mod_eset_subset_ui(ns("subset"), debug = debug)
								)
								dsuccess("Subset module UI added", debug = debug, level = 3, id = id)
								subset_ui
							},
							
							# ✅ Transform Section
							if (show_transform) {
								darrow("Adding transform module UI", debug = debug, level = 3, id = id)
								transform_ui <- box(
									title = "Transform Data",
									width = 12,
									status = "primary",
									solidHeader = TRUE,
									collapsible = TRUE,
									collapsed = TRUE,
									mod_eset_transform_ui(ns("transform"), debug = debug)
								)
								dsuccess("Transform module UI added", debug = debug, level = 3, id = id)
								transform_ui
							},
							
							# ✅ Debug button
							if (debug) {
								darrow("Adding debug button", debug = debug, level = 3, id = id)
								debug_btn <- fluidRow(
									column(
										width = 12,
										style = "margin-top: 10px;",
										actionButton(
											ns("debug"),
											"Debug:  mod_eset_selector_standalone",
											icon = icon("bug"),
											class = "btn-warning btn-sm",
											style = "width: 100%;"
										)
									)
								)
								dsuccess("Debug button added", debug = debug, level = 3, id = id)
								debug_btn
							}
						)
					)
				)
			)
			
			dsuccess("Collapsible details panel complete", debug = debug, level = 2, id = id)
			details_panel
		} else {
			dmsg("Info panel disabled (show_info = FALSE)", debug = debug, level = 2, id = id)
		}
	)
	
	dsuccess("UI structure complete", debug = debug, level = 1, id = id)
	dend("mod_eset_selector_standalone_ui", id = id, debug = debug)
	
	return(ui_output)
}

#' Standalone ExpressionSet Selector Server with Subset & Transform
#'
#' Complete data preparation pipeline
#'
#' @param id Module namespace ID
#' @param ExpSet_list Reactive returning named list of ExpressionSets
#' @param default_selection Default ExpSet name to select
#' @param source Reactive returning data source ("uploaded", "package", etc.)
#' @param enable_subset Enable subsetting (default TRUE)
#' @param enable_transform Enable transformation (default TRUE)
#' @param debug Enable debug mode (default FALSE)
#' @export
# eset_selector_standalone #####
#' Standalone ExpressionSet Selector Server with Subset & Transform
#'
#' Complete data preparation pipeline
#'
#' @param id Module namespace ID
#' @param ExpSet_list Reactive returning named list of ExpressionSets
#' @param default_selection Default ExpSet name to select
#' @param enable_subset Enable subsetting (default TRUE)
#' @param enable_transform Enable transformation (default TRUE)
#' @param debug Enable debug mode (default FALSE)
#' @export
mod_eset_selector_standalone_server <- function(id, 
																								ExpSet_list,
																								default_selection = reactive(NULL),
																								enable_subset = TRUE,
																								enable_transform = TRUE,
																								debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		dstart("ExpressionSet Selector Standalone", id = id, debug = debug)
		
		# ✅ Debug observer
		if (debug) {
			observeEvent(input$debug, {
				dmsg("Manual debug triggered - entering browser()", debug = TRUE, id = id)
				browser()
			})
		}
		
		### 1. Handle Default Selection Updates ####
		darrow("Setting up default selection observer", debug = debug, level = 1, id = id)
		observe({
			darrow("Checking default_selection update", debug = debug, level = 2, id = id)
			
			if (is.function(default_selection)) {
				new_default <- default_selection()
				dmsg(paste("default_selection is reactive, value:", new_default), debug = debug, level = 3, id = id)
			} else {
				new_default <- default_selection
				dmsg(paste("default_selection is static, value:", new_default), debug = debug, level = 3, id = id)
			}
			
			if (! is.null(new_default) && length(new_default) > 0) {
				dmsg(paste("Updating picker selection to:", new_default), debug = debug, level = 3, id = id)
				updateSelectInput(session, "eset_select-eset_select", selected = new_default)
				dsuccess("Picker updated", debug = debug, level = 3, id = id)
			} else {
				dwarn("No default selection provided", debug = debug, level = 3, id = id)
			}
		})
		dsuccess("Default selection observer ready", debug = debug, level = 1, id = id)
		
		### 2. ExpSet Selection Module ####
		darrow("Initializing ExpSet selection module", debug = debug, level = 1, id = id)
		eset_selected_module <- mod_eset_selector_server(
			"eset_select",
			ExpSet_list = ExpSet_list,
			default_selection = default_selection
		)
		dsuccess("ExpSet selection module initialized", debug = debug, level = 1, id = id)
		
		# Extract selected ExpressionSet
		darrow("Creating eset_selected reactive", debug = debug, level = 1, id = id)
		eset_selected <- reactive({
			darrow("eset_selected reactive triggered", debug = debug, level = 2, id = id)
			req(eset_selected_module$eset())
			
			eset <- eset_selected_module$eset()
			dsuccess(paste0("ExpressionSet loaded: ", nrow(eset), " features × ", ncol(eset), " samples"), 
							 debug = debug, level = 2, id = id)
			eset
		})
		dsuccess("eset_selected reactive created", debug = debug, level = 1, id = id)
		
		### 3. Subset Module (optional) ####
		if (enable_subset) {
			darrow("Initializing subset module", debug = debug, level = 1, id = id)
			
			subset_module <- mod_eset_subset_server(
				"subset",
				eset = eset_selected,
				debug = debug
			)
			dsuccess("Subset module initialized", debug = debug, level = 1, id = id)
			
			# ✅ FIX: Only reset when dataset actually changes (not on every invalidation)
			darrow("Setting up subset reset observer", debug = debug, level = 1, id = id)
			last_eset_name <- reactiveVal(NULL)
			
			observe({
				darrow("Subset reset observer triggered", debug = debug, level = 2, id = id)
				req(eset_selected())
				req(eset_selected_module$name())
				
				current_name <- eset_selected_module$name()
				
				# Only reset if dataset name changed (user selected different data)
				if (is.null(last_eset_name()) || last_eset_name() != current_name) {
					dmsg(paste("Dataset changed:", last_eset_name(), "→", current_name), 
							 debug = debug, level = 2, id = id)
					subset_module$subset_eset(eset_selected())
					last_eset_name(current_name)
					dsuccess("Subset reset complete", debug = debug, level = 2, id = id)
				} else {
					dmsg("Same dataset - keeping current subset", debug = debug, level = 2, id = id)
				}
			}, priority = 100)
			
			# ✅ Wrap reactiveVal in reactive() for consistent access
			eset_after_subset <- reactive({
				req(subset_module$subset_eset)
				subset_module$subset_eset()
			})
			
			dsuccess("Subset module ready", debug = debug, level = 1, id = id)
		} else {
			dmsg("Subset module disabled - using original data", debug = debug, level = 1, id = id)
			eset_after_subset <- eset_selected
		}
		
		### 4. Transform Module (optional) ####
		if (enable_transform) {
			darrow("Initializing transform module", debug = debug, level = 1, id = id)
			
			transform_module <- mod_eset_transform_server(
				"transform",
				eset = eset_after_subset,
				debug = debug
			)
			dsuccess("Transform module initialized", debug = debug, level = 1, id = id)
			
			# ✅ FIX: Track subset dimensions to detect actual changes
			darrow("Setting up transform reset observer", debug = debug, level = 1, id = id)
			last_subset_dims <- reactiveVal(NULL)
			
			observe({
				darrow("Transform reset observer triggered", debug = debug, level = 2, id = id)
				req(eset_after_subset())
				
				current_dims <- paste(dim(eset_after_subset()), collapse = "x")
				
				# Only reset if dimensions changed (subset was applied)
				if (is.null(last_subset_dims()) || last_subset_dims() != current_dims) {
					dmsg(paste("Subset changed:", last_subset_dims(), "→", current_dims), 
							 debug = debug, level = 2, id = id)
					transform_module$transformed_eset(eset_after_subset())
					last_subset_dims(current_dims)
					dsuccess("Transform reset complete", debug = debug, level = 2, id = id)
				} else {
					dmsg("Same subset - keeping current transform", debug = debug, level = 2, id = id)
				}
			}, priority = 100)
			
			# ✅ Wrap reactiveVal in reactive() for consistent access
			final_eset <- reactive({
				req(transform_module$transformed_eset)
				transform_module$transformed_eset()
			})
			
			dsuccess("Transform module ready", debug = debug, level = 1, id = id)
		} else {
			dmsg("Transform module disabled - using subset data", debug = debug, level = 1, id = id)
			final_eset <- eset_after_subset
		}
		
		### 5. Output Renderers ####
		
		# ExpSet info (short version)
		darrow("Setting up eset_info output", debug = debug, level = 1, id = id)
		output$eset_info <- renderPrint({
			darrow("Rendering eset_info", debug = debug, level = 2, id = id)
			req(eset_selected())
			
			cat("Assay: ", eset_selected_module$name(), "\n")
			cat("Samples: ", ncol(eset_selected()), "\n")
			cat("Features: ", nrow(eset_selected()), "\n")
			cat("Available Assays: ", paste(Biobase::assayDataElementNames(eset_selected()), collapse = ", "), "\n")
			
			dsuccess("eset_info rendered", debug = debug, level = 2, id = id)
		})
		dsuccess("eset_info output ready", debug = debug, level = 1, id = id)
		
		# Complete ExpSet summary
		darrow("Setting up eset_summary output", debug = debug, level = 1, id = id)
		output$eset_summary <- renderPrint({
			darrow("Rendering eset_summary", debug = debug, level = 2, id = id)
			req(final_eset())  # ✅ Show FINAL data (after subset/transform)
			
			cat("═══════════════════════════════════════════════════════════\n")
			cat("EXPRESSIONSET SUMMARY (FINAL PROCESSED DATA)\n")
			cat("════════════════════════════��══════════════════════════════\n")
			cat("Original Assay: ", eset_selected_module$name(), "\n")
			cat("Dimensions: ", nrow(final_eset()), " features × ", ncol(final_eset()), " samples\n\n")
			
			# Show if subset was applied
			if (enable_subset) {
				orig_dims <- paste(nrow(eset_selected()), "×", ncol(eset_selected()))
				final_dims <- paste(nrow(final_eset()), "×", ncol(final_eset()))
				if (orig_dims != final_dims) {
					cat("⚠️ SUBSET APPLIED: ", orig_dims, " → ", final_dims, "\n\n")
				}
			}
			
			cat("Available Assays:\n")
			cat("  ", paste(Biobase::assayDataElementNames(final_eset()), collapse = ", "), "\n\n")
			
			cat("Sample Metadata (pData) Columns:\n")
			cat("  ", paste(colnames(Biobase::pData(final_eset())), collapse = ", "), "\n\n")
			
			tryCatch({
				fdata <- Biobase::fData(final_eset())
				if (!is.null(fdata) && ncol(fdata) > 0) {
					cat("Feature Metadata (fData) Columns:\n")
					cat("  ", paste(colnames(fdata), collapse = ", "), "\n")
				} else {
					cat("Feature Metadata: Not available\n")
				}
			}, error = function(e) {
				cat("Feature Metadata: Not available\n")
				dwarn(paste("Error reading fData:", e$message), debug = debug, level = 3, id = id)
			})
			
			dsuccess("eset_summary rendered", debug = debug, level = 2, id = id)
		})
		dsuccess("eset_summary output ready", debug = debug, level = 1, id = id)
		
		### 6. Return Values ####
		darrow("Preparing return values", debug = debug, level = 1, id = id)
		
		return_list <- list(
			assay_names = eset_selected_module$assay_names,
			# Final processed data (after subset + transform)
			eset = final_eset,
			eset_name = eset_selected_module$name,
			# Intermediate stages (if needed)
			eset_original = eset_selected,
			eset_subset = if (enable_subset) eset_after_subset else NULL,
			# Access to sub-modules (if needed for advanced use)
			subset_module = if (enable_subset) subset_module else NULL,
			transform_module = if (enable_transform) transform_module else NULL
		)
		
		dsuccess("Return values prepared", debug = debug, level = 1, id = id)
		dend("ExpressionSet Selector Standalone", id = id, debug = debug)
		
		return(return_list)
	})
}
# mod_eset_selector_standalone_server <- function(id, 
# 																								ExpSet_list,
# 																								default_selection = reactive(NULL),
# 																								#source = reactive("unknown"),
# 																								enable_subset = TRUE,
# 																								enable_transform = TRUE,
# 																								debug = FALSE) {
# 	moduleServer(id, function(input, output, session) {
# 		
# 		dstart("ExpressionSet Selector Standalone", id = id, debug = debug)
# 		
# 		# ✅ Debug observer
# 		if (debug) {
# 			observeEvent(input$debug, {
# 				dmsg("Manual debug triggered - entering browser()", debug = TRUE, id = id)
# 				browser()
# 			})
# 		}
# 		
# 		### 1. Handle Default Selection Updates ####
# 		darrow("Setting up default selection observer", debug = debug, level = 1, id = id)
# 		observe({
# 			darrow("Checking default_selection update", debug = debug, level = 2, id = id)
# 			
# 			if (is.function(default_selection)) {
# 				new_default <- default_selection()
# 				dmsg("default_selection is reactive, value:", new_default, debug = debug, level = 3, id = id)
# 			} else {
# 				new_default <- default_selection
# 				dmsg("default_selection is static, value:", new_default, debug = debug, level = 3, id = id)
# 			}
# 			
# 			if (! is.null(new_default) && length(new_default) > 0) {
# 				dmsg("Updating picker selection to:", new_default, debug = debug, level = 3, id = id)
# 				updatePickerInput(session, "eset_select", selected = new_default)
# 				dsuccess("Picker updated", debug = debug, level = 3, id = id)
# 			} else {
# 				dwarn("No default selection provided", debug = debug, level = 3, id = id)
# 			}
# 		})
# 		dsuccess("Default selection observer ready", debug = debug, level = 1, id = id)
# 		
# 		### 2. ExpSet Selection Module ####
# 		darrow("Initializing ExpSet selection module", debug = debug, level = 1, id = id)
# 		eset_selected_module <- mod_eset_selector_server(
# 			"eset_select",
# 			ExpSet_list = ExpSet_list,
# 			default_selection = default_selection
# 		)
# 		dsuccess("ExpSet selection module initialized", debug = debug, level = 1, id = id)
# 		
# 		# Extract selected ExpressionSet
# 		darrow("Creating eset_selected reactive", debug = debug, level = 1, id = id)
# 		eset_selected <- reactive({
# 			darrow("eset_selected reactive triggered", debug = debug, level = 2, id = id)
# 			req(eset_selected_module$eset())
# 			
# 			eset <- eset_selected_module$eset()
# 			dsuccess(paste0("ExpressionSet loaded:  ", nrow(eset), " features × ", ncol(eset), " samples"), 
# 							 debug = debug, level = 2, id = id)
# 			eset
# 		})
# 		dsuccess("eset_selected reactive created", debug = debug, level = 1, id = id)
# 		
# 		### 3. Subset Module (optional) ####
# 		if (enable_subset) {
# 			darrow("Initializing subset module", debug = debug, level = 1, id = id)
# 			
# 			subset_module <- mod_eset_subset_server(
# 				"subset",
# 				eset = eset_selected,
# 				debug = debug
# 			)
# 			dsuccess("Subset module initialized", debug = debug, level = 1, id = id)
# 			
# 			# Reset subset when new data is loaded
# 			darrow("Setting up subset reset observer", debug = debug, level = 1, id = id)
# 			observe({
# 				darrow("Subset reset observer triggered", debug = debug, level = 2, id = id)
# 				req(eset_selected())
# 				
# 				dmsg("Resetting subset module with new data", debug = debug, level = 2, id = id)
# 				subset_module$subset_eset(eset_selected())
# 				dsuccess("Subset module reset complete", debug = debug, level = 2, id = id)
# 			}, priority = 100)
# 			
# 			eset_after_subset <- subset_module$subset_eset
# 			dsuccess("Subset module ready", debug = debug, level = 1, id = id)
# 		} else {
# 			dmsg("Subset module disabled - using original data", debug = debug, level = 1, id = id)
# 			eset_after_subset <- eset_selected
# 		}
# 		
# 		### 4. Transform Module (optional) ####
# 		if (enable_transform) {
# 			darrow("Initializing transform module", debug = debug, level = 1, id = id)
# 			
# 			transform_module <- mod_eset_transform_server(
# 				"transform",
# 				eset = eset_after_subset,
# 				debug = debug
# 			)
# 			dsuccess("Transform module initialized", debug = debug, level = 1, id = id)
# 			
# 			# Reset transform when subset changes
# 			darrow("Setting up transform reset observer", debug = debug, level = 1, id = id)
# 			observe({
# 				darrow("Transform reset observer triggered", debug = debug, level = 2, id = id)
# 				req(eset_after_subset())
# 				
# 				dmsg("Resetting transform module with new data", debug = debug, level = 2, id = id)
# 				transform_module$transformed_eset(eset_after_subset())
# 				dsuccess("Transform module reset complete", debug = debug, level = 2, id = id)
# 			}, priority = 100)
# 			
# 			final_eset <- transform_module$transformed_eset
# 			dsuccess("Transform module ready", debug = debug, level = 1, id = id)
# 		} else {
# 			dmsg("Transform module disabled - using subset data", debug = debug, level = 1, id = id)
# 			final_eset <- eset_after_subset
# 		}
# 		
# 		### 5. Output Renderers ####
# 		
# 		# ExpSet info (short version)
# 		darrow("Setting up eset_info output", debug = debug, level = 1, id = id)
# 		output$eset_info <- renderPrint({
# 			darrow("Rendering eset_info", debug = debug, level = 2, id = id)
# 			req(eset_selected())
# 			
# 			cat("Assay:  ", eset_selected_module$name(), "\n")
# 			cat("Samples: ", ncol(eset_selected()), "\n")
# 			cat("Features: ", nrow(eset_selected()), "\n")
# 			cat("Available Assays: ", paste(Biobase::assayDataElementNames(eset_selected()), collapse = ", "), "\n")
# 			
# 			dsuccess("eset_info rendered", debug = debug, level = 2, id = id)
# 		})
# 		dsuccess("eset_info output ready", debug = debug, level = 1, id = id)
# 		
# 		# Complete ExpSet summary
# 		darrow("Setting up eset_summary output", debug = debug, level = 1, id = id)
# 		output$eset_summary <- renderPrint({
# 			darrow("Rendering eset_summary", debug = debug, level = 2, id = id)
# 			req(eset_selected())
# 			
# 			cat("═══════════════════════════════════════════════════════════\n")
# 			cat("EXPRESSIONSET SUMMARY\n")
# 			cat("═══════════════════════════════════════════════════════════\n")
# 			cat("Selected Assay: ", eset_selected_module$name(), "\n")
# 			cat("Dimensions: ", nrow(eset_selected()), " features × ", ncol(eset_selected()), " samples\n\n")
# 			
# 			cat("Available Assays:\n")
# 			cat("  ", paste(Biobase::assayDataElementNames(eset_selected()), collapse = ", "), "\n\n")
# 			
# 			cat("Sample Metadata (pData) Columns:\n")
# 			cat("  ", paste(colnames(Biobase::pData(eset_selected())), collapse = ", "), "\n\n")
# 			
# 			tryCatch({
# 				fdata <- Biobase::fData(eset_selected())
# 				if (!is.null(fdata) && ncol(fdata) > 0) {
# 					cat("Feature Metadata (fData) Columns:\n")
# 					cat("  ", paste(colnames(fdata), collapse = ", "), "\n")
# 				} else {
# 					cat("Feature Metadata: Not available\n")
# 				}
# 			}, error = function(e) {
# 				cat("Feature Metadata: Not available\n")
# 				dwarn("Error reading fData:", e$message, debug = debug, level = 3, id = id)
# 			})
# 			
# 			dsuccess("eset_summary rendered", debug = debug, level = 2, id = id)
# 		})
# 		dsuccess("eset_summary output ready", debug = debug, level = 1, id = id)
# 		
# 		### 6. Return Values ####
# 		darrow("Preparing return values", debug = debug, level = 1, id = id)
# 		
# 		return_list <- list(
# 			assay_names = eset_selected_module$assay_names,
# 			# Final processed data
# 			eset = final_eset,
# 			eset_name = eset_selected_module$name,
# 			# Intermediate stages (if needed)
# 			eset_original = eset_selected,
# 			eset_subset = if (enable_subset) eset_after_subset else NULL,
# 			# Access to sub-modules (if needed)
# 			subset_module = if (enable_subset) subset_module else NULL,
# 			transform_module = if (enable_transform) transform_module else NULL
# 		)
# 		
# 		dsuccess("Return values prepared", debug = debug, level = 1, id = id)
# 		dend("ExpressionSet Selector Standalone", id = id, debug = debug)
# 		
# 		return(return_list)
# 	})
# }
#. #####
# eset_subset ####
# eset_subset ####

#' ExpressionSet Subsetting Module - UI
#'
#' Subset ExpressionSet by pData or fData columns
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_eset_subset_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Subset ExpressionSet",
				width = 12,
				collapsible = TRUE,
				collapsed = TRUE,
				
				p("Filter samples or features based on metadata columns."),
				
				uiOutput(ns("debug_ui")),
				
				fluidRow(
					column(
						width = 6,
						h4(icon("users"), " Sample Subsetting (pData)"),
						uiOutput(ns("pdata_subset_ui")),
						hr(),
						verbatimTextOutput(ns("pdata_subset_summary"))
					),
					column(
						width = 6,
						h4(icon("dna"), " Feature Subsetting (fData)"),
						uiOutput(ns("fdata_subset_ui")),
						hr(),
						verbatimTextOutput(ns("fdata_subset_summary"))
					)
				),
				
				hr(),
				
				fluidRow(
					column(
						width = 6,
						actionButton(
							ns("apply_subset"),
							"Apply Subset",
							icon = icon("filter"),
							class = "btn-success btn-lg btn-block"
						)
					),
					column(
						width = 6,
						actionButton(
							ns("reset_subset"),
							"Reset to Original",
							icon = icon("undo"),
							class = "btn-warning btn-lg btn-block"
						)
					)
				),
				
				hr(),
				
				uiOutput(ns("subset_status"))
			)
		)
	)
}

#' ExpressionSet Subsetting Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet (original)
#' @param debug Enable debug mode
#' @export
mod_eset_subset_server <- function(id, eset, debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
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
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - ExpressionSet Subset Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Original ExpressionSet")
				message("  • subset_eset() - Subsetted ExpressionSet")
				message("  • selected_pdata_filters() - pData filters")
				message("  • selected_fdata_filters() - fData filters")
				message("\nUseful commands:")
				message("  dim(eset())")
				message("  dim(subset_eset())")
				message("  selected_pdata_filters()")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Store original and subset ExpressionSets
		original_eset <- reactive({ eset() })
		subset_eset <- reactiveVal(NULL)
		
		# Initialize with original
		observe({
			req(original_eset())
			if (is.null(subset_eset())) {
				subset_eset(original_eset())
			}
		})
		
		# Get pData columns
		pdata_columns <- reactive({
			req(original_eset())
			colnames(Biobase::pData(original_eset()))
		})
		
		# Get fData columns
		fdata_columns <- reactive({
			req(original_eset())
			tryCatch({
				fd <- Biobase::fData(original_eset())
				if (!is.null(fd) && ncol(fd) > 0) {
					colnames(fd)
				} else {
					NULL
				}
			}, error = function(e) NULL)
		})
		
		# Storage for selected filters
		# Structure: list(column_name = list(values = c(...), action = "keep"/"remove"))
		selected_pdata_filters <- reactiveVal(list())
		selected_fdata_filters <- reactiveVal(list())
		
		# Render pData subsetting UI
		output$pdata_subset_ui <- renderUI({
			req(pdata_columns())
			
			ns <- session$ns
			cols <- pdata_columns()
			
			tagList(
				selectInput(
					ns("pdata_column"),
					"Select pData Column:",
					choices = c("None" = "", cols),
					selected = ""
				),
				
				# ✅ ADD: Keep vs Remove toggle
				conditionalPanel(
					condition = "input.pdata_column != ''",
					ns = ns,
					radioButtons(
						ns("pdata_action"),
						"Action:",
						choices = c(
							"Keep selected values" = "keep",
							"Remove selected values" = "remove"
						),
						selected = "keep",
						inline = TRUE
					)
				),
				
				uiOutput(ns("pdata_values_ui")),
				
				actionButton(
					ns("add_pdata_filter"),
					"Add Filter",
					icon = icon("plus"),
					class = "btn-primary btn-sm"
				)
			)
		})
		
		# Render pData values based on selected column
		output$pdata_values_ui <- renderUI({
			req(input$pdata_column)
			req(input$pdata_column != "")
			
			ns <- session$ns
			col <- input$pdata_column
			pdata <- Biobase::pData(original_eset())
			
			values <- unique(pdata[[col]])
			values <- values[!is.na(values)]
			
			# ✅ CHANGE: Default selection based on action
			default_selected <- if (input$pdata_action == "keep") {
				values  # Select all for "keep"
			} else {
				NULL    # Select none for "remove"
			}
			
			# ✅ CHANGE: Label changes based on action
			label <- if (input$pdata_action == "keep") {
				paste("Select values to KEEP from", col, ":")
			} else {
				paste("Select values to REMOVE from", col, ":")
			}
			
			selectInput(
				ns("pdata_values"),
				label,
				choices = values,
				selected = default_selected,
				multiple = TRUE
			)
		})
		
		# Add pData filter
		observeEvent(input$add_pdata_filter, {
			req(input$pdata_column)
			req(input$pdata_values)
			req(input$pdata_action)
			
			current_filters <- selected_pdata_filters()
			
			# ✅ Store both values AND action
			current_filters[[input$pdata_column]] <- list(
				values = input$pdata_values,
				action = input$pdata_action
			)
			
			selected_pdata_filters(current_filters)
			
			action_text <- ifelse(input$pdata_action == "keep", "KEEP", "REMOVE")
			showNotification(
				paste0("Added filter: ", input$pdata_column, " (", action_text, " ", 
							 length(input$pdata_values), " values)"),
				type = "message",
				duration = 3
			)
		})
		
		# Render fData subsetting UI
		output$fdata_subset_ui <- renderUI({
			ns <- session$ns
			
			fcols <- fdata_columns()
			
			if (is.null(fcols) || length(fcols) == 0) {
				return(p("No fData available for subsetting"))
			}
			
			tagList(
				selectInput(
					ns("fdata_column"),
					"Select fData Column:",
					choices = c("None" = "", fcols),
					selected = ""
				),
				
				# ✅ ADD: Keep vs Remove toggle
				conditionalPanel(
					condition = "input.fdata_column != ''",
					ns = ns,
					radioButtons(
						ns("fdata_action"),
						"Action:",
						choices = c(
							"Keep selected values" = "keep",
							"Remove selected values" = "remove"
						),
						selected = "keep",
						inline = TRUE
					)
				),
				
				uiOutput(ns("fdata_values_ui")),
				
				actionButton(
					ns("add_fdata_filter"),
					"Add Filter",
					icon = icon("plus"),
					class = "btn-primary btn-sm"
				)
			)
		})
		
		# Render fData values based on selected column
		output$fdata_values_ui <- renderUI({
			req(input$fdata_column)
			req(input$fdata_column != "")
			
			ns <- session$ns
			col <- input$fdata_column
			fdata <- Biobase::fData(original_eset())
			
			values <- unique(fdata[[col]])
			values <- values[!is.na(values)]
			
			# ✅ CHANGE: Default selection based on action
			default_selected <- if (input$fdata_action == "keep") {
				values  # Select all for "keep"
			} else {
				NULL    # Select none for "remove"
			}
			
			# ✅ CHANGE: Label changes based on action
			label <- if (input$fdata_action == "keep") {
				paste("Select values to KEEP from", col, ":")
			} else {
				paste("Select values to REMOVE from", col, ":")
			}
			
			selectInput(
				ns("fdata_values"),
				label,
				choices = values,
				selected = default_selected,
				multiple = TRUE
			)
		})
		
		# Add fData filter
		observeEvent(input$add_fdata_filter, {
			req(input$fdata_column)
			req(input$fdata_values)
			req(input$fdata_action)
			
			current_filters <- selected_fdata_filters()
			
			# ✅ Store both values AND action
			current_filters[[input$fdata_column]] <- list(
				values = input$fdata_values,
				action = input$fdata_action
			)
			
			selected_fdata_filters(current_filters)
			
			action_text <- ifelse(input$fdata_action == "keep", "KEEP", "REMOVE")
			showNotification(
				paste0("Added filter: ", input$fdata_column, " (", action_text, " ", 
							 length(input$fdata_values), " values)"),
				type = "message",
				duration = 3
			)
		})
		
		# pData filter summary
		output$pdata_subset_summary <- renderPrint({
			filters <- selected_pdata_filters()
			
			if (length(filters) == 0) {
				cat("No pData filters applied\n")
			} else {
				cat("Active pData Filters:\n")
				cat("═══════════════════════\n")
				for (col in names(filters)) {
					filter_info <- filters[[col]]
					action_text <- toupper(filter_info$action)
					values_text <- paste(filter_info$values, collapse = ", ")
					
					cat(sprintf("%s [%s]:\n  %s\n", col, action_text, values_text))
				}
			}
		})
		
		# fData filter summary
		output$fdata_subset_summary <- renderPrint({
			filters <- selected_fdata_filters()
			
			if (length(filters) == 0) {
				cat("No fData filters applied\n")
			} else {
				cat("Active fData Filters:\n")
				cat("═══════════════════════\n")
				for (col in names(filters)) {
					filter_info <- filters[[col]]
					action_text <- toupper(filter_info$action)
					values_text <- paste(filter_info$values, collapse = ", ")
					
					cat(sprintf("%s [%s]:\n  %s\n", col, action_text, values_text))
				}
			}
		})
		
		# Apply subset
		observeEvent(input$apply_subset, {
			req(original_eset())
			
			tryCatch({
				ExpSet <- original_eset()
				pdata_filters <- selected_pdata_filters()
				fdata_filters <- selected_fdata_filters()
				
				# ✅ UPDATED: Apply pData filters with keep/remove logic
				if (length(pdata_filters) > 0) {
					pdata <- Biobase::pData(ExpSet)
					keep_samples <- rep(TRUE, nrow(pdata))
					
					for (col in names(pdata_filters)) {
						filter_info <- pdata_filters[[col]]
						
						if (filter_info$action == "keep") {
							# KEEP: samples must be IN the selected values
							keep_samples <- keep_samples & (pdata[[col]] %in% filter_info$values)
						} else {
							# REMOVE: samples must NOT be IN the selected values
							keep_samples <- keep_samples & !(pdata[[col]] %in% filter_info$values)
						}
					}
					
					ExpSet <- ExpSet[, keep_samples]
					message(sprintf("Kept %d / %d samples", sum(keep_samples), length(keep_samples)))
				}
				
				# ✅ UPDATED: Apply fData filters with keep/remove logic
				if (length(fdata_filters) > 0) {
					fdata <- Biobase::fData(ExpSet)
					keep_features <- rep(TRUE, nrow(fdata))
					
					for (col in names(fdata_filters)) {
						filter_info <- fdata_filters[[col]]
						
						if (filter_info$action == "keep") {
							# KEEP: features must be IN the selected values
							keep_features <- keep_features & (fdata[[col]] %in% filter_info$values)
						} else {
							# REMOVE: features must NOT be IN the selected values
							keep_features <- keep_features & !(fdata[[col]] %in% filter_info$values)
						}
					}
					
					ExpSet <- ExpSet[keep_features, ]
					message(sprintf("Kept %d / %d features", sum(keep_features), length(keep_features)))
				}
				
				subset_eset(ExpSet)
				
				showNotification(
					"✅ Subset applied successfully!",
					type = "message",
					duration = 5
				)
				
			}, error = function(e) {
				showNotification(
					paste("❌ Subset failed:", e$message),
					type = "error",
					duration = 10
				)
			})
		})
		
		# Reset to original
		observeEvent(input$reset_subset, {
			subset_eset(original_eset())
			selected_pdata_filters(list())
			selected_fdata_filters(list())
			
			showNotification(
				"Reset to original ExpressionSet",
				type = "message",
				duration = 3
			)
		})
		
		# Status display
		output$subset_status <- renderUI({
			req(original_eset())
			req(subset_eset())
			
			orig_samples <- ncol(original_eset())
			orig_features <- nrow(original_eset())
			sub_samples <- ncol(subset_eset())
			sub_features <- nrow(subset_eset())
			
			div(
				class = "alert alert-info",
				h4("Subset Status:"),
				p(sprintf("Samples: %d / %d (%.1f%%)", 
									sub_samples, orig_samples, 
									100 * sub_samples / orig_samples)),
				p(sprintf("Features: %d / %d (%.1f%%)", 
									sub_features, orig_features, 
									100 * sub_features / orig_features))
			)
		})
		
		# Return subset ExpressionSet
		return(list(
			subset_eset = subset_eset
		))
	})
}

#' #' ExpressionSet Subsetting Module - UI
#' #'
#' #' Subset ExpressionSet by pData or fData columns
#' #'
#' #' @param id Module namespace ID
#' #' @param debug Show debug button
#' #' @export
#' mod_eset_subset_ui <- function(id, debug = FALSE) {
#' 	ns <- NS(id)
#' 	
#' 	tagList(
#' 		fluidRow(
#' 			box(
#' 				title = "Subset ExpressionSet",
#' 				width = 12,
#' 				#status = "primary",
#' 				#solidHeader = TRUE,
#' 				collapsible = TRUE,
#' 				collapsed = T,
#' 				
#' 				p("Filter samples or features based on metadata columns. "),
#' 				
#' 				uiOutput(ns("debug_ui")),
#' 				
#' 				fluidRow(
#' 					column(
#' 						width = 6,
#' 						h4(icon("users"), " Sample Subsetting (pData)"),
#' 						uiOutput(ns("pdata_subset_ui")),
#' 						hr(),
#' 						verbatimTextOutput(ns("pdata_subset_summary"))
#' 					),
#' 					column(
#' 						width = 6,
#' 						h4(icon("dna"), " Feature Subsetting (fData)"),
#' 						uiOutput(ns("fdata_subset_ui")),
#' 						hr(),
#' 						verbatimTextOutput(ns("fdata_subset_summary"))
#' 					)
#' 				),
#' 				
#' 				hr(),
#' 				
#' 				fluidRow(
#' 					column(
#' 						width = 6,
#' 						actionButton(
#' 							ns("apply_subset"),
#' 							"Apply Subset",
#' 							icon = icon("filter"),
#' 							class = "btn-success btn-lg btn-block"
#' 						)
#' 					),
#' 					column(
#' 						width = 6,
#' 						actionButton(
#' 							ns("reset_subset"),
#' 							"Reset to Original",
#' 							icon = icon("undo"),
#' 							class = "btn-warning btn-lg btn-block"
#' 						)
#' 					)
#' 				),
#' 				
#' 				hr(),
#' 				
#' 				uiOutput(ns("subset_status"))
#' 			)
#' 		)
#' 	)
#' }
#' 
#' #' ExpressionSet Subsetting Module - Server
#' #'
#' #' @param id Module namespace ID
#' #' @param eset Reactive ExpressionSet (original)
#' #' @param debug Enable debug mode
#' #' @export
#' mod_eset_subset_server <- function(id, eset, debug = FALSE) {
#' 	moduleServer(id, function(input, output, session) {
#' 		
#' 		# Render debug button
#' 		output$debug_ui <- renderUI({
#' 			if (debug) {
#' 				tagList(
#' 					actionButton(
#' 						session$ns("debug"),
#' 						"Debug",
#' 						icon = icon("bug"),
#' 						class = "btn-warning btn-sm"
#' 					),
#' 					hr()
#' 				)
#' 			}
#' 		})
#' 		
#' 		# Debug observer
#' 		if (debug) {
#' 			observeEvent(input$debug, {
#' 				message("\n═══════════════════════════════════════════════")
#' 				message("🔍 DEBUG MODE - ExpressionSet Subset Module")
#' 				message("═══════════════════════════════════════════════")
#' 				message("\nAvailable objects:")
#' 				message("  • eset() - Original ExpressionSet")
#' 				message("  • subset_eset() - Subsetted ExpressionSet")
#' 				message("  • selected_pdata_filters() - pData filters")
#' 				message("  • selected_fdata_filters() - fData filters")
#' 				message("\nUseful commands:")
#' 				message("  dim(eset())")
#' 				message("  dim(subset_eset())")
#' 				message("  selected_pdata_filters()")
#' 				message("═══════════════════════════════════════════════\n")
#' 				browser()
#' 			})
#' 		}
#' 		
#' 		# Store original and subset ExpressionSets
#' 		original_eset <- reactive({ eset() })
#' 		subset_eset <- reactiveVal(NULL)
#' 		
#' 		# Initialize with original
#' 		observe({
#' 			req(original_eset())
#' 			if (is.null(subset_eset())) {
#' 				subset_eset(original_eset())
#' 			}
#' 		})
#' 		
#' 		# Get pData columns
#' 		pdata_columns <- reactive({
#' 			req(original_eset())
#' 			colnames(Biobase::pData(original_eset()))
#' 		})
#' 		
#' 		# Get fData columns
#' 		fdata_columns <- reactive({
#' 			req(original_eset())
#' 			tryCatch({
#' 				fd <- Biobase::fData(original_eset())
#' 				if (!is.null(fd) && ncol(fd) > 0) {
#' 					colnames(fd)
#' 				} else {
#' 					NULL
#' 				}
#' 			}, error = function(e) NULL)
#' 		})
#' 		
#' 		# Storage for selected filters
#' 		selected_pdata_filters <- reactiveVal(list())
#' 		selected_fdata_filters <- reactiveVal(list())
#' 		
#' 		# Render pData subsetting UI
#' 		output$pdata_subset_ui <- renderUI({
#' 			req(pdata_columns())
#' 			
#' 			ns <- session$ns
#' 			cols <- pdata_columns()
#' 			
#' 			tagList(
#' 				selectInput(
#' 					ns("pdata_column"),
#' 					"Select pData Column:",
#' 					choices = c("None" = "", cols),
#' 					selected = ""
#' 				),
#' 				uiOutput(ns("pdata_values_ui")),
#' 				actionButton(
#' 					ns("add_pdata_filter"),
#' 					"Add Filter",
#' 					icon = icon("plus"),
#' 					class = "btn-primary btn-sm"
#' 				)
#' 			)
#' 		})
#' 		
#' 		# Render pData values based on selected column
#' 		output$pdata_values_ui <- renderUI({
#' 			req(input$pdata_column)
#' 			req(input$pdata_column != "")
#' 			
#' 			ns <- session$ns
#' 			col <- input$pdata_column
#' 			pdata <- Biobase::pData(original_eset())
#' 			
#' 			values <- unique(pdata[[col]])
#' 			values <- values[! is.na(values)]
#' 			
#' 			selectInput(
#' 				ns("pdata_values"),
#' 				paste("Select values to KEEP from", col, ":"),
#' 				choices = values,
#' 				selected = values,
#' 				multiple = TRUE
#' 			)
#' 		})
#' 		
#' 		# Add pData filter
#' 		observeEvent(input$add_pdata_filter, {
#' 			req(input$pdata_column)
#' 			req(input$pdata_values)
#' 			
#' 			current_filters <- selected_pdata_filters()
#' 			current_filters[[input$pdata_column]] <- input$pdata_values
#' 			selected_pdata_filters(current_filters)
#' 			
#' 			showNotification(
#' 				paste("Added filter:", input$pdata_column),
#' 				type = "message",
#' 				duration = 3
#' 			)
#' 		})
#' 		
#' 		# Render fData subsetting UI
#' 		output$fdata_subset_ui <- renderUI({
#' 			ns <- session$ns
#' 			
#' 			fcols <- fdata_columns()
#' 			
#' 			if (is.null(fcols) || length(fcols) == 0) {
#' 				return(p("No fData available for subsetting"))
#' 			}
#' 			
#' 			tagList(
#' 				selectInput(
#' 					ns("fdata_column"),
#' 					"Select fData Column:",
#' 					choices = c("None" = "", fcols),
#' 					selected = ""
#' 				),
#' 				uiOutput(ns("fdata_values_ui")),
#' 				actionButton(
#' 					ns("add_fdata_filter"),
#' 					"Add Filter",
#' 					icon = icon("plus"),
#' 					class = "btn-primary btn-sm"
#' 				)
#' 			)
#' 		})
#' 		
#' 		# Render fData values based on selected column
#' 		output$fdata_values_ui <- renderUI({
#' 			req(input$fdata_column)
#' 			req(input$fdata_column != "")
#' 			
#' 			ns <- session$ns
#' 			col <- input$fdata_column
#' 			fdata <- Biobase::fData(original_eset())
#' 			
#' 			values <- unique(fdata[[col]])
#' 			values <- values[!is.na(values)]
#' 			
#' 			selectInput(
#' 				ns("fdata_values"),
#' 				paste("Select values to KEEP from", col, ":"),
#' 				choices = values,
#' 				selected = values,
#' 				multiple = TRUE
#' 			)
#' 		})
#' 		
#' 		# Add fData filter
#' 		observeEvent(input$add_fdata_filter, {
#' 			req(input$fdata_column)
#' 			req(input$fdata_values)
#' 			
#' 			current_filters <- selected_fdata_filters()
#' 			current_filters[[input$fdata_column]] <- input$fdata_values
#' 			selected_fdata_filters(current_filters)
#' 			
#' 			showNotification(
#' 				paste("Added filter:", input$fdata_column),
#' 				type = "message",
#' 				duration = 3
#' 			)
#' 		})
#' 		
#' 		# pData filter summary
#' 		output$pdata_subset_summary <- renderPrint({
#' 			filters <- selected_pdata_filters()
#' 			
#' 			if (length(filters) == 0) {
#' 				cat("No pData filters applied\n")
#' 			} else {
#' 				cat("Active pData Filters:\n")
#' 				cat("═══════════════════════\n")
#' 				for (col in names(filters)) {
#' 					cat(sprintf("%s: %s\n", col, paste(filters[[col]], collapse = ", ")))
#' 				}
#' 			}
#' 		})
#' 		
#' 		# fData filter summary
#' 		output$fdata_subset_summary <- renderPrint({
#' 			filters <- selected_fdata_filters()
#' 			
#' 			if (length(filters) == 0) {
#' 				cat("No fData filters applied\n")
#' 			} else {
#' 				cat("Active fData Filters:\n")
#' 				cat("═══════════════════════\n")
#' 				for (col in names(filters)) {
#' 					cat(sprintf("%s: %s\n", col, paste(filters[[col]], collapse = ", ")))
#' 				}
#' 			}
#' 		})
#' 		
#' 		# Apply subset
#' 		observeEvent(input$apply_subset, {
#' 			req(original_eset())
#' 			
#' 			tryCatch({
#' 				ExpSet <- original_eset()
#' 				pdata_filters <- selected_pdata_filters()
#' 				fdata_filters <- selected_fdata_filters()
#' 				
#' 				# Apply pData filters (samples)
#' 				if (length(pdata_filters) > 0) {
#' 					pdata <- Biobase::pData(ExpSet)
#' 					keep_samples <- rep(TRUE, nrow(pdata))
#' 					
#' 					for (col in names(pdata_filters)) {
#' 						keep_samples <- keep_samples & (pdata[[col]] %in% pdata_filters[[col]])
#' 					}
#' 					
#' 					ExpSet <- ExpSet[, keep_samples]
#' 					message(sprintf("Kept %d / %d samples", sum(keep_samples), length(keep_samples)))
#' 				}
#' 				
#' 				# Apply fData filters (features)
#' 				if (length(fdata_filters) > 0) {
#' 					fdata <- Biobase::fData(ExpSet)
#' 					keep_features <- rep(TRUE, nrow(fdata))
#' 					
#' 					for (col in names(fdata_filters)) {
#' 						keep_features <- keep_features & (fdata[[col]] %in% fdata_filters[[col]])
#' 					}
#' 					
#' 					ExpSet <- ExpSet[keep_features, ]
#' 					message(sprintf("Kept %d / %d features", sum(keep_features), length(keep_features)))
#' 				}
#' 				
#' 				subset_eset(ExpSet)
#' 				
#' 				showNotification(
#' 					"✅ Subset applied successfully! ",
#' 					type = "message",
#' 					duration = 5
#' 				)
#' 				
#' 			}, error = function(e) {
#' 				showNotification(
#' 					paste("❌ Subset failed:", e$message),
#' 					type = "error",
#' 					duration = 10
#' 				)
#' 			})
#' 		})
#' 		
#' 		# Reset to original
#' 		observeEvent(input$reset_subset, {
#' 			subset_eset(original_eset())
#' 			selected_pdata_filters(list())
#' 			selected_fdata_filters(list())
#' 			
#' 			showNotification(
#' 				"Reset to original ExpressionSet",
#' 				type = "message",
#' 				duration = 3
#' 			)
#' 		})
#' 		
#' 		# Status display
#' 		output$subset_status <- renderUI({
#' 			req(original_eset())
#' 			req(subset_eset())
#' 			
#' 			orig_samples <- ncol(original_eset())
#' 			orig_features <- nrow(original_eset())
#' 			sub_samples <- ncol(subset_eset())
#' 			sub_features <- nrow(subset_eset())
#' 			
#' 			div(
#' 				class = "alert alert-info",
#' 				h4("Subset Status:"),
#' 				p(sprintf("Samples: %d / %d (%.1f%%)", 
#' 									sub_samples, orig_samples, 
#' 									100 * sub_samples / orig_samples)),
#' 				p(sprintf("Features: %d / %d (%.1f%%)", 
#' 									sub_features, orig_features, 
#' 									100 * sub_features / orig_features))
#' 			)
#' 		})
#' 		
#' 		# Return subset ExpressionSet
#' 		return(list(
#' 			subset_eset = subset_eset
#' 		))
#' 	})
#' }
#' #. ####
#' # eset_transform #####
#' #' ExpressionSet Transformation Module - UI
#' #'
#' #' Transform expression data (log, unlog, row-scale)
#' #'
#' #' @param id Module namespace ID
#' #' @param debug Show debug button
#' #' @export
mod_eset_transform_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Expression Data Transformation",
				width = 12,
				#status = "warning",
				#solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = T,
				
				p("Apply transformations to expression data matrix."),
				
				uiOutput(ns("debug_ui")),
				
				fluidRow(
					column(
						width = 6,
						h4("Select Transformation"),
						radioButtons(
							ns("transform_type"),
							"Transformation Type:",
							choices = c(
								"None (Original Data)" = "none",
								"Log2 Transform" = "log2",
								"Un-log (2^x)" = "unlog2",
								"Row Scale (Center)" = "rowscale",
								"Log2 + Row Scale" = "log2_rowscale"
							),
							selected = "none"
						),
						
						checkboxInput(
							ns("add_constant"),
							"Add constant before log (avoid log(0))",
							value = TRUE
						),
						
						conditionalPanel(
							condition = "input.add_constant == true",
							ns = ns,
							numericInput(
								ns("constant_value"),
								"Constant value:",
								value = 1,
								min = 0,
								step = 0.1
							)
						),
						
						hr(),
						
						actionButton(
							ns("apply_transform"),
							"Apply Transformation",
							icon = icon("magic"),
							class = "btn-warning btn-lg btn-block"
						),
						
						actionButton(
							ns("reset_transform"),
							"Reset to Original",
							icon = icon("undo"),
							class = "btn-secondary btn-block"
						)
					),
					column(
						width = 6,
						h4("Transformation Summary"),
						verbatimTextOutput(ns("transform_summary")),
						
						hr(),
						
						h4("Data Distribution"),
						plotOutput(ns("distribution_plot"), height = "300px")
					)
				)
			)
		)
	)
}

#' ExpressionSet Transformation Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet (input)
#' @param debug Enable debug mode
#' @export
mod_eset_transform_server <- function(id, eset, debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
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
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - ExpressionSet Transform Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Input ExpressionSet")
				message("  • transformed_eset() - Transformed ExpressionSet")
				message("  • input$transform_type - Selected transformation")
				message("\nUseful commands:")
				message("  range(Biobase::exprs(eset()))")
				message("  range(Biobase::exprs(transformed_eset()))")
				message("  input$transform_type")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Store original and transformed ExpressionSets
		original_eset <- reactive({ eset() })
		transformed_eset <- reactiveVal(NULL)
		transform_applied <- reactiveVal("none")
		
		# Initialize with original
		observe({
			req(original_eset())
			if (is.null(transformed_eset())) {
				transformed_eset(original_eset())
			}
		})
		
		# Apply transformation
		observeEvent(input$apply_transform, {
			req(original_eset())
			
			tryCatch({
				ExpSet <- original_eset()
				expr_data <- Biobase::exprs(ExpSet)
				transform_type <- input$transform_type
				
				message("Applying transformation: ", transform_type)
				
				# Apply selected transformation
				transformed_data <- switch(
					transform_type,
					"none" = expr_data,
					
					"log2" = {
						if (input$add_constant) {
							log2(expr_data + input$constant_value)
						} else {
							log2(expr_data)
						}
					},
					
					"unlog2" = {
						2^expr_data
					},
					
					"rowscale" = {
						row_scale_function(expr_data)
					},
					
					"log2_rowscale" = {
						logged <- if (input$add_constant) {
							log2(expr_data + input$constant_value)
						} else {
							log2(expr_data)
						}
						row_scale_function(logged)
					},
					
					expr_data  # default
				)
				
				# Create new ExpressionSet with transformed data
				transformed_ExpSet <- ExpSet
				Biobase::exprs(transformed_ExpSet) <- transformed_data
				
				# Add transformation note
				Biobase::notes(transformed_ExpSet) <- c(
					Biobase::notes(transformed_ExpSet),
					list(
						transformation = list(
							type = transform_type,
							constant_added = if (input$add_constant) input$constant_value else NULL,
							date = Sys.time()
						)
					)
				)
				
				transformed_eset(transformed_ExpSet)
				transform_applied(transform_type)
				
				showNotification(
					paste("✅ Transformation applied:", transform_type),
					type = "message",
					duration = 5
				)
				
			}, error = function(e) {
				showNotification(
					paste("❌ Transformation failed:", e$message),
					type = "error",
					duration = 10
				)
				message("Transformation error: ", e$message)
			})
		})
		
		# Reset to original
		observeEvent(input$reset_transform, {
			transformed_eset(original_eset())
			transform_applied("none")
			
			showNotification(
				"Reset to original data",
				type = "message",
				duration = 3
			)
		})
		
		# Transformation summary
		output$transform_summary <- renderPrint({
			req(original_eset())
			req(transformed_eset())
			
			orig_data <- Biobase::exprs(original_eset())
			trans_data <- Biobase::exprs(transformed_eset())
			
			cat("═══════════════════════════════════════\n")
			cat("TRANSFORMATION SUMMARY\n")
			cat("═══════════════════════════════════════\n\n")
			
			cat("Applied transformation:", transform_applied(), "\n\n")
			
			cat("Original data:\n")
			cat("  Range: ", sprintf("%.2f to %.2f", min(orig_data, na.rm = TRUE), max(orig_data, na.rm = TRUE)), "\n")
			cat("  Mean: ", sprintf("%.2f", mean(orig_data, na.rm = TRUE)), "\n")
			cat("  SD: ", sprintf("%.2f", sd(orig_data, na.rm = TRUE)), "\n\n")
			
			cat("Transformed data:\n")
			cat("  Range: ", sprintf("%. 2f to %.2f", min(trans_data, na.rm = TRUE), max(trans_data, na.rm = TRUE)), "\n")
			cat("  Mean: ", sprintf("%.2f", mean(trans_data, na.rm = TRUE)), "\n")
			cat("  SD: ", sprintf("%.2f", sd(trans_data, na.rm = TRUE)), "\n")
			
			cat("\n═══════════════════════════════════════\n")
		})
		
		# Distribution plot
		output$distribution_plot <- renderPlot({
			req(original_eset())
			req(transformed_eset())
			
			orig_data <- as.vector(Biobase::exprs(original_eset()))
			trans_data <- as.vector(Biobase::exprs(transformed_eset()))
			
			# Sample data if too large
			if (length(orig_data) > 10000) {
				idx <- sample(length(orig_data), 10000)
				orig_data <- orig_data[idx]
				trans_data <- trans_data[idx]
			}
			
			par(mfrow = c(1, 2))
			
			hist(orig_data, breaks = 50, main = "Original Data", 
					 xlab = "Expression Value", col = "lightblue")
			
			hist(trans_data, breaks = 50, main = "Transformed Data", 
					 xlab = "Expression Value", col = "lightcoral")
		})
		
		# Return transformed ExpressionSet
		return(list(
			transformed_eset = transformed_eset,
			transform_type = reactive(transform_applied())
		))
	})
}

