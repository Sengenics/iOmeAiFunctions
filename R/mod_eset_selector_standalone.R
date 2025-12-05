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
																						show_status = TRUE,
																						show_subset = TRUE,
																						show_transform = TRUE,
																						debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		# Compact selector with expandable details
		fluidRow(
			box(
				#title = "Select ExpressionSet",
				width = 12,
				#status = "info",
				#solidHeader = TRUE,
				
				# Selector dropdown
				column(11,
					mod_eset_selector_ui(ns("eset_select"))
				),
				
				column(1,
				
				# Collapsible details section
					tags$div(
						style = "text-align: right; margin-top: 5px;",
						actionLink(
							ns("toggle_details"),
							icon("info-circle", class = "fa-lg"),
							style = "color: #337ab7;"
						)
					)
				),
				column(12,
				conditionalPanel(
					condition = "input.toggle_details % 2 == 1",
					ns = ns,
					hr(),
					#fluidRow(

			
			if (show_summary) {
				fluidRow(
					width = 12,
					box(
						title = "Data Summary",
						width = NULL,
						#status = "success",
						#solidHeader = TRUE,
						collapsible = T,
						collapsed = T,
						
						verbatimTextOutput(ns("eset_summary"))
					)
				)
			},
		
		
		if (show_status) {
			fluidRow(
				box(
					title = "Data Status",
					width = 12,
					#status = "info",
					collapsible = T,
					collapsed = T,
					
					fluidRow(
						column(width = 3, valueBoxOutput(ns("status_eset_list"), width = NULL)),
						column(width = 3, valueBoxOutput(ns("status_selected"), width = NULL)),
						column(width = 3, valueBoxOutput(ns("status_samples"), width = NULL)),
						column(width = 3, valueBoxOutput(ns("status_features"), width = NULL))
					)
				)
			)
			
		},
		# Subset Section
		if (show_subset) {
			mod_eset_subset_ui(ns("subset"), debug = debug)
		},
		
		# Transform Section
		if (show_transform) {
			mod_eset_transform_ui(ns("transform"), debug = debug)
		}
					)
				)
			)
		)
	)
	#)
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
mod_eset_selector_standalone_server <- function(id, 
																								ExpSet_list,
																								default_selection = NULL,
																								source = reactive("unknown"),
																								enable_subset = TRUE,
																								enable_transform = TRUE,
																								debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
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
		
		# ============================================
		# 2.  Subset Module (optional)
		# ============================================
		if (enable_subset) {
			subset_module <- mod_eset_subset_server(
				"subset",
				eset = eset_selected,
				debug = debug
			)
			
			# Reset subset when new data is loaded
			observe({
				req(eset_selected())
				subset_module$subset_eset(eset_selected())
			}, priority = 100)
			
			eset_after_subset <- subset_module$subset_eset
		} else {
			eset_after_subset <- eset_selected
		}
		
		# ============================================
		# 3.  Transform Module (optional)
		# ============================================
		if (enable_transform) {
			transform_module <- mod_eset_transform_server(
				"transform",
				eset = eset_after_subset,
				debug = debug
			)
			
			# Reset transform when subset changes
			observe({
				req(eset_after_subset())
				transform_module$transformed_eset(eset_after_subset())
			}, priority = 100)
			
			final_eset <- transform_module$transformed_eset
		} else {
			final_eset <- eset_after_subset
		}
		
		# Status value boxes
		output$status_eset_list <- renderValueBox({
			if (! is.null(ExpSet_list())) {
				source_label <- switch(
					source(),
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
					subtitle = "No ExpSets Available",
					icon = icon("exclamation-triangle"),
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
		
		# Return values
		
		# ============================================
		# Return Values
		# ============================================
		return(list(
			assay_names = eset_selected_module$assay_names,
			# Final processed data
			
			eset = final_eset,
			eset_name = eset_selected_module$name,
			
			# Intermediate stages (if needed)
			eset_original = eset_selected,
			eset_subset = if (enable_subset) eset_after_subset else NULL,
			
			# Access to sub-modules (if needed)
			subset_module = if (enable_subset) subset_module else NULL,
			transform_module = if (enable_transform) transform_module else NULL
		))
		# return(list(
		# 	eset = eset_selected,
		# 	eset_name = eset_selected_module$name
		# ))
	})
}