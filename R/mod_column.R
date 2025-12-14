#' Generic Column Selector - UI
#'
#' Reusable column selector for phenoData
#'
#' @param id Module namespace ID
#' @param label Label for the selector (e. g., "Sample Grouping Column", "Batch Testing Columns")
#' @param help_text Optional help text
#' @param show_summary Show summary box (default TRUE)
#' @param show_info Show info bubble (default TRUE)
#' @param debug Show debug button
#' @export
mod_column_selector_ui <- function(id, 
																	 label = "Select Column",
																	 help_text = NULL,
																	 show_summary = TRUE,
																	 show_info = TRUE,
																	 debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		# ✅ Selector with optional info icon
		fluidRow(
			column(
				width = if (show_info) 10 else 12,
				uiOutput(ns("column_selector_ui"))
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
							
							# Column Summary
							if (show_summary) {
								box(
									title = "Column Summary",
									width = 12,
									collapsible = TRUE,
									collapsed = TRUE,
									
									verbatimTextOutput(ns("column_summary"))
								)
							},
							
							# Debug button
							if (debug) {
								fluidRow(
									column(
										width = 12,
										style = "margin-top: 10px;",
										actionButton(
											ns("debug"),
											"Debug:  mod_column_selector",
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
		}
	)
}

#' Generic Column Selector - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param available_columns Reactive vector of available column names (optional - uses all pData columns if NULL)
#' @param default_columns Default selection(s) - character vector or reactive
#' @param multiple Allow multiple selection (default FALSE)
#' @param label Label for the selector
#' @param help_text Help text to display
#' @param debug Enable debug mode
#' @export
mod_column_selector_server <- function(id,
																			 eset,
																			 available_columns = NULL,
																			 default_columns = NULL,
																			 multiple = FALSE,
																			 label = "Select Column:",
																			 help_text = NULL,
																			 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# ✅ Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("🔍 DEBUG MODE - mod_column_selector")
				message("  • Label:  ", label)
				message("  • Multiple: ", multiple)
				message("  • Selected:  ", paste(input$selected_columns, collapse = ", "))
				browser()
			})
		}
		
		# Get available columns
		columns_to_show <- reactive({
			req(eset())
			
			# If available_columns is provided, use it (with reactive handling)
			if (!is.null(available_columns)) {
				cols <- if (is.reactive(available_columns)) {
					available_columns()
				} else {
					available_columns
				}
				
				if (is.null(cols) || length(cols) == 0) {
					return(character(0))
				}
				
				return(cols)
			}
			
			# Otherwise, use all pData columns
			colnames(Biobase::pData(eset()))
		})
		
		# Get default selections
		default_selections <- reactive({
			cols_available <- columns_to_show()
			
			if (length(cols_available) == 0) {
				return(NULL)
			}
			
			# Handle reactive or static defaults
			defaults <- if (is.reactive(default_columns)) {
				default_columns()
			} else {
				default_columns
			}
			
			if (is.null(defaults)) {
				# Auto-select first column if no default
				return(cols_available[1])
			}
			
			# Ensure defaults are in available columns
			intersect(defaults, cols_available)
		})
		
		# Render selector
		output$column_selector_ui <- renderUI({
			cols <- columns_to_show()
			
			if (length(cols) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No columns available"),
						p("No suitable columns found in the data.")
					)
				)
			}
			
			defaults <- default_selections()
			
			tagList(
				selectInput(
					ns("selected_columns"),
					label,
					choices = cols,
					selected = defaults,
					multiple = multiple,
					width = "100%"
				),
				if (!is.null(help_text)) {
					helpText(help_text)
				}
			)
		})
		
		# Column summary
		output$column_summary <- renderPrint({
			req(input$selected_columns)
			req(eset())
			
			selected <- input$selected_columns
			ExpSet <- eset()
			pdata <- Biobase::pData(ExpSet)
			
			cat("═══════════════════════════════════════════════\n")
			cat("SELECTED COLUMN", if (multiple) "S", "\n", sep = "")
			cat("═══════════════════════════════════════════════\n\n")
			
			if (multiple) {
				cat("Number of columns selected:", length(selected), "\n\n")
			}
			
			for (col in selected) {
				if (col %in% colnames(pdata)) {
					groups <- table(pdata[[col]], useNA = "ifany")
					cat("Column:", col, "\n")
					cat("  Number of Groups:", length(groups), "\n")
					cat("  Total Samples:", sum(groups), "\n")
					cat("  Distribution:\n")
					for (i in seq_along(groups)) {
						cat(sprintf("    %s: %d samples\n", names(groups)[i], groups[i]))
					}
					cat("\n")
				}
			}
			
			cat("═══════════════════════════════════════════════\n")
		})
		
		# Return selected column(s)
		return(list(
			selected_columns = reactive({ input$selected_columns }),
			# Alias for single-selection compatibility
			selected_column = reactive({ input$selected_columns })
		))
	})
}
