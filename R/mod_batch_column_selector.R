#' Batch Testing Column Selector - UI
#'
#' Select columns for batch effect testing
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_batch_column_selector_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Batch Testing Columns",
				width = 12,
				status = "warning",
				solidHeader = TRUE,
				
				#p("Select which phenoData columns to test for batch effects."),
				#p("Only columns with useful grouping patterns (from Annotation Analysis) are shown."),
				
				uiOutput(ns("batch_column_selector_ui")),
				box(
					title = '',
					width = 12,
					#status = "info",
					#solidHeader = TRUE,
					collapsible = TRUE,
					collapsed = TRUE,  # Starts folded
				
					# Debug UI
					uiOutput(ns("debug_ui")),
					
					hr(),
					
					h4("Selected Columns Summary"),
					verbatimTextOutput(ns("selection_summary"))
				)
			)
		)
	)
}

#' Batch Testing Column Selector - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param filtered_columns Reactive vector of filtered column names from annotation analysis
#' @param default_columns Reactive or static vector of default selections
#' @param debug Enable debug mode
#' @export
mod_batch_column_selector_server <- function(id, 
																						 eset, 
																						 filtered_columns,
																						 default_columns = NULL,
																						 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		# Render debug button - ALWAYS render the UI
		output$debug_ui <- renderUI({
			message("Debug parameter value: ", debug)  # Diagnostic
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
			} else {
				NULL
			}
		})
		
		# Debug observer
		observeEvent(input$debug, {
			message("\n═══════════════════════════════════════════════")
			message("🔍 DEBUG MODE - Batch Column Selector Module")
			message("═══════════════════════════════════════════════")
			message("\nAvailable objects:")
			message("  • eset() - Selected ExpressionSet")
			message("  • filtered_columns() - Filtered column names")
			message("  • input$batch_columns - Selected columns")
			message("\nUseful commands:")
			message("  filtered_columns()")
			message("  length(filtered_columns())")
			message("  input$batch_columns")
			message("═══════════════════════════════════════════════\n")
			browser()
		})
		
		# Get default selections
		default_selections <- reactive({
			if (is.reactive(default_columns)) {
				default_columns()
			} else if (!  is.null(default_columns)) {
				default_columns
			} else {
				NULL
			}
		})
		
		# Render batch column selector
		output$batch_column_selector_ui <- renderUI({
			ns <- session$ns
			
			# Check if filtered_columns is available
			if (is.null(filtered_columns)) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No filtered columns available"),
						p("Please complete the Annotation Analysis first.")
					)
				)
			}
			
			cols <- tryCatch({
				filtered_columns()
			}, error = function(e) {
				message("Error getting filtered_columns: ", e$message)
				NULL
			})
			
			# Check if columns were returned
			if (is.null(cols) || length(cols) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No columns available for batch testing"),
						p("Run Annotation Analysis to identify suitable columns, or check that your data has valid grouping columns.")
					)
				)
			}
			
			available_columns <- cols
			defaults <- default_selections()
			
			# Ensure defaults are in available columns
			if (!is.null(defaults)) {
				defaults <- intersect(defaults, available_columns)
			}
			
			tagList(
				#p(strong(sprintf("%d columns available for batch testing", length(available_columns)))),
				selectInput(
					ns("batch_columns"),
					"Batch Testing Columns:",
					choices = available_columns,
					selected = defaults,
					multiple = TRUE,
					width = "100%"
				),
				#helpText(
				#	"Select multiple columns to test for batch effects.  ",
				#	"These columns passed filtering criteria (2+ samples per group, 2+ groups)."
				#)
			)
		})
		
		# Selection summary
		output$selection_summary <- renderPrint({
			req(input$batch_columns)
			req(eset())
			
			batch_cols <- input$batch_columns
			ExpSet <- eset()
			pdata <- Biobase::pData(ExpSet)
			
			cat("═══════════════════════════════════════════════\n")
			cat("BATCH TESTING COLUMNS\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			cat("Number of columns selected:", length(batch_cols), "\n\n")
			
			for (col in batch_cols) {
				if (col %in% colnames(pdata)) {
					groups <- table(pdata[[col]], useNA = "ifany")
					cat("Column:", col, "\n")
					cat("  Groups:", length(groups), "\n")
					cat("  Distribution:", paste(names(groups), "=", groups, collapse = ", "), "\n")
					cat("\n")
				}
			}
			
			cat("═══════════════════════════════════════════════\n")
		})
		
		# Return selected columns
		return(list(
			selected_columns = reactive({
				input$batch_columns
			})
		))
	})
}