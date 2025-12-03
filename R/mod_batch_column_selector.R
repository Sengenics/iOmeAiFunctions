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
				title = "Select Columns for Batch Testing",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				
				p("Select which phenoData columns to test for batch effects."),
				p("Only columns with useful grouping patterns (from Annotation Analysis) are shown."),
				
				uiOutput(ns("column_selector_ui")),
				
				if (debug) {
					hr()
					actionButton(
						ns("debug"),
						"Debug",
						icon = icon("bug"),
						class = "btn-warning btn-sm"
					)
				},
				
				hr(),
				
				h4("Selected Columns Summary"),
				verbatimTextOutput(ns("selection_summary"))
			)
		)
	)
}

#' Batch Testing Column Selector - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param useful_columns Reactive vector of useful column names from annotation analysis
#' @param default_columns Reactive or static vector of default selections
#' @param debug Enable debug mode
#' @export
mod_batch_column_selector_server <- function(id, 
																						 eset, 
																						 useful_columns, 
																						 default_columns = NULL,
																						 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Batch Column Selector Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • useful_columns() - Useful column names")
				message("  • input$batch_columns - Selected columns")
				message("\nUseful commands:")
				message("  useful_columns()")
				message("  input$batch_columns")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Get default selections
		default_selections <- reactive({
			if (is.reactive(default_columns)) {
				default_columns()
			} else if (! is.null(default_columns)) {
				default_columns
			} else {
				NULL
			}
		})
		
		# Render column selector
		output$column_selector_ui <- renderUI({
			req(useful_columns())
			
			ns <- session$ns
			available_columns <- useful_columns()
			defaults <- default_selections()
			
			# Ensure defaults are in available columns
			if (! is.null(defaults)) {
				defaults <- intersect(defaults, available_columns)
			}
			
			tagList(
				selectInput(
					ns("batch_columns"),
					"Batch Testing Columns:",
					choices = available_columns,
					selected = defaults,
					multiple = TRUE,
					width = "100%"
				),
				helpText(
					"Select multiple columns to test for batch effects.  ",
					"These columns were identified as having useful grouping patterns."
				)
			)
		})
		
		# Selection summary
		output$selection_summary <- renderPrint({
			req(input$batch_columns)
			req(eset())
			
			selected <- input$batch_columns
			ExpSet <- eset()
			pdata <- Biobase::pData(ExpSet)
			
			cat("═══════════════════════════════════════════════\n")
			cat("BATCH TESTING COLUMN SELECTION\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			cat("Number of columns selected:", length(selected), "\n\n")
			
			for (col in selected) {
				if (col %in% colnames(pdata)) {
					groups <- table(pdata[[col]], useNA = "ifany")
					cat("Column:", col, "\n")
					cat("  Groups:", length(groups), "\n")
					cat("  Sample distribution:", paste(names(groups), "=", groups, collapse = ", "), "\n")
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