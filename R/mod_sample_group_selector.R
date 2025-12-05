#' Sample Group Column Selector - UI
#'
#' Select primary sample grouping column from phenoData
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_sample_group_selector_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				#title = "Sample Grouping Column",
				width = 12,
				#status = "primary",
				#solidHeader = TRUE,
				#p("Select the primary grouping variable for your samples (e.g., biological groups, conditions, treatments). "),
				uiOutput(ns("sample_group_selector_ui")),
				box(
					#title = '',
					width = 12,
					#status = "info",
					#solidHeader = TRUE,
					collapsible = TRUE,
					collapsed = TRUE,  # Starts folded
					
				
				#p("Select the primary grouping variable for your samples (e.g., biological groups, conditions, treatments). "),
				
				#uiOutput(ns("sample_group_selector_ui")),
				
				# Debug UI
				uiOutput(ns("debug_ui")),

					verbatimTextOutput(ns("group_summary"))
				)
			)
		)
	)
}

#' Sample Group Column Selector - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param default_column Character.  Default column name (e.g., "Labels")
#' @param debug Enable debug mode
#' @export
mod_sample_group_selector_server <- function(id, 
																						 eset, 
																						 default_column = "Labels",
																						 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		# Render debug button
		output$debug_ui <- renderUI({
			if (debug) {
				actionButton(
					session$ns("debug"),
					"Debug",
					icon = icon("bug"),
					class = "btn-warning btn-sm"
				)
			}
		})
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Sample Group Selector Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • input$sample_group_column - Selected column")
				message("\nUseful commands:")
				message("  input$sample_group_column")
				message("  colnames(Biobase::pData(eset()))")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Get all pData columns
		all_pdata_columns <- reactive({
			req(eset())
			colnames(Biobase::pData(eset()))
		})
		
		# Render sample group selector
		output$sample_group_selector_ui <- renderUI({
			req(all_pdata_columns())
			
			ns <- session$ns
			available_columns <- all_pdata_columns()
			
			# Use default if it exists, otherwise first column
			default_selection <- if (default_column %in% available_columns) {
				default_column
			} else {
				available_columns[1]
			}
			
			tagList(
				selectInput(
					ns("sample_group_column"),
					"Sample Grouping Column:",
					choices = available_columns,
					selected = default_selection,
					width = "100%"
				),
				#helpText("Primary biological grouping variable (e.g., disease status, treatment, condition).")
			)
		})
		
		# Group summary
		output$group_summary <- renderPrint({
			req(input$sample_group_column)
			req(eset())
			
			sample_group <- input$sample_group_column
			ExpSet <- eset()
			pdata <- Biobase::pData(ExpSet)
			
			cat("═══════════════════════════════════════════════\n")
			cat("SAMPLE GROUPING COLUMN\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			cat("Column:", sample_group, "\n\n")
			
			if (sample_group %in% colnames(pdata)) {
				groups <- table(pdata[[sample_group]], useNA = "ifany")
				cat("Number of Groups:", length(groups), "\n")
				cat("Total Samples:", sum(groups), "\n\n")
				
				cat("Group Distribution:\n")
				for (i in seq_along(groups)) {
					cat(sprintf("  %s: %d samples\n", names(groups)[i], groups[i]))
				}
			}
			
			cat("\n═══════════════════════════════════════════════\n")
		})
		
		# Return selected column
		return(list(
			selected_column = reactive({
				input$sample_group_column
			})
		))
	})
}