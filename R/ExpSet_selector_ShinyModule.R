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
			# ExpSets <- names(ExpSet_list())
			# name_list <- list()
			# 
			# for (entry in ExpSets) {
			# 	entries <- names(ExpSet_list()[[entry]]@assayData)
			# 	name_list[[entry]] <- entries
			# }
			# 
			return(name_list)
		})
		
		# Render the selection UI
		output$eset_select_ui <- renderUI({
			req(ExpSet_names())
			
			choices <- ExpSet_names()
			
			# Try to select default, otherwise first available
			if (default_selection %in% unlist(choices)) {
				selected <- default_selection
			} else {
				selected <- unlist(choices)[1]
			}
			
			selectInput(
				ns("eset_select"),
				"Select Expression Data:",
				choices = choices,
				selected = selected
			)
		})
		
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

