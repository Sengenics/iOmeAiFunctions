#' Example Shiny App for Limma Analysis
#'
#' Demonstrates usage of limma analysis modules from iOmeAiFunctions package
#'
#' @note
#' Version 1.0.0
#' File: examples/limma_app_example.R
#'
#' @examples
#' \dontrun{
#' # Run the app
#' shiny::runApp("path/to/limma_app_example.R")
#' }

library(shiny)
library(iOmeAiFunctions)  # Your package
library(Biobase)
library(limma)
library(ggplot2)
library(pheatmap)
library(DT)

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
	titlePanel("Limma Differential Expression Analysis"),
	
	sidebarLayout(
		sidebarPanel(
			width = 3,
			
			# Data loading section
			h3("Data Loading"),
			fileInput("eset_file", "Upload ExpressionSet RDS:",
								accept = ".rds"),
			
			# OR use ExpSet_list extractor
			fileInput("expset_list_file", "Upload ExpSet_list RDS:",
								accept = ".rds"),
			
			conditionalPanel(
				condition = "input.expset_list_file",
				selectInput("data_type", "Data type:",
										choices = c("clinical", "sample")),
				selectInput("normalisation", "Normalisation:",
										choices = c("loess_normalisation", "median_normalisation")),
				checkboxInput("use_combat", "Use ComBat", value = FALSE),
				selectInput("ncf_select", "Feature filter:",
										choices = c("all", "retain", "filter"),
										selected = "retain")
			),
			
			hr(),
			
			# Contrast configuration module
			h3("Analysis Configuration"),
			limmaContrastUI("limma_contrast"),
			
			hr(),
			
			# Feature selection
			h4("Feature Selection"),
			uiOutput("feature_select_ui")
		),
		
		mainPanel(
			width = 9,
			
			tabsetPanel(
				id = "results_tabs",
				
				# Results summary
				tabPanel("Summary",
								 limmaResultsUI("limma_summary")
				),
				
				# Volcano plot
				tabPanel("Volcano Plot",
								 limmaVolcanoUI("limma_volcano")
				),
				
				# Heatmap
				tabPanel("Heatmap",
								 limmaHeatmapUI("limma_heatmap")
				),
				
				# Violin plots
				tabPanel("Violin Plots",
								 limmaViolinUI("limma_violin")
				)
			)
		)
	)
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
	
	# Load ExpressionSet directly
	eSet_direct <- reactive({
		req(input$eset_file)
		readRDS(input$eset_file$datapath)
	})
	
	# OR extract from ExpSet_list
	eSet_from_list <- reactive({
		req(input$expset_list_file)
		
		ExpSet_list <- readRDS(input$expset_list_file$datapath)
		
		extracted <- ExpSet_list_extract_function(
			ExpSet_list = ExpSet_list,
			data = input$data_type,
			normalisation = input$normalisation,
			ComBat = input$use_combat,
			ncf_select = input$ncf_select
		)
		
		extracted$ExpSet
	})
	
	# Choose which ExpressionSet to use
	eSet <- reactive({
		if(!is.null(input$expset_list_file)){
			eSet_from_list()
		} else if(!is.null(input$eset_file)){
			eSet_direct()
		} else {
			NULL
		}
	})
	
	# Feature selection UI
	output$feature_select_ui <- renderUI({
		req(eSet())
		
		all_features <- rownames(exprs(eSet()))
		
		tagList(
			selectInput("feature_selection_method", 
									"Feature selection method:",
									choices = c("All features" = "all",
															"Top N by variance" = "variance",
															"Manual selection" = "manual")),
			
			conditionalPanel(
				condition = "input.feature_selection_method == 'variance'",
				numericInput("n_features", "Number of features:", 
										 value = min(500, length(all_features)), 
										 min = 10, 
										 max = length(all_features))
			),
			
			conditionalPanel(
				condition = "input.feature_selection_method == 'manual'",
				selectInput("manual_features", "Select features:",
										choices = all_features,
										multiple = TRUE,
										size = 10)
			)
		)
	})
	
	# Get selected features
	feature_select <- reactive({
		req(eSet())
		req(input$feature_selection_method)
		
		all_features <- rownames(exprs(eSet()))
		
		if(input$feature_selection_method == "all"){
			return(all_features)
		}
		
		if(input$feature_selection_method == "variance"){
			req(input$n_features)
			expr_data <- exprs(eSet())
			feature_var <- apply(expr_data, 1, var, na.rm = TRUE)
			top_features <- names(sort(feature_var, decreasing = TRUE)[1:input$n_features])
			return(top_features)
		}
		
		if(input$feature_selection_method == "manual"){
			req(input$manual_features)
			return(input$manual_features)
		}
		
		return(all_features)
	})
	
	# Run limma analysis using contrast module
	limma_results <- limmaContrastServer(
		id = "limma_contrast",
		eSet = eSet,
		feature_select = feature_select
	)
	
	# Results summary module
	limmaResultsServer(
		id = "limma_summary",
		limma_results = limma_results
	)
	
	# Volcano plot module
	limmaVolcanoServer(
		id = "limma_volcano",
		limma_results = limma_results
	)
	
	# Heatmap module
	limmaHeatmapServer(
		id = "limma_heatmap",
		limma_results = limma_results
	)
	
	# Violin plot module
	limmaViolinServer(
		id = "limma_violin",
		limma_results = limma_results
	)
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)