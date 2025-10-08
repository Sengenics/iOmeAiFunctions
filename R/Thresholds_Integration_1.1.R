# Threshold Tool Module for iOmeAiFunctions Package
# Single-function integration for main app
#
# Usage in your main app after loading the package:
# library(iOmeAiFunctions)
# UI:     thresholdToolUI()
# Server: thresholdToolServer(input, output, session)

library(shiny)
library(plotly)
library(dplyr)

#' Threshold Tool UI
#'
#' Returns a complete tabPanel for Product Thresholds with three sub-tabs:
#' Product Values, Protein Plot, and Spot Plot
#'
#' @return A tabPanel object containing the complete threshold tool interface
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   tabsetPanel(
#'     tabPanel("Dashboard", ...),
#'     thresholdToolUI()
#'   )
#' )
#' }
#' @export
thresholdToolUI <- function() {
	tabPanel("Product Thresholds",
					 fluidRow(
					 	column(12,
					 				 wellPanel(
					 				 	h4("Global Preset Configuration"),
					 				 	selectInput("threshold_preset", "Load Preset:",
					 				 							choices = c("v2 Thresholds", "2025 Transition", "2023 Thresholds"),
					 				 							selected = "2025 Transition", width = "300px")
					 				 )
					 	)
					 ),

					 tabsetPanel(
					 	tabPanel("Product Values",
					 					 br(),
					 					 fluidRow(
					 					 	column(4,
					 					 				 wellPanel(
					 					 				 	h4("Product Positions"),
					 					 				 	p(em("Spots = Proteins × Replicates")),
					 					 				 	hr(),
					 					 				 	h5("PAI"),
					 					 				 	numericInput("pai_proteins", "# Proteins:", value = 114, min = 0, max = 5000, step = 1),
					 					 				 	numericInput("pai_replicates", "# Replicates:", value = 4, min = 1, max = 10, step = 1),
					 					 				 	p(strong(textOutput("pai_spots_calc", inline = TRUE))),
					 					 				 	hr(),
					 					 				 	h5("CTA"),
					 					 				 	numericInput("cta_proteins", "# Proteins:", value = 270, min = 0, max = 5000, step = 1),
					 					 				 	numericInput("cta_replicates", "# Replicates:", value = 4, min = 1, max = 10, step = 1),
					 					 				 	p(strong(textOutput("cta_spots_calc", inline = TRUE))),
					 					 				 	hr(),
					 					 				 	h5("i-Ome Cancer"),
					 					 				 	numericInput("iome_cancer_proteins", "# Proteins:", value = 533, min = 0, max = 5000, step = 1),
					 					 				 	numericInput("iome_cancer_replicates", "# Replicates:", value = 3, min = 1, max = 10, step = 1),
					 					 				 	p(strong(textOutput("iome_cancer_spots_calc", inline = TRUE))),
					 					 				 	hr(),
					 					 				 	h5("i-Ome Discovery v1"),
					 					 				 	numericInput("iome_discovery_proteins", "# Proteins:", value = 1857, min = 0, max = 5000, step = 1),
					 					 				 	numericInput("iome_discovery_replicates", "# Replicates:", value = 4, min = 1, max = 10, step = 1),
					 					 				 	p(strong(textOutput("iome_discovery_spots_calc", inline = TRUE))),
					 					 				 	hr(),
					 					 				 	h5("i-Ome Discovery v2"),
					 					 				 	numericInput("iome_discovery_v2_proteins", "# Proteins:", value = 2500, min = 0, max = 5000, step = 1),
					 					 				 	numericInput("iome_discovery_v2_replicates", "# Replicates:", value = 4, min = 1, max = 10, step = 1),
					 					 				 	p(strong(textOutput("iome_discovery_v2_spots_calc", inline = TRUE))),
					 					 				 	hr(),
					 					 				 	h5("Custom"),
					 					 				 	numericInput("custom_proteins", "# Proteins:", value = NA, min = 0, max = 5000, step = 1),
					 					 				 	numericInput("custom_replicates", "# Replicates:", value = 4, min = 1, max = 10, step = 1),
					 					 				 	p(strong(textOutput("custom_spots_calc", inline = TRUE))),
					 					 				 	hr(),
					 					 				 	actionButton("reset_products", "Reset Products to Defaults", class = "btn-warning btn-block")
					 					 				 )
					 					 	),
					 					 	column(4, h4("Protein Thresholds"), tableOutput("protein_table")),
					 					 	column(4, h4("Spot Thresholds"), tableOutput("spot_table"))
					 					 )
					 	),
					 	tabPanel("Protein Plot", proteinsUI("proteins")),
					 	tabPanel("Spot Plot", spotsUI("spots"))
					 )
	)
}

#' Threshold Tool Server Logic
#'
#' Provides all server-side logic for the threshold visualization tool.
#' Must be called within your main server function.
#'
#' @param input Shiny input object from the main server
#' @param output Shiny output object from the main server
#' @param session Shiny session object from the main server
#' @return NULL (called for side effects)
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   # Your existing server logic
#'
#'   # Add threshold tool
#'   thresholdToolServer(input, output, session)
#'
#'   # More of your server logic
#' }
#' }
#' @export
thresholdToolServer <- function(input, output, session) {

	current_preset <- reactive({
		req(input$threshold_preset)
		input$threshold_preset
	})

	product_positions <- reactive({
		list(
			proteins = list(
				pai = input$pai_proteins,
				cta = input$cta_proteins,
				iome_cancer = input$iome_cancer_proteins,
				iome_discovery = input$iome_discovery_proteins,
				iome_discovery_v2 = input$iome_discovery_v2_proteins,
				custom = input$custom_proteins
			),
			spots = list(
				pai = input$pai_proteins * input$pai_replicates,
				cta = input$cta_proteins * input$cta_replicates,
				iome_cancer = input$iome_cancer_proteins * input$iome_cancer_replicates,
				iome_discovery = input$iome_discovery_proteins * input$iome_discovery_replicates,
				iome_discovery_v2 = input$iome_discovery_v2_proteins * input$iome_discovery_v2_replicates,
				custom = if(!is.na(input$custom_proteins)) input$custom_proteins * input$custom_replicates else NA
			)
		)
	})

	output$pai_spots_calc <- renderText({ paste("Total Spots:", input$pai_proteins * input$pai_replicates) })
	output$cta_spots_calc <- renderText({ paste("Total Spots:", input$cta_proteins * input$cta_replicates) })
	output$iome_cancer_spots_calc <- renderText({ paste("Total Spots:", input$iome_cancer_proteins * input$iome_cancer_replicates) })
	output$iome_discovery_spots_calc <- renderText({ paste("Total Spots:", input$iome_discovery_proteins * input$iome_discovery_replicates) })
	output$iome_discovery_v2_spots_calc <- renderText({ paste("Total Spots:", input$iome_discovery_v2_proteins * input$iome_discovery_v2_replicates) })
	output$custom_spots_calc <- renderText({
		if(!is.na(input$custom_proteins)) {
			paste("Total Spots:", input$custom_proteins * input$custom_replicates)
		} else {
			"Total Spots: -"
		}
	})

	observeEvent(input$reset_products, {
		updateNumericInput(session, "pai_proteins", value = 114)
		updateNumericInput(session, "pai_replicates", value = 4)
		updateNumericInput(session, "cta_proteins", value = 270)
		updateNumericInput(session, "cta_replicates", value = 4)
		updateNumericInput(session, "iome_cancer_proteins", value = 533)
		updateNumericInput(session, "iome_cancer_replicates", value = 3)
		updateNumericInput(session, "iome_discovery_proteins", value = 1857)
		updateNumericInput(session, "iome_discovery_replicates", value = 4)
		updateNumericInput(session, "iome_discovery_v2_proteins", value = 2500)
		updateNumericInput(session, "iome_discovery_v2_replicates", value = 4)
		updateNumericInput(session, "custom_proteins", value = NA)
		updateNumericInput(session, "custom_replicates", value = 4)
	})

	protein_products <- proteinsServer("proteins", current_preset, product_positions)
	spot_products <- spotsServer("spots", current_preset, product_positions)

	output$protein_table <- renderTable({
		req(protein_products())
		products <- protein_products()
		if (nrow(products) == 0) return(data.frame(Message = "No products configured"))
		data.frame(
			Product = products$Product,
			`Total Proteins` = products$x_pos,
			`% Failing` = sprintf("%.2f%%", products$pct_fail),
			`# Failing` = sprintf("%.0f", products$n_fail),
			check.names = FALSE,
			stringsAsFactors = FALSE
		)
	}, striped = TRUE, hover = TRUE, bordered = TRUE)

	output$spot_table <- renderTable({
		req(spot_products())
		products <- spot_products()
		if (nrow(products) == 0) return(data.frame(Message = "No products configured"))
		data.frame(
			Product = products$Product,
			`Total Spots` = products$x_pos,
			`% Failing` = sprintf("%.2f%%", products$pct_fail),
			`# Failing` = sprintf("%.0f", products$n_fail),
			check.names = FALSE,
			stringsAsFactors = FALSE
		)
	}, striped = TRUE, hover = TRUE, bordered = TRUE)
}
