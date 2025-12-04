#' ExpressionSet Transformation Module - UI
#'
#' Transform expression data (log, unlog, row-scale)
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
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