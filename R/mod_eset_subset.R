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
				#status = "primary",
				#solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = T,
				
				p("Filter samples or features based on metadata columns. "),
				
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
			values <- values[! is.na(values)]
			
			selectInput(
				ns("pdata_values"),
				paste("Select values to KEEP from", col, ":"),
				choices = values,
				selected = values,
				multiple = TRUE
			)
		})
		
		# Add pData filter
		observeEvent(input$add_pdata_filter, {
			req(input$pdata_column)
			req(input$pdata_values)
			
			current_filters <- selected_pdata_filters()
			current_filters[[input$pdata_column]] <- input$pdata_values
			selected_pdata_filters(current_filters)
			
			showNotification(
				paste("Added filter:", input$pdata_column),
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
			
			selectInput(
				ns("fdata_values"),
				paste("Select values to KEEP from", col, ":"),
				choices = values,
				selected = values,
				multiple = TRUE
			)
		})
		
		# Add fData filter
		observeEvent(input$add_fdata_filter, {
			req(input$fdata_column)
			req(input$fdata_values)
			
			current_filters <- selected_fdata_filters()
			current_filters[[input$fdata_column]] <- input$fdata_values
			selected_fdata_filters(current_filters)
			
			showNotification(
				paste("Added filter:", input$fdata_column),
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
					cat(sprintf("%s: %s\n", col, paste(filters[[col]], collapse = ", ")))
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
					cat(sprintf("%s: %s\n", col, paste(filters[[col]], collapse = ", ")))
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
				
				# Apply pData filters (samples)
				if (length(pdata_filters) > 0) {
					pdata <- Biobase::pData(ExpSet)
					keep_samples <- rep(TRUE, nrow(pdata))
					
					for (col in names(pdata_filters)) {
						keep_samples <- keep_samples & (pdata[[col]] %in% pdata_filters[[col]])
					}
					
					ExpSet <- ExpSet[, keep_samples]
					message(sprintf("Kept %d / %d samples", sum(keep_samples), length(keep_samples)))
				}
				
				# Apply fData filters (features)
				if (length(fdata_filters) > 0) {
					fdata <- Biobase::fData(ExpSet)
					keep_features <- rep(TRUE, nrow(fdata))
					
					for (col in names(fdata_filters)) {
						keep_features <- keep_features & (fdata[[col]] %in% fdata_filters[[col]])
					}
					
					ExpSet <- ExpSet[keep_features, ]
					message(sprintf("Kept %d / %d features", sum(keep_features), length(keep_features)))
				}
				
				subset_eset(ExpSet)
				
				showNotification(
					"✅ Subset applied successfully! ",
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