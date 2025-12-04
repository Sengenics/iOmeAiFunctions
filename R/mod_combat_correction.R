#' ComBat Batch Correction Module - UI
#'
#' Select batch factors and run ComBat correction
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_combat_correction_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "ComBat Batch Correction",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Select batch factors to correct using ComBat. "),
				p(strong("Only 'safe' factors are shown:"), "Strong batch effect (p < 0.05) and NOT confounded with sample groups (p > 0.05). "),
				
				uiOutput(ns("debug_ui")),
				
				uiOutput(ns("combat_selector_ui")),
				
				hr(),
				
				fluidRow(
					column(
						width = 6,
						h4("ComBat Settings"),
						selectInput(
							ns("par_prior"),
							"Parametric Prior:",
							choices = c("Parametric" = TRUE, "Non-parametric" = FALSE),
							selected = TRUE
						),
						helpText("Parametric is faster and works well for most cases.   Use non-parametric for non-normal data.")
					),
					column(
						width = 6,
						h4("Run Correction"),
						actionButton(
							ns("run_combat"),
							"Run ComBat Correction",
							icon = icon("play"),
							class = "btn-success btn-lg"
						),
						br(), br(),
						uiOutput(ns("correction_status"))
					)
				)
			)
		),
		
		fluidRow(
			box(
				title = "Correction Results",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				verbatimTextOutput(ns("correction_summary")),
				
				hr(),
				
				h4("Download Corrected Data"),
				downloadButton(ns("download_corrected_eset"), "Download Corrected ExpressionSet (. rds)"),
				downloadButton(ns("download_corrected_csv"), "Download Corrected Matrix (.csv)")
			)
		)
	)
}

#' ComBat Batch Correction Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param sample_group_column Reactive character.   Sample grouping column (to preserve)
#' @param combined_results Reactive data. frame.  Results from combined batch analysis
#' @param debug Enable debug mode
#' @export
mod_combat_correction_server <- function(id,
																				 eset,
																				 sample_group_column,
																				 combined_results,
																				 debug = FALSE) {
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
				message("🔍 DEBUG MODE - ComBat Correction Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • sample_group_column() - Sample grouping")
				message("  • safe_batch_factors() - Safe factors to correct")
				message("  • corrected_eset() - Corrected ExpressionSet")
				message("\nUseful commands:")
				message("  safe_batch_factors()")
				message("  input$batch_factor")
				message("  str(corrected_eset())")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Identify safe batch factors
		safe_batch_factors <- reactive({
			req(combined_results())
			
			df <- combined_results()
			
			# Filter for: ANOVA p < 0.05 (batch effect) AND Fisher p > 0.05 (not confounded)
			safe <- df %>%
				filter(ANOVA_p_value < 0.05, Fisher_p_value > 0.05) %>%
				arrange(ANOVA_p_value) %>%  # Sort by strongest batch effect
				pull(Batch_Column)
			
			as.character(safe)
		})
		
		# Render batch factor selector
		output$combat_selector_ui <- renderUI({
			ns <- session$ns
			
			safe_factors <- safe_batch_factors()
			
			if (is.null(safe_factors) || length(safe_factors) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No safe batch factors identified"),
						p("All tested batch factors are either:"),
						tags$ul(
							tags$li("Not significant (no batch effect), or"),
							tags$li("Confounded with sample groups (unsafe to correct)")
						),
						p("Run the Combined Batch Analysis first, or adjust your batch column selection.")
					)
				)
			}
			
			tagList(
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(sprintf(" %d safe batch factor(s) identified", length(safe_factors)))
				),
				
				selectInput(
					ns("batch_factor"),
					"Select Batch Factor to Correct:",
					choices = safe_factors,
					selected = safe_factors[1]  # Default to strongest effect
				),
				
				helpText(
					"Listed in order of batch effect strength (strongest first).   ",
					"ComBat will correct for this factor while preserving biological variation."
				)
			)
		})
		
		# Store corrected ExpressionSet
		corrected_eset <- reactiveVal(NULL)
		
		# Run ComBat correction
		observeEvent(input$run_combat, {
			req(eset())
			req(input$batch_factor)
			req(sample_group_column())
			
			# Show progress
			showNotification("Running ComBat correction.. .", type = "message", duration = NULL, id = "combat_progress")
			
			tryCatch({
				ExpSet <- eset()
				batch_factor <- input$batch_factor
				sample_group <- sample_group_column()
				par_prior <- as.logical(input$par_prior)
				
				# Get data
				expr_data <- Biobase::exprs(ExpSet)
				meta <- Biobase::pData(ExpSet)
				
				# Validate
				if (! batch_factor %in% colnames(meta)) {
					stop("Batch factor not found in metadata")
				}
				
				if (! sample_group %in% colnames(meta)) {
					stop("Sample group column not found in metadata")
				}
				
				# Prepare batch and mod
				batch <- meta[[batch_factor]]
				mod <- model.matrix(~ as.factor(meta[[sample_group]]))
				
				# Run ComBat
				message("Running ComBat correction...")
				message("  Batch factor: ", batch_factor)
				message("  Preserving: ", sample_group)
				message("  Parametric prior: ", par_prior)
				
				corrected_data <- sva::ComBat(
					dat = expr_data,
					batch = batch,
					mod = mod,
					par.prior = par_prior
				)
				
				# Create new ExpressionSet with corrected data
				corrected_ExpSet <- ExpSet
				Biobase::exprs(corrected_ExpSet) <- corrected_data
				
				# Add note to experimentData
				Biobase::notes(corrected_ExpSet) <- c(
					Biobase::notes(corrected_ExpSet),
					list(
						combat_correction = list(
							batch_factor = batch_factor,
							preserved_group = sample_group,
							par_prior = par_prior,
							correction_date = Sys.time()
						)
					)
				)
				
				# Store result
				corrected_eset(corrected_ExpSet)
				
				removeNotification("combat_progress")
				showNotification("✅ ComBat correction complete!", type = "message", duration = 5)
				
			}, error = function(e) {
				removeNotification("combat_progress")
				showNotification(paste("❌ ComBat failed:", e$message), type = "error", duration = 10)
				message("ComBat error: ", e$message)
			})
		})
		
		# Correction status
		output$correction_status <- renderUI({
			if (! is.null(corrected_eset())) {
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" Correction complete! "),
					p("Corrected ExpressionSet available for download.")
				)
			}
		})
		
		# Correction summary
		output$correction_summary <- renderPrint({
			req(corrected_eset())
			
			ExpSet_orig <- eset()
			ExpSet_corrected <- corrected_eset()
			
			cat("═══════════════════════════════════════════════\n")
			cat("COMBAT CORRECTION SUMMARY\n")
			cat("═══════════════════════════════════════════════\n\n")
			
			cat("Corrected for batch factor:", input$batch_factor, "\n")
			cat("Preserved biological group:", sample_group_column(), "\n")
			cat("Parametric prior:", input$par_prior, "\n\n")
			
			cat("Original data range:", 
					round(min(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "to",
					round(max(Biobase::exprs(ExpSet_orig), na.rm = TRUE), 2), "\n")
			
			cat("Corrected data range:", 
					round(min(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "to",
					round(max(Biobase::exprs(ExpSet_corrected), na.rm = TRUE), 2), "\n\n")
			
			cat("Dimensions:", nrow(ExpSet_corrected), "features ×", ncol(ExpSet_corrected), "samples\n")
			
			cat("\n═══════════════════════════════════════════════\n")
		})
		
		# Download corrected ExpressionSet
		output$download_corrected_eset <- downloadHandler(
			filename = function() {
				paste0("corrected_eset_", input$batch_factor, "_", Sys.Date(), ".rds")
			},
			content = function(file) {
				req(corrected_eset())
				saveRDS(corrected_eset(), file)
			}
		)
		
		# Download corrected matrix
		output$download_corrected_csv <- downloadHandler(
			filename = function() {
				paste0("corrected_data_", input$batch_factor, "_", Sys.Date(), ". csv")
			},
			content = function(file) {
				req(corrected_eset())
				corrected_data <- Biobase::exprs(corrected_eset())
				write.csv(corrected_data, file, row.names = TRUE)
			}
		)
		
		# Return corrected ExpressionSet
		return(list(
			corrected_eset = corrected_eset,
			safe_factors = safe_batch_factors
		))
	})
}