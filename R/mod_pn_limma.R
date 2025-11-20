#' PN Limma Analysis Module - UI
#'
#' @param id Namespace ID
#' @export
mod_pn_limma_ui <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			box(
				title = "Pooled Normal AAb Estimation",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				
				actionButton(ns('denoise_mod_debug'), 'Mod Debug', class = "btn-warning btn-sm"),
				
				p("Estimate the expected number of autoantibodies (AAbs) in Pooled Normal samples using limma differential expression analysis."),
				
				fluidRow(
					column(4, 
								 selectInput(
								 	ns("limma_method"),
								 	"Analysis Method",
								 	choices = c(
								 		"Standard (PN vs Clinical)" = "standard", 
								 		"With PSA Covariate" = "psa_cov"
								 	),
								 	selected = "psa_cov"
								 )
					),
					column(4, 
								 numericInput(
								 	ns("limma_fc_cutoff"),
								 	"Fold Change Cutoff",
								 	value = 1.75,
								 	min = 1.0,
								 	max = 3.0,
								 	step = 0.25
								 )
					),
					column(4,
								 br(),
								 actionButton(
								 	ns("run_limma"),
								 	"Run Limma Analysis",
								 	icon = icon("play"),
								 	class = "btn-primary btn-lg",
								 	style = "margin-top: 5px;"
								 )
					)
				),
				
				hr(),
				
				conditionalPanel(
					condition = paste0("output['", ns("limma_complete"), "']"),
					
					h4("Expected PN AAbs"),
					verbatimTextOutput(ns("limma_summary")),
					
					hr(),
					
					fluidRow(
						column(6,
									 h4("Top Differential Antigens"),
									 DT::dataTableOutput(ns("limma_top_table"))
						),
						column(6,
									 h4("Volcano Plot"),
									 plotOutput(ns("limma_volcano"), height = "400px")
						)
					),
					
					hr(),
					
					downloadButton(ns("download_limma"), "Download Full Results", class = "btn-success")
				)
			)
		)
	)
}

#' PN Limma Analysis Module - Server
#'
#' @param id Namespace ID
#' @param eset_raw Reactive returning raw ExpressionSet
#' @param eset_norm Reactive returning normalized ExpressionSet (optional)
#' @return Reactive list containing:
#'   - exp_PN_AAbs: numeric vector of expected range
#'   - PN_AAbs: character vector of specific antigens
#'   - limma_results: full limma analysis results object
#' @export
mod_pn_limma_server <- function(id, eset_raw, eset_norm = NULL) {
	moduleServer(id, function(input, output, session) {
		
		observeEvent(input$denoise_mod_debug, {
			browser()
		})
		
		# Reactive values
		rv <- reactiveValues(
			limma_results = NULL,
			exp_PN_AAbs = NULL,
			PN_AAbs = NULL
		)
		
		# Run limma analysis
		observeEvent(input$run_limma, {
			req(eset_raw()) 
			
			showNotification("Running limma analysis...", type = "message", duration = NULL, id = "limma_running")
			
			tryCatch({
				# Get data
				eset_temp <- eset_raw()
				metadata <- Biobase::pData(eset_temp)
				
				# Create 2-class grouping variable: PN vs clinical
				metadata$limma_group <- ifelse(
					metadata$Sample_Group == "Pooled Normal", 
					"PN", 
					"clinical"
				)
				metadata$limma_group <- factor(metadata$limma_group, levels = c("PN", "clinical"))
				
				# Update the ExpressionSet with new metadata
				pData(eset_temp)$limma_group <- metadata$limma_group
				
				# Determine covariates
				covariate1 <- if (input$limma_method == "psa_cov" && "PSA_score" %in% colnames(metadata)) {
					"PSA_score"
				} else {
					NULL
				}
				
				# Run limma_analysis (the new function)
				results <- limma_analysis(
					eSet = eset_temp,
					variable = "limma_group",
					feature_select = rownames(Biobase::exprs(eset_temp)),
					covariate1 = covariate1,
					covariate2 = NULL,
					user_contrast = NULL,  # Use default coefficient 2 (clinical vs PN)
					EB_trend = TRUE,
					EB_robust = TRUE,
					FC_cut = input$limma_fc_cutoff,
					p_val = 0.05
				)
				
				# Extract results
				rv$limma_results <- results
				
				# Calculate expected PN AAbs
				# Look for features with FC >= cutoff (increased in PN)
				sig_table <- results$topTable %>%
					filter(abs(logFC) >= log2(input$limma_fc_cutoff), P.Value < 0.05)
				
				sig_AAbs <- rownames(sig_table)
				n_sig <- length(sig_AAbs)
				
				# Set range (e.g., -2 to +4 around observed)
				rv$exp_PN_AAbs <- max(1, n_sig - 2):min(nrow(Biobase::exprs(eset_temp)), n_sig + 4)
				rv$PN_AAbs <- sig_AAbs
				
				removeNotification("limma_running")
				showNotification(
					HTML(paste0(
						"<strong>✅ Limma analysis complete!</strong><br>",
						"Found ", n_sig, " significant AAbs<br>",
						"Expected range: ", min(rv$exp_PN_AAbs), "-", max(rv$exp_PN_AAbs)
					)),
					type = "message",
					duration = 10
				)
				
			}, error = function(e) {
				removeNotification("limma_running")
				showNotification(
					paste("❌ Limma analysis failed:", e$message),
					type = "error",
					duration = 10
				)
				print(e)  # Debug
			})
		})
		
		# Outputs
		output$limma_complete <- reactive({
			!is.null(rv$limma_results)
		})
		outputOptions(output, "limma_complete", suspendWhenHidden = FALSE)
		
		output$limma_summary <- renderPrint({
			req(rv$limma_results)
			
			cat("Expected PN AAb Count: ", paste(rv$exp_PN_AAbs, collapse = ", "), "\n")
			cat("Number of Specific AAbs: ", length(rv$PN_AAbs), "\n\n")
			cat("Specific PN AAbs:\n")
			cat(paste(rv$PN_AAbs, collapse = ", "), "\n")
		})
		
		output$limma_top_table <- DT::renderDataTable({
			req(rv$limma_results)
			
			# Get significant features
			sig_table <- rv$limma_results$topTable %>%
				filter(abs(logFC) >= log2(input$limma_fc_cutoff), P.Value < 0.05) %>%
				arrange(P.Value)
			
			# Add FC column if not present
			if(!"FC" %in% colnames(sig_table)) {
				sig_table$FC <- logratio2foldchange(sig_table$logFC)
			}
			
			sig_table %>%
				select(logFC, FC, P.Value, adj.P.Val) %>%
				DT::datatable(
					options = list(pageLength = 10, scrollX = TRUE),
					rownames = TRUE
				) %>%
				DT::formatRound(columns = c("logFC", "FC"), digits = 2) %>%
				DT::formatSignif(columns = c("P.Value", "adj.P.Val"), digits = 3)
		})
		
		output$limma_volcano <- renderPlot({
			req(rv$limma_results)
			
			# Prepare volcano plot data
			plot_data <- rv$limma_results$topTable %>%
				mutate(
					significant = abs(logFC) >= log2(input$limma_fc_cutoff) & P.Value < 0.05,
					protein = rownames(rv$limma_results$topTable)
				)
			
			# Label only significant features
			plot_data$label <- ifelse(plot_data$significant, plot_data$protein, "")
			
			ggplot(plot_data, aes(x = logFC, y = -log10(P.Value))) +
				geom_point(aes(color = significant), alpha = 0.6, size = 2) +
				geom_text_repel(aes(label = label), size = 3, max.overlaps = 20) +
				scale_color_manual(values = c("grey60", "red")) +
				geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue") +
				geom_vline(xintercept = log2(input$limma_fc_cutoff), linetype = "dashed", color = "blue") +
				geom_vline(xintercept = -log2(input$limma_fc_cutoff), linetype = "dashed", color = "blue") +
				labs(
					title = "PN vs Clinical Differential Expression",
					x = "log2 Fold Change",
					y = "-log10(P-value)"
				) +
				theme_minimal() +
				theme(legend.position = "none")
		})
		
		output$download_limma <- downloadHandler(
			filename = function() {
				paste0("PN_limma_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
			},
			content = function(file) {
				req(rv$limma_results)
				write.csv(rv$limma_results$topTable, file, row.names = TRUE)
			}
		)
		
		# Return reactive values for use in other modules
		return(reactive({
			list(
				exp_PN_AAbs = rv$exp_PN_AAbs,
				PN_AAbs = rv$PN_AAbs,
				limma_results = rv$limma_results
			)
		}))
	})
}