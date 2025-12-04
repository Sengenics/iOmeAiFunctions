#' Batch Effect Visualization Module - UI
#'
#' Compare batch effects before and after correction using multiple visualization methods
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_batch_visualization_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Batch Effect Visualization Settings",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				uiOutput(ns("debug_ui")),
				
				fluidRow(
					column(
						width = 3,
						selectInput(
							ns("color_by"),
							"Color by:",
							choices = NULL
						)
					),
					column(
						width = 3,
						selectInput(
							ns("shape_by"),
							"Shape by (t-SNE only):",
							choices = NULL
						)
					),
					column(
						width = 3,
						selectInput(
							ns("replicate_column"),
							"Technical Replicate ID:",
							choices = c("None" = "", "Select column...")
						),
						helpText("Replicates will be highlighted on dendrogram")
					),
					column(
						width = 3,
						checkboxInput(
							ns("show_corrected"),
							"Show Corrected Data",
							value = FALSE
						),
						helpText("Compare before/after ComBat")
					)
				),
				
				hr(),
				
				fluidRow(
					column(
						width = 4,
						numericInput(
							ns("tsne_perplexity"),
							"t-SNE Perplexity:",
							value = 30,
							min = 5,
							max = 50
						)
					),
					column(
						width = 4,
						numericInput(
							ns("tsne_iterations"),
							"t-SNE Iterations:",
							value = 1000,
							min = 250,
							max = 5000,
							step = 250
						)
					),
					column(
						width = 4,
						actionButton(
							ns("run_analysis"),
							"Generate Visualizations",
							icon = icon("chart-line"),
							class = "btn-primary btn-lg",
							style = "margin-top: 25px;"
						)
					)
				)
			)
		),
		
		# Dendrogram
		fluidRow(
			box(
				title = "Hierarchical Clustering Dendrogram",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Technical replicates should cluster together if batch effects are minimal."),
				
				tabsetPanel(
					id = ns("dendro_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("dendrogram_original"), height = "600px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("dendrogram_corrected"), height = "600px")
					)
				)
			)
		),
		
		# t-SNE
		fluidRow(
			box(
				title = "t-SNE Dimensionality Reduction",
				width = 12,
				status = "warning",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				p("Samples should separate by biological groups, not batch factors."),
				
				tabsetPanel(
					id = ns("tsne_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("tsne_original"), height = "600px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("tsne_corrected"), height = "600px")
					),
					tabPanel(
						"Side-by-Side",
						plotOutput(ns("tsne_comparison"), height = "600px")
					)
				)
			)
		),
		
		# PCA
		fluidRow(
			box(
				title = "Principal Component Analysis (PCA)",
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				tabsetPanel(
					id = ns("pca_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("pca_original"), height = "600px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("pca_corrected"), height = "600px")
					),
					tabPanel(
						"Variance Explained",
						plotOutput(ns("pca_variance"), height = "400px")
					)
				)
			)
		),
		
		# Heatmap
		fluidRow(
			box(
				title = "Sample Distance Heatmap",
				width = 12,
				status = "danger",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				tabsetPanel(
					id = ns("heatmap_tabs"),
					tabPanel(
						"Original Data",
						plotOutput(ns("heatmap_original"), height = "700px")
					),
					tabPanel(
						"Corrected Data",
						plotOutput(ns("heatmap_corrected"), height = "700px")
					)
				)
			)
		)
	)
}

#' Batch Effect Visualization Module - Server
#'
#' @param id Module namespace ID
#' @param eset_original Reactive ExpressionSet (original data)
#' @param eset_corrected Reactive ExpressionSet (ComBat corrected data)
#' @param debug Enable debug mode
#' @export
mod_batch_visualization_server <- function(id,
																					 eset_original,
																					 eset_corrected = reactive(NULL),
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
				message("🔍 DEBUG MODE - Batch Visualization Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset_original() - Original ExpressionSet")
				message("  • eset_corrected() - Corrected ExpressionSet")
				message("  • input$color_by - Coloring factor")
				message("  • input$shape_by - Shape factor")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Update column choices
		observe({
			req(eset_original())
			
			cols <- colnames(Biobase::pData(eset_original()))
			
			updateSelectInput(session, "color_by", choices = cols, selected = cols[1])
			updateSelectInput(session, "shape_by", choices = c("None" = "", cols), selected = "")
			updateSelectInput(session, "replicate_column", choices = c("None" = "", cols), selected = "")
		})
		
		# Storage for analysis results
		analysis_results <- reactiveVal(NULL)
		
		# Run analysis
		observeEvent(input$run_analysis, {
			req(eset_original())
			req(input$color_by)
			
			showNotification("Running visualization analysis...", id = "viz_progress", duration = NULL, type = "message")
			
			tryCatch({
				results <- list()
				
				# Original data analysis
				results$original <- perform_analysis(
					eset_original(),
					perplexity = input$tsne_perplexity,
					iterations = input$tsne_iterations
				)
				
				# Corrected data analysis (if available)
				if (input$show_corrected && !is.null(eset_corrected())) {
					results$corrected <- perform_analysis(
						eset_corrected(),
						perplexity = input$tsne_perplexity,
						iterations = input$tsne_iterations
					)
				}
				
				analysis_results(results)
				
				removeNotification("viz_progress")
				showNotification("✅ Visualization complete!", type = "message", duration = 3)
				
			}, error = function(e) {
				removeNotification("viz_progress")
				showNotification(paste("❌ Analysis failed:", e$message), type = "error", duration = 10)
			})
		})
		
		# Helper function to perform all analyses
		perform_analysis <- function(eset, perplexity, iterations) {
			expr_data <- Biobase::exprs(eset)
			meta <- Biobase::pData(eset)
			
			# Row scale for distance calculations
			expr_scaled <- row_scale_function(expr_data)
			
			# Hierarchical clustering
			dist_mat <- dist(t(expr_scaled))
			hclust_res <- hclust(dist_mat, method = "ward.D2")
			
			# t-SNE
			set.seed(42)
			tsne_res <- Rtsne::Rtsne(
				t(expr_scaled),
				dims = 2,
				perplexity = perplexity,
				max_iter = iterations,
				check_duplicates = FALSE
			)
			
			# PCA
			pca_res <- prcomp(t(expr_scaled), scale.  = FALSE)
			
			list(
				hclust = hclust_res,
				dist = dist_mat,
				tsne = tsne_res,
				pca = pca_res,
				meta = meta,
				expr = expr_data,
				expr_scaled = expr_scaled
			)
		}
		
		# Dendrogram - Original
		output$dendrogram_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_dendrogram(
				results$hclust,
				results$meta,
				color_by = input$color_by,
				replicate_col = input$replicate_column,
				title = "Original Data"
			)
		})
		
		# Dendrogram - Corrected
		output$dendrogram_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_dendrogram(
				results$hclust,
				results$meta,
				color_by = input$color_by,
				replicate_col = input$replicate_column,
				title = "ComBat Corrected Data"
			)
		})
		
		# t-SNE - Original
		output$tsne_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_tsne(
				results$tsne,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "Original Data"
			)
		})
		
		# t-SNE - Corrected
		output$tsne_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_tsne(
				results$tsne,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "ComBat Corrected Data"
			)
		})
		
		# t-SNE - Comparison
		output$tsne_comparison <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			orig <- analysis_results()$original
			corr <- analysis_results()$corrected
			
			plot_tsne_comparison(
				orig$tsne, corr$tsne,
				orig$meta,
				color_by = input$color_by,
				shape_by = input$shape_by
			)
		})
		
		# PCA - Original
		output$pca_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_pca(
				results$pca,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "Original Data"
			)
		})
		
		# PCA - Corrected
		output$pca_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_pca(
				results$pca,
				results$meta,
				color_by = input$color_by,
				shape_by = input$shape_by,
				title = "ComBat Corrected Data"
			)
		})
		
		# PCA Variance
		output$pca_variance <- renderPlot({
			req(analysis_results())
			
			orig_var <- analysis_results()$original$pca$sdev^2 / sum(analysis_results()$original$pca$sdev^2)
			
			if (! is.null(analysis_results()$corrected)) {
				corr_var <- analysis_results()$corrected$pca$sdev^2 / sum(analysis_results()$corrected$pca$sdev^2)
				plot_pca_variance_comparison(orig_var, corr_var)
			} else {
				plot_pca_variance_single(orig_var)
			}
		})
		
		# Heatmap - Original
		output$heatmap_original <- renderPlot({
			req(analysis_results())
			req(input$color_by)
			
			results <- analysis_results()$original
			plot_distance_heatmap(
				results$dist,
				results$meta,
				color_by = input$color_by,
				title = "Original Data"
			)
		})
		
		# Heatmap - Corrected
		output$heatmap_corrected <- renderPlot({
			req(analysis_results())
			req(analysis_results()$corrected)
			req(input$color_by)
			
			results <- analysis_results()$corrected
			plot_distance_heatmap(
				results$dist,
				results$meta,
				color_by = input$color_by,
				title = "ComBat Corrected Data"
			)
		})
	})
}