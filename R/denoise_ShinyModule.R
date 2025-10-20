#' Denoiser Module UI
#'
#' @param id Character; module namespace ID
#'
#' @export
mod_denoiser_ui <- function(id) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			# Left panel - Controls ####
			
			column(
				width = 3,
				shinydashboard::box(
					title = "Denoiser Controls",
					width = NULL,
					status = "primary",
					solidHeader = TRUE,
					actionButton(ns('denoise_mod_debug'),'Mod Debug'),
					actionButton(
						ns("run_denoise"),
						"Run Denoising",
						icon = icon("play"),
						class = "btn-success btn-lg btn-block"
					),
					actionButton(
						ns("denoise_update_ExpSet"),
						"Update ExpSet list",
						icon = icon("plus-circle"),
						class = "btn-success btn-lg btn-block"
					),
					downloadButton(
						ns("download_expset"),
						"Download ExpressionSet",
						class = "btn-primary btn-block"
					),
					
					br(),
					
					# Data selection
					h4("Data Selection"),
					selectInput(
						ns("assay_name"),
						"Expression Data Assay:",
						choices = c("NetI" = "NetI", "exprs" = "exprs"),
						selected = "exprs"
					),
					
					selectInput(
						ns("norm_assay_name"),
						"Normalized Data Assay (for borders):",
						choices = c("loess_combat" = "loess_combat", 
												"median_combat" = "median_combat"),
						selected = "loess_combat"
					),
					
					hr(),
					
					# PC removal
					h4("PC Removal"),
					sliderInput(
						ns("n_PCs"),
						"Number of PCs to Remove:",
						min = 1,
						max = 10,
						value = 3,
						step = 1
					),
					
					checkboxInput(
						ns("scale_pca"),
						"Scale data for PCA",
						value = TRUE
					),
					checkboxInput(
						ns("center_pca"),
						"Center data for PCA",
						value = TRUE
					),
					
					hr(),
					
					# Cutpoint settings
					h4("Cutpoint Settings"),
					selectInput(
						ns("method"),
						"Cutting Method:",
						choices = c(
							"Singular (global threshold)" = "singular",
							"MAD (per-antigen)" = "MAD",
							"AAD (per-antigen)" = "AAD"
						),
						selected = "singular"
					),
					
					sliderInput(
						ns("cut_min"),
						"Cutpoint Range - Min:",
						min = 0,
						max = 5,
						value = 0.4,
						step = 0.1
					),
					
					sliderInput(
						ns("cut_max"),
						"Cutpoint Range - Max:",
						min = 0,
						max = 5,
						value = 3,
						step = 0.1
					),
					
					sliderInput(
						ns("cut_step"),
						"Cutpoint Step:",
						min = 0.05,
						max = 0.5,
						value = 0.1,
						step = 0.05
					),
					
					hr(),
					
					# PN identification
					h4("Pooled Normal Samples"),
					selectInput(
						ns("PN_column"),
						"PN Identifier Column:",
						choices = NULL  # Will be populated from eset
					),
					
					textInput(
						ns("PN_value"),
						"PN Identifier Value:",
						value = "Pooled Normal"
					),
					
					hr(),
					
					# Expected PN AAbs
					h4("Expected PN AAbs"),
					textAreaInput(
						ns("PN_AAbs"),
						"Expected PN AAbs (comma-separated):",
						value = "PSIP1, MAPK9, MX1, UBE2I, PTPN11, MLH1",
						rows = 3
					),
					
					numericInput(
						ns("exp_PN_min"),
						"Expected PN AAb Count - Min:",
						value = 6,
						min = 0,
						step = 1
					),
					
					numericInput(
						ns("exp_PN_max"),
						"Expected PN AAb Count - Max:",
						value = 12,
						min = 0,
						step = 1
					),
					
					hr(),
					
					# Annotation columns
					h4("Plot Annotations"),
					selectizeInput(
						ns("annotation_cols"),
						"Annotation Columns:",
						choices = NULL,  # Will be populated from eset
						multiple = TRUE,
						options = list(
							placeholder = 'Select annotation columns...',
							maxItems = 5
						)
					),
					
					hr(),
					
					# Action buttons
		
					
					downloadButton(
						ns("download_results"),
						"Download Results",
						class = "btn-primary btn-block"
					)
				)
			),
			
			# Right panel - Tabs #####
			column(
				width = 9,
				tabsetPanel(
					id = ns("main_tabs"),
					type = "tabs",
					
					## Components #####
					tabPanel('Components',
									 mod_pc_visualizer_ui("pc_viz")),
						
					## Tab 1: PCA & Denoising ####
					
					
					tabPanel(
						"PCA & Denoising",
						br(),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "PCA Variance Explained",
									width = NULL,
									plotOutput(ns("pca_variance_plot"), height = "300px")
								)
							)
						),
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "PC Selection",
									width = NULL,
									sliderInput(
										ns("pc_view_slider"),
										"PCs Removed:",
										min = 0,
										max = 3,
										value = 1,
										step = 1,
										animate = TRUE
									)
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Denoised Data Info",
									width = NULL,
									tableOutput(ns("denoised_info_table"))
								)
							)
						),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Denoised Data Heatmap",
									width = NULL,
									plotOutput(ns("denoised_heatmap"), height = "600px")
								)
							),
							column(
								width = 12,
								shinydashboard::box(
									title = "Denoised Data Density",
									width = NULL,
									plotOutput(ns("denoised_density"), height = "600px")
								)
							)
						)
					),
					
					## Tab 2: Cutpoint Analysis ####
					tabPanel(
						"Cutpoint Analysis",
						br(),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Cutpoint Metrics",
									width = NULL,
									plotOutput(ns("cutpoint_summary_plot"), height = "600px")
								)
							)
						),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Cutpoint Results Table",
									width = NULL,
									DTOutput(ns("cutpoint_table"))
								)
							)
						)
					),
					## Pooled Normal Analysis ####
					tabPanel(
						"Pooled Normal Analysis",
						br(),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Pooled Normal Sample Selection",
									width = NULL,
									status = "info",
									collapsible = TRUE,
									collapsed = FALSE,
									
									fluidRow(
										column(
											width = 4,
											numericInput(
												ns("pn_pc_level"),
												"PCs Removed:",
												value = 3,
												min = 0,
												max = 10,
												step = 1
											)
										),
										column(
											width = 4,
											checkboxInput(
												ns("pn_show_expected_aabs"),
												"Highlight Expected PN AAbs",
												value = TRUE
											)
										),
										column(
											width = 4,
											checkboxInput(
												ns("pn_cluster_samples"),
												"Cluster Samples",
												value = FALSE
											)
										)
									)
								)
							)
						),
						
						# Statistics row
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "PN Sample Statistics",
									width = NULL,
									tableOutput(ns("pn_stats_table"))
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Expected PN AAb Detection",
									width = NULL,
									status = "warning",
									tableOutput(ns("pn_expected_aabs_table"))
								)
							)
						),
						
						# Three visualization modules side by side (plots stacked vertically within each)
						fluidRow(
							column(
								width = 4,
								mod_pn_viz_ui(ns("pn_neti"), title = "NetI Data", status = "primary", show_density = TRUE)
							),
							column(
								width = 4,
								mod_pn_viz_ui(ns("pn_norm"), title = "Normalized Data", status = "info", show_density = TRUE)
							),
							column(
								width = 4,
								mod_pn_viz_ui(ns("pn_denoised"), title = "Denoised Data", status = "success", show_density = TRUE)
							)
						),
						
						# Comparison plot
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Comparison: PN vs Non-PN Samples",
									width = NULL,
									plotOutput(ns("pn_comparison_plot"), height = "500px")
								)
							)
						)
					),
					
					## Tab 3: AAb-Called Data ####
					tabPanel(
						"AAb-Called Data",
						br(),
						
						# Cutpoint selection row
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "Cutpoint Selection",
									width = NULL,
									sliderInput(
										ns("cutpoint_slider"),
										"Select Cutpoint:",
										min = 0.4,
										max = 3,
										value = 1.0,
										step = 0.1
									),
									tableOutput(ns("selected_cutpoint_info"))
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Optimal Cutpoint",
									width = NULL,
									status = "success",
									tableOutput(ns("optimal_cutpoint_table"))
								)
							)
						),
						
						# Full AAb-Called Data Heatmap (all samples)
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb-Called Data Heatmap (All Samples)",
									width = NULL,
									status = "info",
									plotOutput(ns("aab_called_heatmap"), height = "600px")
								)
							)
						),
						
						# AAb Call Rate Distribution
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb Call Rate Distribution",
									width = NULL,
									plotOutput(ns("aab_call_rate_hist"), height = "400px")
								)
							)
						),
						
						# Pooled Normal Sample Options
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Pooled Normal Sample Analysis Options",
									width = NULL,
									status = "warning",
									collapsible = TRUE,
									collapsed = FALSE,
									
									p(icon("info-circle"), 
										strong("Note:"), 
										"The visualizations below show only Pooled Normal samples from the AAb-called data."),
									
									fluidRow(
										column(
											width = 6,
											checkboxInput(
												ns("aab_show_expected_aabs"),
												"Highlight Expected PN AAbs",
												value = TRUE
											)
										),
										column(
											width = 6,
											checkboxInput(
												ns("aab_cluster_samples"),
												"Cluster Samples",
												value = FALSE
											)
										)
									)
								)
							)
						),
						
						# PN Sample Statistics for AAb-called data
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "PN AAb Call Statistics",
									width = NULL,
									tableOutput(ns("aab_pn_stats_table"))
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "PN Expected AAb Detection (AAb-Called)",
									width = NULL,
									status = "warning",
									tableOutput(ns("aab_pn_expected_aabs_table"))
								)
							)
						),
						
						# Three visualization modules for PN samples: NetI, Norm, AAb-Called
						# No density plots for binary AAb-called data
						fluidRow(
							column(
								width = 4,
								mod_pn_viz_ui(ns("aab_pn_neti"), title = "PN - NetI Data", status = "primary", show_density = FALSE)
							),
							column(
								width = 4,
								mod_pn_viz_ui(ns("aab_pn_norm"), title = "PN - Normalized Data", status = "info", show_density = FALSE)
							),
							column(
								width = 4,
								mod_pn_viz_ui(ns("aab_pn_called"), title = "PN - AAb-Called Data", status = "success", show_density = FALSE)
							)
						),
						
						# AAb Calls per PN Sample
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb Calls per Pooled Normal Sample",
									width = NULL,
									plotOutput(ns("aab_calls_per_pn_sample"), height = "400px")
								)
							)
						)
					),

					## Tab 4: Visualization ####
					tabPanel(
						"Visualization",
						br(),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb Borders (on Normalized Data)",
									width = NULL,
									plotOutput(ns("aab_borders_plot"), height = "800px")
								)
							)
						),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "t-SNE Plot",
									width = NULL,
									fluidRow(
										column(
											width = 4,
											selectInput(
												ns("tsne_color"),
												"Color by:",
												choices = NULL
											)
										),
										column(
											width = 4,
											selectInput(
												ns("tsne_shape"),
												"Shape by:",
												choices = c("None" = "none")
											)
										),
										column(
											width = 4,
											numericInput(
												ns("tsne_perplexity"),
												"Perplexity:",
												value = 30,
												min = 5,
												max = 50
											)
										)
									),
									plotOutput(ns("tsne_plot"), height = "600px")
								)
							)
						)
					),
					
					# Tab 5: Summary & Export
					tabPanel(
						"Summary",
						br(),
						fluidRow(
							column(
								width = 6,
								shinydashboard::box(
									title = "AAb Summary Statistics",
									width = NULL,
									status = "info",
									tableOutput(ns("summary_stats_table"))
								)
							),
							column(
								width = 6,
								shinydashboard::box(
									title = "Parameter Comparison",
									width = NULL,
									status = "warning",
									DTOutput(ns("parameter_comparison_table"))
								)
							)
						),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Export Template",
									width = NULL,
									textInput(
										ns("template_name"),
										"Template Name:",
										value = "AAb_caller_template"
									),
									textInput(
										ns("project_name"),
										"Project Name:",
										value = "MyProject"
									),
									downloadButton(
										ns("download_template"),
										"Download AAb_caller_template.R",
										class = "btn-primary"
									)
								)
							)
						)
					)
				)
			)
		)
	)
}


#' Denoiser Module Server
#'
#' @param id Character; module namespace ID
#' @param eset_raw Reactive; ExpressionSet with raw/NetI data
#' @param eset_norm Reactive; ExpressionSet with normalized data (optional)
#'
#' @export
mod_denoiser_server <- function(id, ExpSet_list, eset_raw, eset_norm = NULL) {
	moduleServer(id, function(input, output, session) {
		
		ns <- session$ns
		
		observeEvent(input$denoise_mod_debug, {
			browser()
		})
		
		# Reactive values ####
		rv <- reactiveValues(
			denoise_results = NULL,
			cutpoint_results = NULL,
			optimal_cutpoint = NULL,
			aab_called_data = NULL,
			all_results = NULL,
			ExpSet_list = NULL
		)
		
		# Update ExpressionSet with denoised and AAb data ####
		observeEvent(input$denoise_update_ExpSet, { 
			req(rv$denoise_results, rv$aab_called_data, ExpSet_list())
			
			denoise_matrix <- rv$denoise_results$denoised_data[[length(rv$denoise_results$denoised_data)]]
			aab_called_data <- rv$aab_called_data
			ExpSet_list_copy <- ExpSet_list()
			sample_ExpSet <- ExpSet_list_copy$sample_ExpSet
			
			# Shift denoised data to make all values positive
			denoise_matrix_median <- denoise_matrix + abs(min(denoise_matrix))
			
			# Add matrices to ExpressionSet
			sample_ExpSet <- ExpSet_add_matrix_function(sample_ExpSet, denoise_matrix, 'denoised')
			sample_ExpSet <- ExpSet_add_matrix_function(sample_ExpSet, denoise_matrix_median, 'denoised_median')
			sample_ExpSet <- ExpSet_add_matrix_function(sample_ExpSet, aab_called_data, 'aab_called')
			
			ExpSet_list_copy$sample_ExpSet <- sample_ExpSet
			rv$ExpSet_list <- ExpSet_list_copy
			
			showNotification("✓ ExpressionSet updated with denoised and AAb-called data", type = "message", duration = 3)
		})
		
		# Download handler for ExpressionSet ####
		output$download_expset <- downloadHandler(
			filename = function() {
				paste0("ExpressionSet_with_reconstruction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
			},
			content = function(file) {
				req(rv$ExpSet_list)
				withProgress(message = "Preparing download...", value = 0.5, {
					tryCatch({
						saveRDS(rv$ExpSet_list, file)
						showNotification("✓ ExpressionSet downloaded successfully", type = "message", duration = 3)
					}, error = function(e) {
						showNotification(paste("Error saving ExpressionSet:", e$message), type = "error", duration = 10)
					})
				})
			}
		)
		
		# Update UI choices based on eset ####
		observe({
			req(eset_raw()) 
			metadata <- Biobase::pData(eset_raw())
			
			updateSelectInput(session, "PN_column", choices = colnames(metadata),
												selected = if("Sample_Group" %in% colnames(metadata)) "Sample_Group" else colnames(metadata)[1])
			
			updateSelectizeInput(session, "annotation_cols", choices = colnames(metadata),
													 selected = if("Sample_Group" %in% colnames(metadata)) "Sample_Group" else NULL)
			
			updateSelectInput(session, "tsne_color", choices = colnames(metadata))
			updateSelectInput(session, "tsne_shape", choices = c("None" = "none", colnames(metadata)))
		})
		
		# Update PC slider max based on n_PCs input ####
		observe({
			updateSliderInput(session, "pc_view_slider", max = input$n_PCs, value = min(input$n_PCs))
		})
		
		# Parse PN AAbs input ####
		PN_AAbs_parsed <- reactive({
			req(input$PN_AAbs)
			aabs <- unlist(strsplit(input$PN_AAbs, ","))
			aabs <- trimws(aabs)
			aabs[aabs != ""]
		})
		
		# Run denoising pipeline ####
		observeEvent(input$run_denoise, {
			req(eset_raw())
			
			withProgress(message = 'Running denoising pipeline...', value = 0, {
				
				# Step 1: Validate inputs
				incProgress(0.1, detail = "Validating inputs...")
				tryCatch({
					validate_denoise_inputs(eset = eset_raw(), assay_name = input$assay_name,
																	PN_column = input$PN_column, PN_value = input$PN_value)
				}, error = function(e) {
					showNotification(paste("Validation error:", e$message), type = "error", duration = 10)
					return(NULL)
				})
				
				# Step 2: Remove PCs
				incProgress(0.2, detail = "Removing principal components...")
				rv$denoise_results <- denoise_remove_PCs(eset = eset_raw(), assay_name = input$assay_name,
																								 n_PCs = input$n_PCs, scale = input$scale_pca, center = input$center_pca)
				
				# Step 3: Find cutpoints for each PC level
				incProgress(0.3, detail = "Testing cutpoints...")
				cut_seq <- seq(from = input$cut_min, to = input$cut_max, by = input$cut_step)
				all_cutpoint_results <- list()
				
				for (i in seq_along(rv$denoise_results$denoised_data)) {
					incProgress(0.3 / length(rv$denoise_results$denoised_data), detail = paste("Testing PC level", i, "..."))
					
					cutpoint_results <- denoise_find_cutpoints(
						denoised_data = rv$denoise_results$denoised_data[[i]], eset = eset_raw(),
						PN_column = input$PN_column, PN_value = input$PN_value,
						PN_AAbs = PN_AAbs_parsed(), cut_seq = cut_seq, method = input$method
					)
					cutpoint_results$PCs_removed <- i
					all_cutpoint_results[[i]] <- cutpoint_results
				}
				rv$cutpoint_results <- do.call(rbind, all_cutpoint_results)
				
				# Step 4: Select optimal cutpoint
				incProgress(0.2, detail = "Selecting optimal cutpoint...")
				rv$optimal_cutpoint <- denoise_select_optimal_cutpoint(
					cutpoint_results = rv$cutpoint_results,
					exp_PN_AAbs = seq(input$exp_PN_min, input$exp_PN_max)
				)
				
				# Step 5: Generate final AAb-called data
				incProgress(0.2, detail = "Generating AAb-called data...")
				rv$aab_called_data <- denoise_apply_cutpoint(
					denoised_data = rv$denoise_results$denoised_data[[rv$optimal_cutpoint$PCs_removed]],
					cutpoint = rv$optimal_cutpoint$cutpoint, method = input$method
				)
				
				# Update cutpoint slider
				updateSliderInput(session, "cutpoint_slider", min = input$cut_min, max = input$cut_max,
													value = rv$optimal_cutpoint$cutpoint, step = input$cut_step)
				
				incProgress(0.1, detail = "Complete!")
				showNotification("Denoising complete!", type = "message", duration = 5)
			})
		})
		
		# PCA & Denoising Tab Outputs ####
		output$pca_variance_plot <- renderPlot({
			req(rv$denoise_results)
			variance_df <- data.frame(
				PC = paste0("PC", seq_along(rv$denoise_results$variance_explained)),
				Variance = rv$denoise_results$variance_explained
			)
			ggplot2::ggplot(variance_df[1:min(20, nrow(variance_df)), ], ggplot2::aes(x = PC, y = Variance)) +
				ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
				ggplot2::geom_vline(xintercept = input$n_PCs + 0.5, linetype = "dashed", color = "red", size = 1) +
				ggplot2::labs(title = "Variance Explained by Principal Components",
											subtitle = paste("Red line indicates", input$n_PCs, "PC(s) to be removed"),
											x = "Principal Component", y = "Variance Explained (%)") +
				ggplot2::theme_minimal() +
				ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
		})
		
		output$denoised_info_table <- renderTable({
			req(rv$denoise_results)
			pc_level <- input$pc_view_slider
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			data.frame(
				Metric = c("PCs Removed", "Features", "Samples", "Max Value", "Min Value"),
				Value = c(pc_level, nrow(denoised_data), ncol(denoised_data),
									round(max(denoised_data), 2), round(min(denoised_data), 2))
			)
		})
		
		output$denoised_heatmap <- renderPlot({
			req(rv$denoise_results, eset_raw())
			plot_denoise_heatmap(
				denoised_data = rv$denoise_results$denoised_data[[input$pc_view_slider]],
				eset = eset_raw(), annotation_cols = input$annotation_cols,
				title = paste("Denoised Data -", input$pc_view_slider, "PC(s) Removed"),
				show_rownames = FALSE, show_colnames = FALSE
			)
		})
		
		output$denoised_density <- renderPlot({
			req(rv$denoise_results, eset_raw())
			plot_density_data(rv$denoise_results$denoised_data[[input$pc_view_slider]], eset_raw())
		})
		
		# Cutpoint Analysis Tab Outputs ####
		output$cutpoint_summary_plot <- renderPlot({
			req(rv$cutpoint_results, rv$optimal_cutpoint)
			cutpoint_subset <- rv$cutpoint_results[rv$cutpoint_results$PCs_removed == input$pc_view_slider, ]
			plot_cutpoint_summary(cutpoint_results = cutpoint_subset, optimal_cutpoint = rv$optimal_cutpoint$cutpoint)
		})
		
		output$cutpoint_table <- renderDT({
			req(rv$cutpoint_results)
			cutpoint_subset <- rv$cutpoint_results[rv$cutpoint_results$PCs_removed == input$pc_view_slider, ]
			DT::datatable(cutpoint_subset, options = list(pageLength = 10, scrollX = TRUE, order = list(list(3, 'desc'))), rownames = FALSE) %>%
				DT::formatRound(columns = c('cutpoint', 'TP_FP_ratio', 'zz_2_frac', 'zz_4_frac'), digits = 2) %>%
				DT::formatStyle('TP_FP_ratio', background = DT::styleColorBar(cutpoint_subset$TP_FP_ratio, 'lightblue'),
												backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
		})
		
		# AAb-Called Data Tab Outputs ####
		current_aab_data <- reactive({
			req(rv$denoise_results, input$cutpoint_slider)
			denoise_apply_cutpoint(
				denoised_data = rv$denoise_results$denoised_data[[input$pc_view_slider]],
				cutpoint = input$cutpoint_slider, method = input$method
			)
		})
		
		output$selected_cutpoint_info <- renderTable({
			req(rv$cutpoint_results)
			cutpoint_subset <- rv$cutpoint_results[
				rv$cutpoint_results$PCs_removed == input$pc_view_slider &
					abs(rv$cutpoint_results$cutpoint - input$cutpoint_slider) < 0.01,
			]
			if (nrow(cutpoint_subset) > 0) {
				data.frame(
					Metric = c("Cutpoint", "PN AAb Count", "TP:FP Ratio", "ZZ_con2 Rate"),
					Value = c(cutpoint_subset$cutpoint[1], cutpoint_subset$PN_Aab_count_67_perc[1],
										round(cutpoint_subset$TP_FP_ratio[1], 2), round(cutpoint_subset$zz_2_frac[1], 3))
				)
			} else {
				data.frame(Metric = "No data", Value = "-")
			}
		})
		
		output$optimal_cutpoint_table <- renderTable({
			req(rv$optimal_cutpoint)
			data.frame(
				Parameter = c("PCs Removed", "Cutpoint", "PN AAb Count", "PN Hit Rate (%)", "TP:FP Ratio", "Unique AAbs", "Median AAbs/Sample"),
				Value = c(rv$optimal_cutpoint$PCs_removed, rv$optimal_cutpoint$cutpoint, rv$optimal_cutpoint$PN_Aab_count_67_perc,
									round(rv$optimal_cutpoint$PN_AAb_hit_rate, 1), round(rv$optimal_cutpoint$TP_FP_ratio, 2),
									rv$optimal_cutpoint$N_unique_AAbs, rv$optimal_cutpoint$sample_AAb_median)
			)
		})
		
		output$aab_called_heatmap <- renderPlot({
			req(current_aab_data(), eset_raw())
			plot_denoise_heatmap(
				denoised_data = current_aab_data(), eset = eset_raw(), annotation_cols = input$annotation_cols,
				title = paste("AAb-Called Data - Cutpoint:", input$cutpoint_slider),
				show_rownames = TRUE, show_colnames = FALSE
			)
		})
		
		output$aab_call_rate_hist <- renderPlot({
			req(current_aab_data())
			call_rates <- apply(current_aab_data(), 1, function(x) length(which(x > 0)) / length(x) * 100)
			graphics::hist(call_rates, breaks = 30, main = "Distribution of AAb Call Rates",
										 xlab = "Call Rate (%)", ylab = "Number of Antigens", col = "skyblue", border = "white")
			graphics::abline(v = median(call_rates), col = "red", lwd = 2, lty = 2)
			graphics::legend("topright", legend = paste("Median:", round(median(call_rates), 1), "%"),
											 col = "red", lwd = 2, lty = 2, bty = "n")
		})
		
		# Visualization Tab Outputs ####
		output$aab_borders_plot <- renderPlot({
			req(current_aab_data(), eset_raw(), eset_norm())
			background_data <- Biobase::exprs(eset_norm())
			if (!is.matrix(background_data)) background_data <- as.matrix(background_data)
			background_RC <- t(scale(t(background_data), scale = FALSE, center = TRUE))
			plot_denoise_borders(
				background_data = background_RC, aab_called_data = current_aab_data(),
				eset = eset_raw(), annotation_cols = input$annotation_cols,
				variable = if(length(input$annotation_cols) > 0) input$annotation_cols[1] else NULL,
				title = "AAb Borders on Row-Centered Data"
			)
		})
		
		output$tsne_plot <- renderPlot({
			req(current_aab_data(), eset_raw())
			shape_by <- if(input$tsne_shape == "none") NULL else input$tsne_shape
			plot_denoise_tsne(
				aab_called_data = current_aab_data(), eset = eset_raw(),
				color_by = input$tsne_color, shape_by = shape_by,
				perplexity = input$tsne_perplexity, seed = 42
			)
		})
		
		# Summary Tab Outputs ####
		output$summary_stats_table <- renderTable({
			req(rv$aab_called_data, eset_raw())
			calculate_aab_summary(
				aab_called_data = rv$aab_called_data, eset = eset_raw(),
				group_by = if(length(input$annotation_cols) > 0) input$annotation_cols[1] else NULL
			)
		})
		
		output$parameter_comparison_table <- renderDT({
			req(rv$cutpoint_results)
			comparison <- compare_denoise_parameters(
				cutpoint_results = rv$cutpoint_results, top_n = 20, sort_by = "TP_FP_ratio"
			)
			DT::datatable(comparison, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
				DT::formatRound(columns = c('cutpoint', 'TP_FP_ratio', 'zz_2_frac', 'zz_4_frac'), digits = 2)
		})
		
		# Download Handlers ####
		output$download_results <- downloadHandler(
			filename = function() paste0("denoiser_results_", Sys.Date(), ".zip"),
			content = function(file) {
				temp_dir <- tempdir()
				results_dir <- file.path(temp_dir, "denoiser_results")
				dir.create(results_dir, showWarnings = FALSE)
				
				write.csv(rv$aab_called_data, file.path(results_dir, "AAb_called_data.csv"))
				write.csv(rv$cutpoint_results, file.path(results_dir, "cutpoint_analysis.csv"), row.names = FALSE)
				write.csv(rv$optimal_cutpoint, file.path(results_dir, "optimal_cutpoint.csv"), row.names = FALSE)
				
				for (i in seq_along(rv$denoise_results$denoised_data)) {
					write.csv(rv$denoise_results$denoised_data[[i]], file.path(results_dir, paste0("denoised_", i, "PCs.csv")))
				}
				zip(file, results_dir, flags = "-r9Xj")
			}
		)
		
		output$download_template <- downloadHandler(
			filename = function() paste0(input$template_name, ".R"),
			content = function(file) {
				generate_aab_caller_template(
					output_file = file, project_name = input$project_name, eset_file = "eset.rds",
					assay_name = input$assay_name, n_PCs = input$n_PCs, PN_AAbs = PN_AAbs_parsed(),
					exp_PN_AAbs = seq(input$exp_PN_min, input$exp_PN_max), annotation_cols = input$annotation_cols
				)
			}
		)
		
		# Pooled Normal Analysis - Data Preparation ####
		observe({
			req(rv$denoise_results)
			updateNumericInput(session, "pn_pc_level", value = input$pc_view_slider,
												 max = length(rv$denoise_results$denoised_data))
		})
		
		pn_data <- reactive({
			req(rv$denoise_results, eset_raw(), eset_norm())
			
			pc_level <- input$pn_pc_level
			if (pc_level > length(rv$denoise_results$denoised_data)) {
				pc_level <- length(rv$denoise_results$denoised_data)
			}
			
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			
			if (length(PN_samples) == 0) {
				showNotification("No Pooled Normal samples found!", type = "warning", duration = 5)
				return(NULL)
			}
			
			neti_data <- Biobase::exprs(eset_raw())
			norm_data <- Biobase::exprs(eset_norm())
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			
			pn_neti <- neti_data[, colnames(neti_data) %in% PN_samples, drop = FALSE]
			pn_norm <- norm_data[, colnames(norm_data) %in% PN_samples, drop = FALSE]
			pn_denoised <- denoised_data[, colnames(denoised_data) %in% PN_samples, drop = FALSE]
			
			pn_neti <- pn_neti[rowSums(pn_neti) != 0, , drop = FALSE]
			pn_norm <- pn_norm[rowSums(pn_norm) != 0, , drop = FALSE]
			pn_denoised <- pn_denoised[rowSums(pn_denoised) != 0, , drop = FALSE]
			
			pn_metadata <- metadata[colnames(pn_denoised), , drop = FALSE]
			
			list(pn_neti = pn_neti, pn_norm = pn_norm, pn_denoised = pn_denoised,
					 pn_metadata = pn_metadata, pc_level = pc_level)
		})
		
		aab_pn_data <- reactive({ 
			req(current_aab_data(), eset_raw(), eset_norm())
			
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			
			if (length(PN_samples) == 0) return(NULL)
			
			neti_data <- Biobase::exprs(eset_raw())
			norm_data <- Biobase::exprs(eset_norm())
			aab_data <- current_aab_data()
			
			pn_aab <- aab_data[, colnames(aab_data) %in% PN_samples, drop = FALSE]
			pn_aab <- pn_aab[rowSums(pn_aab) != 0, , drop = FALSE]
			
			pn_neti <- neti_data[rownames(pn_aab), colnames(neti_data) %in% PN_samples, drop = FALSE]
			pn_norm <- norm_data[rownames(pn_aab), colnames(norm_data) %in% PN_samples, drop = FALSE]

			
			pn_metadata <- metadata[colnames(pn_aab), , drop = FALSE]
			
			list(pn_neti = pn_neti, pn_norm = pn_norm, pn_denoised = pn_aab,
					 pn_metadata = pn_metadata, pc_level = input$pn_pc_level)
		})
		
		# Pooled Normal Analysis - Statistics ####
		output$pn_stats_table <- renderTable({
			req(pn_data())
			pn_denoised <- pn_data()$pn_denoised
			if (ncol(pn_denoised) == 0) return(data.frame(Metric = "No PN samples", Value = NA))
			
			data.frame(
				Metric = c("Number of PN Samples", "Number of Features", "Mean Expression", "Median Expression",
									 "SD Expression", "Min Expression", "Max Expression", "Features > 0", "% Features > 0"),
				Value = c(ncol(pn_denoised), nrow(pn_denoised), round(mean(pn_denoised), 3), round(median(pn_denoised), 3),
									round(sd(as.vector(pn_denoised)), 3), round(min(pn_denoised), 3), round(max(pn_denoised), 3),
									sum(rowMeans(pn_denoised) > 0), round(sum(rowMeans(pn_denoised) > 0) / nrow(pn_denoised) * 100, 1))
			)
		})
		
		output$pn_expected_aabs_table <- renderTable({
			req(pn_data())
			pn_denoised <- pn_data()$pn_denoised
			if (ncol(pn_denoised) == 0) return(data.frame(AAb = "No data", Status = NA))
			
			expected_aabs <- PN_AAbs_parsed()
			expected_in_data <- expected_aabs[expected_aabs %in% rownames(pn_denoised)]
			
			if (length(expected_in_data) == 0) {
				return(data.frame(AAb = "None found", Status = "Not detected in data"))
			}
			
			aab_results <- data.frame(
				AAb = expected_in_data,
				Mean_Expression = sapply(expected_in_data, function(aab) round(mean(pn_denoised[aab, ]), 3)),
				Positive_Samples = sapply(expected_in_data, function(aab) sum(pn_denoised[aab, ] > 0)),
				Percent_Positive = sapply(expected_in_data, function(aab) round(sum(pn_denoised[aab, ] > 0) / ncol(pn_denoised) * 100, 1)),
				stringsAsFactors = FALSE
			)
			aab_results$Status <- ifelse(aab_results$Percent_Positive >= 50, "✓ Detected", "⚠ Weak/Absent")
			aab_results
		})
		
		output$aab_pn_stats_table <- renderTable({
			req(aab_pn_data())
			pn_aab <- aab_pn_data()$pn_denoised
			data.frame(
				Metric = c("Number of PN Samples", "Total AAb Calls", "Unique AAbs Called",
									 "Mean AAbs per Sample", "Median AAbs per Sample", "Max AAbs in a Sample"),
				Value = c(ncol(pn_aab), sum(pn_aab > 0), sum(rowSums(pn_aab) > 0),
									round(mean(colSums(pn_aab)), 1), median(colSums(pn_aab)), max(colSums(pn_aab)))
			)
		})
		
		output$aab_pn_expected_aabs_table <- renderTable({
			req(aab_pn_data())
			pn_aab <- aab_pn_data()$pn_denoised
			expected_aabs <- PN_AAbs_parsed()
			expected_in_data <- expected_aabs[expected_aabs %in% rownames(pn_aab)]
			
			if (length(expected_in_data) == 0) {
				return(data.frame(AAb = "None found", Status = "Not detected"))
			}
			
			aab_results <- data.frame(
				AAb = expected_in_data,
				Calls = sapply(expected_in_data, function(aab) sum(pn_aab[aab, ] > 0)),
				Percent = sapply(expected_in_data, function(aab) round(sum(pn_aab[aab, ] > 0) / ncol(pn_aab) * 100, 1)),
				stringsAsFactors = FALSE
			)
			aab_results$Status <- ifelse(aab_results$Percent >= 50, "✓ Called", "⚠ Not Called")
			aab_results
		})
		
		output$aab_calls_per_pn_sample <- renderPlot({
			req(aab_pn_data())
			pn_aab <- aab_pn_data()$pn_denoised
			aab_counts <- colSums(pn_aab)
			barplot(aab_counts, main = "AAb Calls per PN Sample", xlab = "Sample", ylab = "Number of AAb Calls",
							col = "steelblue", las = 2, cex.names = 0.8)
			abline(h = median(aab_counts), col = "red", lwd = 2, lty = 2)
			legend("topright", legend = paste("Median:", median(aab_counts)), col = "red", lwd = 2, lty = 2, bty = "n")
		})
		
		# Pooled Normal Comparison Plot ####
		output$pn_comparison_plot <- renderPlot({
			req(pn_data())
			pn_denoised <- pn_data()$pn_denoised
			
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			
			all_denoised <- rv$denoise_results$denoised_data[[pn_data()$pc_level]]
			non_pn_data <- all_denoised[, !(colnames(all_denoised) %in% PN_samples), drop = FALSE]
			
			if (ncol(pn_denoised) == 0 || ncol(non_pn_data) == 0) return(NULL)
			
			comparison_df <- data.frame(
				Expression = c(as.vector(pn_denoised), as.vector(non_pn_data)),
				Group = rep(c("Pooled Normal", "Other Samples"), c(length(as.vector(pn_denoised)), length(as.vector(non_pn_data))))
			)
			
			p1 <- ggplot2::ggplot(comparison_df, ggplot2::aes(x = Expression, fill = Group)) +
				ggplot2::geom_density(alpha = 0.5) +
				ggplot2::scale_fill_manual(values = c("Pooled Normal" = "#2ca02c", "Other Samples" = "#1f77b4")) +
				ggplot2::labs(title = "Expression Distribution: PN vs Other Samples", x = "Expression Value", y = "Density") +
				ggplot2::theme_minimal(base_size = 12)
			
			p2 <- ggplot2::ggplot(comparison_df, ggplot2::aes(x = Group, y = Expression, fill = Group)) +
				ggplot2::geom_violin(alpha = 0.7) +
				ggplot2::geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
				ggplot2::scale_fill_manual(values = c("Pooled Normal" = "#2ca02c", "Other Samples" = "#1f77b4")) +
				ggplot2::labs(title = "Expression Distribution Comparison", x = "", y = "Expression Value") +
				ggplot2::theme_minimal(base_size = 12) +
				ggplot2::theme(legend.position = "none")
			
			gridExtra::grid.arrange(p1, p2, ncol = 2)
		})
		
		# Call PN Visualization Modules ####
		mod_pn_viz_server(
			id = "pn_neti",
			data = reactive({ req(pn_data()); pn_data()$pn_neti }),
			metadata = reactive({ req(pn_data()); pn_data()$pn_metadata }),
			title = "NetI Data",
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$pn_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$pn_cluster_samples }),
			show_density = TRUE
		)
		
		mod_pn_viz_server(
			id = "pn_norm",
			data = reactive({ req(pn_data()); pn_data()$pn_norm }),
			metadata = reactive({ req(pn_data()); pn_data()$pn_metadata }),
			title = "Normalized Data",
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$pn_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$pn_cluster_samples }),
			show_density = TRUE
		)
		
		mod_pn_viz_server(
			id = "pn_denoised",
			data = reactive({ req(pn_data()); pn_data()$pn_denoised }),
			metadata = reactive({ req(pn_data()); pn_data()$pn_metadata }),
			title = reactive({
				req(pn_data())
				pc_level <- pn_data()$pc_level
				paste0("Denoised Data (", pc_level, " PC", ifelse(pc_level > 1, "s", ""), " Removed)")
			}),
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$pn_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$pn_cluster_samples }),
			show_density = TRUE
		)
		
		# Call AAb PN Visualization Modules (no density for binary data) ####
		mod_pn_viz_server(
			id = "aab_pn_neti",
			data = reactive({ req(aab_pn_data()); aab_pn_data()$pn_neti }),
			metadata = reactive({ req(aab_pn_data()); aab_pn_data()$pn_metadata }),
			title = "PN - NetI Data",
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$aab_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$aab_cluster_samples }),
			show_density = FALSE
		)
		
		mod_pn_viz_server(
			id = "aab_pn_norm",
			data = reactive({ req(aab_pn_data()); aab_pn_data()$pn_norm }),
			metadata = reactive({ req(aab_pn_data()); aab_pn_data()$pn_metadata }),
			title = "PN - Normalized Data",
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$aab_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = reactive({ input$aab_cluster_samples }),
			show_density = FALSE
		)
		
		mod_pn_viz_server(
			id = "aab_pn_called",
			data = reactive({ req(aab_pn_data()); aab_pn_data()$pn_denoised }),
			metadata = reactive({ req(aab_pn_data()); aab_pn_data()$pn_metadata }),
			title = reactive({
				paste0("PN - AAb-Called Data (Cutpoint: ", input$cutpoint_slider, ")")
			}),
			annotation_cols = reactive({ input$annotation_cols }),
			highlight_features = reactive({ if (input$aab_show_expected_aabs) PN_AAbs_parsed() else NULL }),
			cluster_rows = FALSE,
			cluster_cols = FALSE,
			show_density = FALSE
		)
		
		# Return results ####
		return(reactive({
			list(
				denoise_results = rv$denoise_results,
				cutpoint_results = rv$cutpoint_results,
				optimal_cutpoint = rv$optimal_cutpoint,
				aab_called_data = rv$aab_called_data,
				updated_expset = rv$ExpSet_list
			)
		}))
	})
}

# Replace the debug observer with this enhanced version:

observeEvent(input$debug, {
	if (!interactive()) {
		showNotification(
			"Debug mode only works in interactive R sessions",
			type = "warning",
			duration = 5
		)
		return(NULL)
	}
	
	# Determine current state
	has_eset <- !is.null(eset_raw())
	has_denoise <- !is.null(rv$denoise_results)
	has_cutpoints <- !is.null(rv$cutpoint_results)
	has_aab_called <- !is.null(rv$aab_called_data)
	
	message("\n╔═══════════════════════════════════════════════════════════╗")
	message("║              🔍 DEBUG MODE ACTIVATED                      ║")
	message("╚═══════════════════════════════════════════════════════════╝")
	
	# Show current state
	message("\n📊 Current State:")
	message("   ExpressionSet loaded: ", ifelse(has_eset, "✅", "❌"))
	message("   Denoising completed: ", ifelse(has_denoise, "✅", "❌"))
	message("   Cutpoints analyzed: ", ifelse(has_cutpoints, "✅", "❌"))
	message("   AAb-called data: ", ifelse(has_aab_called, "✅", "❌"))
	
	# Show available objects
	message("\n📍 Available objects:")
	if (has_eset) {
		message("   ✓ eset_raw() - Raw/NetI ExpressionSet")
		if (!is.null(eset_norm) && !is.null(eset_norm())) {
			message("   ✓ eset_norm() - Normalized ExpressionSet")
		}
	}
	if (has_denoise) {
		message("   ✓ rv$denoise_results - Denoised data and PCA results")
	}
	if (has_cutpoints) {
		message("   ✓ rv$cutpoint_results - Cutpoint analysis table")
		message("   ✓ rv$optimal_cutpoint - Optimal parameters")
	}
	if (has_aab_called) {
		message("   ✓ rv$aab_called_data - Final AAb-called matrix")
	}
	message("   ✓ input - All UI input values")
	
	# Context-specific commands
	message("\n💡 Suggested commands for current state:")
	
	if (has_eset && !has_denoise) {
		message("\n   === BEFORE DENOISING ===")
		message("   # Quick inspection")
		message("   quick_inspect_eset(eset_raw(), input$assay_name)")
		message("")
		message("   # Check expression data")
		message("   expr <- Biobase::exprs(eset_raw())")
		message("   class(expr)")
		message("   dim(expr)")
		message("   summary(as.vector(expr))")
		message("")
		message("   # Check metadata")
		message("   meta <- Biobase::pData(eset_raw())")
		message("   table(meta$Sample_Group)")
		message("")
		message("   # Check PN samples")
		message("   PN_samples <- get_PN_samples(eset_raw())")
		message("   length(PN_samples)")
	}
	
	if (has_denoise && !has_cutpoints) {
		message("\n   === AFTER DENOISING, BEFORE CUTPOINTS ===")
		message("   # Check denoised data")
		message("   rv$denoise_results$variance_explained")
		message("   denoised_1PC <- rv$denoise_results$denoised_data[[1]]")
		message("   dim(denoised_1PC)")
		message("   head(denoised_1PC[, 1:5])")
	}
	
	if (has_cutpoints && !has_aab_called) {
		message("\n   === CUTPOINT ANALYSIS COMPLETE ===")
		message("   # View cutpoint results")
		message("   head(rv$cutpoint_results)")
		message("   rv$optimal_cutpoint")
		message("")
		message("   # Check specific cutpoint")
		message("   rv$cutpoint_results[rv$cutpoint_results$cutpoint == 1.0, ]")
	}
	
	if (has_aab_called) {
		message("\n   === FULL PIPELINE COMPLETE ===")
		message("   # Check AAb-called data")
		message("   dim(rv$aab_called_data)")
		message("   sum(rv$aab_called_data > 0)  # Total AAb calls")
		message("   colSums(rv$aab_called_data > 0)  # AAbs per sample")
		message("   rowSums(rv$aab_called_data > 0)  # Samples per antigen")
		message("")
		message("   # Summary statistics")
		message("   calculate_aab_summary(rv$aab_called_data, eset_raw())")
	}
	
	# Always show general commands
	message("\n   === GENERAL COMMANDS ===")
	message("   ls()                    # List all objects")
	message("   str(rv)                 # Structure of reactive values")
	message("   names(input)            # All input names")
	message("   input$n_PCs             # Check specific input")
	
	message("\n⌨️  Type 'c' to continue, 'Q' to quit, 'n' to step")
	message("═══════════════════════════════════════════════════════════\n")
	
	browser()
})