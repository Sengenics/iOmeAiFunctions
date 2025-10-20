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
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "Pooled Normal Samples - Denoised Data Heatmap",
									width = NULL,
									status = "success",
									solidHeader = TRUE,
									plotOutput(ns("pn_denoised_heatmap"), height = "800px")
								)
							),
							
							column(
								width = 12,
								shinydashboard::box(
									title = "Correlation Plot",
									width = NULL,
									status = "success",
									solidHeader = TRUE,
									plotOutput(ns("pn_denoised_correlation"), height = "800px")
								)
							)
						),
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
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "PN Samples - Expression Distribution",
									width = NULL,
									plotOutput(ns("pn_density_plot"), height = "400px")
								)
							)
						),
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
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb-Called Data Heatmap",
									width = NULL,
									plotOutput(ns("aab_called_heatmap"), height = "600px")
								)
							),
							
							column(
								width = 4,
								shinydashboard::box(
									title = "NetI",
									width = NULL,
									plotOutput(ns("aab_neti_pn_correlation"), height = "300px")
								)
							),
							column(
								width = 4,
								shinydashboard::box(
									title = "Norm",
									width = NULL,
									plotOutput(ns("aab_norm_pn_correlation"), height = "300px")
								)
							),
							column(
								width = 4,
								shinydashboard::box(
									title = "AAb-Called",
									width = NULL,
									plotOutput(ns("aab_called_pn_correlation"), height = "300px")
								)
							),
							
							column(
								width = 4,
								shinydashboard::box(
									title = "NetI",
									width = NULL,
									plotOutput(ns("aab_neti_pn_heatmap"), height = "300px")
								)
							),
							column(
								width = 4,
								shinydashboard::box(
									title = "Norm",
									width = NULL,
									plotOutput(ns("aab_norm_pn_heatmap"), height = "300px")
								)
							),
							column(
								width = 4,
								shinydashboard::box(
									title = "AAb-Called",
									width = NULL,
									plotOutput(ns("aab_called_pn_heatmap"), height = "300px")
								)
							)
						),
						fluidRow(
							column(
								width = 12,
								shinydashboard::box(
									title = "AAb Call Rate Distribution",
									width = NULL,
									plotOutput(ns("aab_call_rate_hist"), height = "400px")
								)
							)
						)
					),
					
					# Tab 4: Visualization
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
		
		observeEvent(input$denoise_mod_debug,{
			browser()
		})
		
		# Reactive values
		rv <- reactiveValues(
			denoise_results = NULL,
			cutpoint_results = NULL,
			optimal_cutpoint = NULL,
			aab_called_data = NULL,
			all_results = NULL
		)
		
		observeEvent(input$denoise_update_ExpSet,{ 
			
			(denoise_matrix = (rv$denoise_results$denoised_data[[length(rv$denoise_results$denoised_data)]]))
			str(denoise_matrix)
			colnames(denoise_matrix)
			rownames(denoise_matrix)
			#View(rv$cutpoint_results)
			#View(rv$optimal_cutpoint)
			(aab_called_data = rv$aab_called_data)
			str(aab_called_data)
			ExpSet_list = ExpSet_list()
			sample_ExpSet = ExpSet_list$sample_ExpSet
			
			dim(sample_ExpSet)
			dim(denoise_matrix)
			dim(aab_called_data)
			
			hist(as.numeric(as.matrix(denoise_matrix)))
			min(denoise_matrix)
			denoise_matrix_median = denoise_matrix + abs(min(denoise_matrix))
			hist(as.numeric(unlist(denoise_matrix_median)))
			
			denoise_l = denoise_matrix %>% 
				rownames_to_column('Feature') %>% 
				gather(key = Sample, value = denoise,-1)
			
			aab_l = aab_called_data %>% 
				rownames_to_column('Feature') %>% 
				gather(key = Sample, value = aab,-1)
			
			df_l = denoise_l %>% 
				left_join(aab_l) %>% 
				mutate(aab = ifelse(is.na(aab),0,aab))
			
			aab_m = df_l %>% 
				dplyr::select(Feature,Sample,aab) %>% 
				spread(key = Sample, value = aab) %>% 
				column_to_rownames('Feature') %>% 
				as.matrix()
			
			#View(df_l)
			
			sample_ExpSet = ExpSet_add_matrix_function(sample_ExpSet,denoise_matrix,'denoised')
			sample_ExpSet = ExpSet_add_matrix_function(sample_ExpSet,denoise_matrix_median,'denoised_median')
			sample_ExpSet = ExpSet_add_matrix_function(sample_ExpSet,aab_m,'aab_called')
			ExpSet_list$sample_ExpSet = sample_ExpSet
			rv$ExpSet_list = ExpSet_list
		})
		
		# Download handler for ExpressionSet
		output$download_expset <- downloadHandler(
			filename = function() {
				paste0("ExpressionSet_with_reconstruction_", 
							 format(Sys.time(), "%Y%m%d_%H%M%S"), 
							 ".rds")
			},
			content = function(file) {
				req(rv$ExpSet_list)
				
				withProgress(message = "Preparing download...", value = 0.5, {
					tryCatch({
						# Save the ExpressionSet
						saveRDS(rv$ExpSet_list, file)
						
						showNotification(
							"✓ ExpressionSet downloaded successfully",
							type = "message",
							duration = 3
						)
					}, error = function(e) {
						showNotification(
							paste("Error saving ExpressionSet:", e$message),
							type = "error",
							duration = 10
						)
					})
				})
			}
		)
		
		# Update UI choices based on eset
		observe({
			req(eset_raw()) 
			
			metadata <- Biobase::pData(eset_raw())
			
			# Update PN column choices
			updateSelectInput(
				session,
				"PN_column",
				choices = colnames(metadata),
				selected = if("Sample_Group" %in% colnames(metadata)) "Sample_Group" else colnames(metadata)[1]
			)
			
			# Update annotation choices
			updateSelectizeInput(
				session,
				"annotation_cols",
				choices = colnames(metadata),
				selected = if("Sample_Group" %in% colnames(metadata)) "Sample_Group" else NULL
			)
			
			# Update t-SNE color/shape choices
			updateSelectInput(session, "tsne_color", choices = colnames(metadata))
			updateSelectInput(
				session, 
				"tsne_shape", 
				choices = c("None" = "none", colnames(metadata))
			)
		})
		
		# Update PC slider max based on n_PCs input
		observe({
			updateSliderInput(
				session,
				"pc_view_slider",
				max = input$n_PCs,
				value = min(input$n_PCs)
			)
		})
		
		# Parse PN AAbs input
		PN_AAbs_parsed <- reactive({
			req(input$PN_AAbs)
			
			# Split by comma and trim whitespace
			aabs <- unlist(strsplit(input$PN_AAbs, ","))
			aabs <- trimws(aabs)
			aabs <- aabs[aabs != ""]
			
			return(aabs)
		})
		
		# Run denoising pipeline
		observeEvent(input$run_denoise, {
			req(eset_raw())
			
			# Show progress
			withProgress(message = 'Running denoising pipeline...', value = 0, {
				
				# Step 1: Validate inputs
				incProgress(0.1, detail = "Validating inputs...")
				tryCatch({
					validate_denoise_inputs(
						eset = eset_raw(),
						assay_name = input$assay_name,
						PN_column = input$PN_column,
						PN_value = input$PN_value
					)
				}, error = function(e) {
					showNotification(
						paste("Validation error:", e$message),
						type = "error",
						duration = 10
					)
					return(NULL)
				})
				
				# Step 2: Remove PCs
				incProgress(0.2, detail = "Removing principal components...")
				rv$denoise_results <- denoise_remove_PCs(
					eset = eset_raw(),
					assay_name = input$assay_name,
					n_PCs = input$n_PCs,
					scale = input$scale_pca,
					center = input$center_pca
				)
				
				# Step 3: Find cutpoints for each PC level
				incProgress(0.3, detail = "Testing cutpoints...")
				
				cut_seq <- seq(
					from = input$cut_min,
					to = input$cut_max,
					by = input$cut_step
				)
				
				all_cutpoint_results <- list()
				
				for (i in seq_along(rv$denoise_results$denoised_data)) {
					incProgress(
						0.3 / length(rv$denoise_results$denoised_data),
						detail = paste("Testing PC level", i, "...")
					)
					
					cutpoint_results <- denoise_find_cutpoints(
						denoised_data = rv$denoise_results$denoised_data[[i]],
						eset = eset_raw(),
						PN_column = input$PN_column,
						PN_value = input$PN_value,
						PN_AAbs = PN_AAbs_parsed(),
						cut_seq = cut_seq,
						method = input$method
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
					cutpoint = rv$optimal_cutpoint$cutpoint,
					method = input$method
				)
				
				# Update cutpoint slider
				updateSliderInput(
					session,
					"cutpoint_slider",
					min = input$cut_min,
					max = input$cut_max,
					value = rv$optimal_cutpoint$cutpoint,
					step = input$cut_step
				)
				
				incProgress(0.1, detail = "Complete!")
				
				showNotification(
					"Denoising complete!",
					type = "message",
					duration = 5
				)
				
				return(reactive({
					list(
						denoise_results = rv$denoise_results,    # This now includes pca_result!
						cutpoint_results = rv$cutpoint_results,
						optimal_cutpoint = rv$optimal_cutpoint,
						aab_called_data = rv$aab_called_data
					)
				}))
			})
		})
		
		# PCA variance plot
		output$pca_variance_plot <- renderPlot({
			req(rv$denoise_results)
			
			variance_df <- data.frame(
				PC = paste0("PC", seq_along(rv$denoise_results$variance_explained)),
				Variance = rv$denoise_results$variance_explained
			)
			
			ggplot2::ggplot(variance_df[1:min(20, nrow(variance_df)), ], 
											ggplot2::aes(x = PC, y = Variance)) +
				ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
				ggplot2::geom_vline(xintercept = input$n_PCs + 0.5, 
														linetype = "dashed", color = "red", size = 1) +
				ggplot2::labs(
					title = "Variance Explained by Principal Components",
					subtitle = paste("Red line indicates", input$n_PCs, "PC(s) to be removed"),
					x = "Principal Component",
					y = "Variance Explained (%)"
				) +
				ggplot2::theme_minimal() +
				ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
		})
		
		# Denoised info table
		output$denoised_info_table <- renderTable({
			req(rv$denoise_results)
			pc_level <- input$pc_view_slider
			
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			
			data.frame(
				Metric = c("PCs Removed", "Features", "Samples", "Max Value", "Min Value"),
				Value = c(
					pc_level,
					nrow(denoised_data),
					ncol(denoised_data),
					round(max(denoised_data), 2),
					round(min(denoised_data), 2)
				)
			)
		})
		
		# Denoised heatmap
		output$denoised_heatmap <- renderPlot({
			req(rv$denoise_results, eset_raw())
			pc_level <- input$pc_view_slider
			
			plot_denoise_heatmap(
				denoised_data = rv$denoise_results$denoised_data[[pc_level]],
				eset = eset_raw(),
				annotation_cols = input$annotation_cols,
				title = paste("Denoised Data -", pc_level, "PC(s) Removed"),
				show_rownames = FALSE,
				show_colnames = FALSE
			)
		})
		
		output$denoised_density <- renderPlot({
			req(rv$denoise_results, eset_raw())
			pc_level <- input$pc_view_slider
			
			data = rv$denoise_results$denoised_data[[pc_level]]
			plot_density_data(data,eset_raw())
			# plot_denoise_heatmap(
			# 	denoised_data = rv$denoise_results$denoised_data[[pc_level]],
			# 	eset = eset_raw(),
			# 	annotation_cols = input$annotation_cols,
			# 	title = paste("Denoised Data -", pc_level, "PC(s) Removed"),
			# 	show_rownames = FALSE,
			# 	show_colnames = FALSE
			# )
		})
		
		# Cutpoint summary plot
		output$cutpoint_summary_plot <- renderPlot({
			req(rv$cutpoint_results, rv$optimal_cutpoint)
			
			pc_level <- input$pc_view_slider
			cutpoint_subset <- rv$cutpoint_results[rv$cutpoint_results$PCs_removed == pc_level, ]
			
			plot_cutpoint_summary(
				cutpoint_results = cutpoint_subset,
				optimal_cutpoint = rv$optimal_cutpoint$cutpoint
			)
		})
		
		# Cutpoint results table
		output$cutpoint_table <- renderDT({
			req(rv$cutpoint_results)
			
			pc_level <- input$pc_view_slider
			cutpoint_subset <- rv$cutpoint_results[rv$cutpoint_results$PCs_removed == pc_level, ]
			
			DT::datatable(
				cutpoint_subset,
				options = list(
					pageLength = 10,
					scrollX = TRUE,
					order = list(list(3, 'desc'))  # Sort by TP_FP_ratio
				),
				rownames = FALSE
			) %>%
				DT::formatRound(columns = c('cutpoint', 'TP_FP_ratio', 'zz_2_frac', 'zz_4_frac'), digits = 2) %>%
				DT::formatStyle(
					'TP_FP_ratio',
					background = DT::styleColorBar(cutpoint_subset$TP_FP_ratio, 'lightblue'),
					backgroundSize = '100% 90%',
					backgroundRepeat = 'no-repeat',
					backgroundPosition = 'center'
				)
		})
		
		# Get AAb-called data for selected cutpoint
		current_aab_data <- reactive({
			#browser()
			req(rv$denoise_results, input$cutpoint_slider)
			
			pc_level <- input$pc_view_slider
			
			denoise_apply_cutpoint(
				denoised_data = rv$denoise_results$denoised_data[[pc_level]],
				cutpoint = input$cutpoint_slider,
				method = input$method
			)
		})
		
		# Selected cutpoint info
		output$selected_cutpoint_info <- renderTable({
			req(rv$cutpoint_results)
			
			pc_level <- input$pc_view_slider
			cutpoint_subset <- rv$cutpoint_results[
				rv$cutpoint_results$PCs_removed == pc_level &
					abs(rv$cutpoint_results$cutpoint - input$cutpoint_slider) < 0.01,
			]
			
			if (nrow(cutpoint_subset) > 0) {
				data.frame(
					Metric = c("Cutpoint", "PN AAb Count", "TP:FP Ratio", "ZZ_con2 Rate"),
					Value = c(
						cutpoint_subset$cutpoint[1],
						cutpoint_subset$PN_Aab_count_67_perc[1],
						round(cutpoint_subset$TP_FP_ratio[1], 2),
						round(cutpoint_subset$zz_2_frac[1], 3)
					)
				)
			} else {
				data.frame(Metric = "No data", Value = "-")
			}
		})
		
		# Optimal cutpoint table
		output$optimal_cutpoint_table <- renderTable({
			req(rv$optimal_cutpoint)
			
			data.frame(
				Parameter = c("PCs Removed", "Cutpoint", "PN AAb Count", "PN Hit Rate (%)", 
											"TP:FP Ratio", "Unique AAbs", "Median AAbs/Sample"),
				Value = c(
					rv$optimal_cutpoint$PCs_removed,
					rv$optimal_cutpoint$cutpoint,
					rv$optimal_cutpoint$PN_Aab_count_67_perc,
					round(rv$optimal_cutpoint$PN_AAb_hit_rate, 1),
					round(rv$optimal_cutpoint$TP_FP_ratio, 2),
					rv$optimal_cutpoint$N_unique_AAbs,
					rv$optimal_cutpoint$sample_AAb_median
				)
			)
		})
		
		## AAb-called ####
		output$aab_called_heatmap <- renderPlot({ 
			req(current_aab_data(), eset_raw())
			
			plot_denoise_heatmap(
				denoised_data = current_aab_data(),
				eset = eset_raw(),
				annotation_cols = input$annotation_cols,
				title = paste("AAb-Called Data - Cutpoint:", input$cutpoint_slider),
				show_rownames = TRUE,
				show_colnames = FALSE
			)
		})
		
		aab_called_pn_data = reactive({
			pn_samples = rownames(pn_data()$pn_metadata)
			data = current_aab_data()
			pn_data = data[,pn_samples]
			filtered_pn <- pn_data[rowSums(pn_data) != 0, ]
			str(pn_data)
			plot_data = filtered_pn
			plot_data
		})
		
		output$aab_called_pn_correlation = renderPlot({
			plot_data = aab_called_pn_data()
			correlation_title_plot_function(plot_data,method = 'pearson')
			
		})
		
		output$aab_called_pn_heatmap <- renderPlot({ 
			req(aab_called_pn_data(), eset_raw())
			plot_data = aab_called_pn_data()
			plot_denoise_heatmap(
				denoised_data = plot_data,
				eset = eset_raw(),
				annotation_cols = input$annotation_cols,
				title = paste("AAb-Called Data - Cutpoint:", input$cutpoint_slider),
				show_rownames = TRUE,
				show_colnames = FALSE
			)
		})
		
		
		aab_neti_pn_correlation_data = reactive({
			aab_pn_data = aab_called_pn_data()
			data = exprs(eset_raw())
			plot_data = data[rownames(aab_pn_data),colnames(aab_pn_data)]
			plot_data
		})
		output$aab_neti_pn_correlation = renderPlot({
			plot_data = aab_neti_pn_correlation_data()
			correlation_title_plot_function(plot_data,method = 'pearson')
		})
		
		output$aab_neti_pn_heatmap <- renderPlot({ 
			plot_data = aab_neti_pn_correlation_data()
			plot_denoise_heatmap(
				denoised_data = plot_data,
				eset = eset_raw(),
				annotation_cols = input$annotation_cols,
				title = paste("AAb-Called Data - Cutpoint:", input$cutpoint_slider),
				show_rownames = TRUE,
				show_colnames = FALSE
			)
		})
		
		aab_norm_pn_correlation_data = reactive({
			aab_pn_data = aab_called_pn_data()
			data = exprs(eset_norm())
			plot_data = data[rownames(aab_pn_data),colnames(aab_pn_data)]
			plot_data
		})
		output$aab_norm_pn_correlation = renderPlot({
			plot_data = aab_norm_pn_correlation_data()
			correlation_title_plot_function(plot_data,method = 'pearson')
		})
		
		output$aab_norm_pn_heatmap <- renderPlot({ 
			plot_data = aab_norm_pn_correlation_data()
			plot_denoise_heatmap(
				denoised_data = plot_data,
				eset = eset_raw(),
				annotation_cols = input$annotation_cols,
				title = paste("AAb-Called Data - Cutpoint:", input$cutpoint_slider),
				show_rownames = TRUE,
				show_colnames = FALSE
			)
		})
		
		# AAb call rate histogram
		output$aab_call_rate_hist <- renderPlot({
			req(current_aab_data())
			
			call_rates <- apply(current_aab_data(), 1, function(x) {
				length(which(x > 0)) / length(x) * 100
			})
			
			graphics::hist(
				call_rates,
				breaks = 30,
				main = "Distribution of AAb Call Rates",
				xlab = "Call Rate (%)",
				ylab = "Number of Antigens",
				col = "skyblue",
				border = "white"
			)
			graphics::abline(v = median(call_rates), col = "red", lwd = 2, lty = 2)
			graphics::legend("topright", 
											 legend = paste("Median:", round(median(call_rates), 1), "%"),
											 col = "red", lwd = 2, lty = 2, bty = "n")
		})
		
		# AAb borders plot
		# AAb borders plot
		output$aab_borders_plot <- renderPlot({
			req(current_aab_data(), eset_raw())
			
			# Get background data
			# if (!is.null(eset_norm) && !is.null(eset_norm())) {
			# 	background_data <- get_expression_data(eset_norm(), input$norm_assay_name)
			# } else {
			# 	background_data <- get_expression_data(eset_raw(), input$assay_name)
			# }
			
			background_data = exprs(eset_norm())
			
			# Ensure background_data is a matrix before transposing
			if (!is.matrix(background_data)) {
				background_data <- as.matrix(background_data)
			}
			
			# Row-center background data
			background_RC <- t(scale(t(background_data), scale = FALSE, center = TRUE))
			
			plot_denoise_borders(
				background_data = background_RC,
				aab_called_data = current_aab_data(),
				eset = eset_raw(),
				annotation_cols = input$annotation_cols,
				variable = if(length(input$annotation_cols) > 0) input$annotation_cols[1] else NULL,
				title = "AAb Borders on Row-Centered Data"
			)
		})
		# output$aab_borders_plot <- renderPlot({
		# 	req(current_aab_data(), eset_raw())
		# 	
		# 	# Get background data
		# 	if (!is.null(eset_norm) && !is.null(eset_norm())) {
		# 		background_data <- get_expression_data(eset_norm(), input$norm_assay_name)
		# 	} else {
		# 		background_data <- get_expression_data(eset_raw(), input$assay_name)
		# 	}
		# 	
		# 	# Row-center background data
		# 	background_RC <- t(scale(t(background_data), scale = FALSE, center = TRUE))
		# 	
		# 	plot_denoise_borders(
		# 		background_data = background_RC,
		# 		aab_called_data = current_aab_data(),
		# 		eset = eset_raw(),
		# 		annotation_cols = input$annotation_cols,
		# 		variable = if(length(input$annotation_cols) > 0) input$annotation_cols[1] else NULL,
		# 		title = "AAb Borders on Row-Centered Data"
		# 	)
		# })
		
		
		
		# t-SNE plot
		output$tsne_plot <- renderPlot({
			req(current_aab_data(), eset_raw())
			
			shape_by <- if(input$tsne_shape == "none") NULL else input$tsne_shape
			
			plot_denoise_tsne(
				aab_called_data = current_aab_data(),
				eset = eset_raw(),
				color_by = input$tsne_color,
				shape_by = shape_by,
				perplexity = input$tsne_perplexity,
				seed = 42
			)
		})
		
		# Summary statistics table
		output$summary_stats_table <- renderTable({
			req(rv$aab_called_data, eset_raw())
			
			calculate_aab_summary(
				aab_called_data = rv$aab_called_data,
				eset = eset_raw(),
				group_by = if(length(input$annotation_cols) > 0) input$annotation_cols[1] else NULL
			)
		})
		
		# Parameter comparison table
		output$parameter_comparison_table <- renderDT({
			req(rv$cutpoint_results)
			
			comparison <- compare_denoise_parameters(
				cutpoint_results = rv$cutpoint_results,
				top_n = 20,
				sort_by = "TP_FP_ratio"
			)
			
			DT::datatable(
				comparison,
				options = list(pageLength = 10, scrollX = TRUE),
				rownames = FALSE
			) %>%
				DT::formatRound(columns = c('cutpoint', 'TP_FP_ratio', 'zz_2_frac', 'zz_4_frac'), digits = 2)
		})
		
		# Download results
		output$download_results <- downloadHandler(
			filename = function() {
				paste0("denoiser_results_", Sys.Date(), ".zip")
			},
			content = function(file) {
				# Create temp directory
				temp_dir <- tempdir()
				results_dir <- file.path(temp_dir, "denoiser_results")
				dir.create(results_dir, showWarnings = FALSE)
				
				# Save AAb-called data
				write.csv(
					rv$aab_called_data,
					file.path(results_dir, "AAb_called_data.csv")
				)
				
				# Save cutpoint results
				write.csv(
					rv$cutpoint_results,
					file.path(results_dir, "cutpoint_analysis.csv"),
					row.names = FALSE
				)
				
				# Save optimal parameters
				write.csv(
					rv$optimal_cutpoint,
					file.path(results_dir, "optimal_cutpoint.csv"),
					row.names = FALSE
				)
				
				# Save denoised matrices
				for (i in seq_along(rv$denoise_results$denoised_data)) {
					write.csv(
						rv$denoise_results$denoised_data[[i]],
						file.path(results_dir, paste0("denoised_", i, "PCs.csv"))
					)
				}
				
				# Create zip file
				zip(file, results_dir, flags = "-r9Xj")
			}
		)
		
		# Download template
		output$download_template <- downloadHandler(
			filename = function() {
				paste0(input$template_name, ".R")
			},
			content = function(file) {
				generate_aab_caller_template(
					output_file = file,
					project_name = input$project_name,
					eset_file = "eset.rds",  # User should update this
					assay_name = input$assay_name,
					n_PCs = input$n_PCs,
					PN_AAbs = PN_AAbs_parsed(),
					exp_PN_AAbs = seq(input$exp_PN_min, input$exp_PN_max),
					annotation_cols = input$annotation_cols
				)
			}
		)
		
		
		
		# Add these after the existing output functions in mod_denoiser_server:
		
		## PN Denoised Heatmap ######
		
		# Sync pn_pc_level with the main pc_view_slider
		observe({
			req(rv$denoise_results)
			
			updateNumericInput(
				session,
				"pn_pc_level",
				value = input$pc_view_slider,  # Match the main PC slider
				max = length(rv$denoise_results$denoised_data)
			)
		})
		
		pn_data = reactive({
		
			req(rv$denoise_results, eset_raw())
			
			pc_level <- input$pn_pc_level
			if (pc_level > length(rv$denoise_results$denoised_data)) {
				pc_level <- length(rv$denoise_results$denoised_data)
			}
			
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			
			# Get PN sample identifiers
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			
			# Filter to only PN samples
			if (length(PN_samples) == 0) {
				showNotification(
					"No Pooled Normal samples found!",
					type = "warning",
					duration = 5
				)
				return(NULL)
			}
			
			
			# Subset to PN samples
			pn_data <- denoised_data[, colnames(denoised_data) %in% PN_samples, drop = FALSE]
			# Prepare annotation for PN samples
			pn_metadata <- metadata[colnames(pn_data), , drop = FALSE]
			list(pn_data = pn_data,
					 pn_metadata = pn_metadata)
		})
			
		output$pn_denoised_heatmap <- renderPlot({
			pc_level <- input$pn_pc_level
			pn_data_list = pn_data()
			pn_data = pn_data_list$pn_data
			pn_metadata = pn_data_list$pn_metadata
			
				
			if (ncol(pn_data) == 0) {
				showNotification(
					"No matching PN samples in denoised data!",
					type = "error",
					duration = 5
				)
				return(NULL)
			}
			
			# # Prepare annotation for PN samples
			# pn_metadata <- metadata[colnames(pn_data), , drop = FALSE]
			
			annotation_col <- NULL
			if (length(input$annotation_cols) > 0) {
				annotation_col <- pn_metadata[, input$annotation_cols, drop = FALSE]
			}
			
			# Highlight expected PN AAbs
			annotation_row <- NULL
			if (input$pn_show_expected_aabs) {
				expected_aabs <- PN_AAbs_parsed()
				annotation_row <- data.frame(
					ExpectedAAb = ifelse(rownames(pn_data) %in% expected_aabs, "Expected", "Other"),
					row.names = rownames(pn_data)
				)
			}
			
			# Create heatmap
			pheatmap::pheatmap(
				pn_data,
				main = paste0("Pooled Normal Samples - Denoised Data (", pc_level, " PC", 
											ifelse(pc_level > 1, "s", ""), " Removed)"),
				annotation_col = annotation_col,
				annotation_row = annotation_row,
				cluster_rows = TRUE,
				cluster_cols = input$pn_cluster_samples,
				show_rownames = TRUE,
				show_colnames = TRUE,
				fontsize_row = 8,
				fontsize_col = 10,
				color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
				annotation_colors = if (input$pn_show_expected_aabs) {
					list(ExpectedAAb = c(Expected = "#d62728", Other = "grey80"))
				} else NULL
			)
		})
		
		

		output$pn_denoised_correlation <- renderPlot({
			pn_data_list = pn_data()
			pn_data = pn_data_list$pn_data
			
			correlation_title_plot_function(pn_data)
		})
		
		
		
		# PN Statistics Table ####
		output$pn_stats_table <- renderTable({
			req(rv$denoise_results, eset_raw())
			
			pc_level <- input$pn_pc_level
			if (pc_level > length(rv$denoise_results$denoised_data)) {
				pc_level <- length(rv$denoise_results$denoised_data)
			}
			
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			
			# Get PN samples
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			
			pn_data <- denoised_data[, colnames(denoised_data) %in% PN_samples, drop = FALSE]
			
			if (ncol(pn_data) == 0) return(data.frame(Metric = "No PN samples", Value = NA))
			
			# Calculate statistics
			data.frame(
				Metric = c(
					"Number of PN Samples",
					"Number of Features",
					"Mean Expression",
					"Median Expression",
					"SD Expression",
					"Min Expression",
					"Max Expression",
					"Features > 0",
					"% Features > 0"
				),
				Value = c(
					ncol(pn_data),
					nrow(pn_data),
					round(mean(pn_data), 3),
					round(median(pn_data), 3),
					round(sd(as.vector(pn_data)), 3),
					round(min(pn_data), 3),
					round(max(pn_data), 3),
					sum(rowMeans(pn_data) > 0),
					round(sum(rowMeans(pn_data) > 0) / nrow(pn_data) * 100, 1)
				)
			)
		})
		
		# PN Expected AAbs Table ####
		output$pn_expected_aabs_table <- renderTable({
			req(rv$denoise_results, eset_raw())
			
			pc_level <- input$pn_pc_level
			if (pc_level > length(rv$denoise_results$denoised_data)) {
				pc_level <- length(rv$denoise_results$denoised_data)
			}
			
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			
			# Get PN samples
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			pn_data <- denoised_data[, colnames(denoised_data) %in% PN_samples, drop = FALSE]
			
			if (ncol(pn_data) == 0) return(data.frame(AAb = "No data", Status = NA))
			
			# Check expected AAbs
			expected_aabs <- PN_AAbs_parsed()
			expected_in_data <- expected_aabs[expected_aabs %in% rownames(pn_data)]
			
			if (length(expected_in_data) == 0) {
				return(data.frame(
					AAb = "None found",
					Status = "Not detected in data"
				))
			}
			
			# Calculate mean expression for each expected AAb
			aab_results <- data.frame(
				AAb = expected_in_data,
				Mean_Expression = sapply(expected_in_data, function(aab) {
					round(mean(pn_data[aab, ]), 3)
				}),
				Positive_Samples = sapply(expected_in_data, function(aab) {
					sum(pn_data[aab, ] > 0)
				}),
				Percent_Positive = sapply(expected_in_data, function(aab) {
					round(sum(pn_data[aab, ] > 0) / ncol(pn_data) * 100, 1)
				}),
				stringsAsFactors = FALSE
			)
			
			# Add detection status
			aab_results$Status <- ifelse(
				aab_results$Percent_Positive >= 50,
				"✓ Detected",
				"⚠ Weak/Absent"
			)
			
			aab_results
		})
		
		# PN Density Plot ####
		output$pn_density_plot <- renderPlot({
			req(rv$denoise_results, eset_raw())
			
			pc_level <- input$pn_pc_level
			if (pc_level > length(rv$denoise_results$denoised_data)) {
				pc_level <- length(rv$denoise_results$denoised_data)
			}
			
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			
			# Get PN samples
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			pn_data <- denoised_data[, colnames(denoised_data) %in% PN_samples, drop = FALSE]
			
			if (ncol(pn_data) == 0) return(NULL)
			
			# Convert to long format for ggplot
			pn_long <- data.frame(
				Sample = rep(colnames(pn_data), each = nrow(pn_data)),
				Expression = as.vector(pn_data)
			)
			
			# Create density plot
			ggplot2::ggplot(pn_long, ggplot2::aes(x = Expression, color = Sample)) +
				ggplot2::geom_density(size = 1, alpha = 0.7) +
				ggplot2::labs(
					title = "Expression Distribution in Pooled Normal Samples",
					subtitle = paste0("After removing ", pc_level, " PC", ifelse(pc_level > 1, "s", "")),
					x = "Expression Value",
					y = "Density"
				) +
				ggplot2::theme_minimal(base_size = 14) +
				ggplot2::theme(legend.position = "right")
		})
		
		# PN Comparison Plot ####
		output$pn_comparison_plot <- renderPlot({
			req(rv$denoise_results, eset_raw())
			
			pc_level <- input$pn_pc_level
			if (pc_level > length(rv$denoise_results$denoised_data)) {
				pc_level <- length(rv$denoise_results$denoised_data)
			}
			
			denoised_data <- rv$denoise_results$denoised_data[[pc_level]]
			
			# Get PN and non-PN samples
			metadata <- Biobase::pData(eset_raw())
			PN_samples <- rownames(metadata)[metadata[[input$PN_column]] == input$PN_value]
			
			pn_data <- denoised_data[, colnames(denoised_data) %in% PN_samples, drop = FALSE]
			non_pn_data <- denoised_data[, !(colnames(denoised_data) %in% PN_samples), drop = FALSE]
			
			if (ncol(pn_data) == 0 || ncol(non_pn_data) == 0) {
				return(NULL)
			}
			
			# Prepare comparison data
			comparison_df <- data.frame(
				Expression = c(as.vector(pn_data), as.vector(non_pn_data)),
				Group = rep(c("Pooled Normal", "Other Samples"), 
										c(length(as.vector(pn_data)), length(as.vector(non_pn_data))))
			)
			
			# Create comparison plots
			p1 <- ggplot2::ggplot(comparison_df, ggplot2::aes(x = Expression, fill = Group)) +
				ggplot2::geom_density(alpha = 0.5) +
				ggplot2::scale_fill_manual(values = c("Pooled Normal" = "#2ca02c", "Other Samples" = "#1f77b4")) +
				ggplot2::labs(
					title = "Expression Distribution: PN vs Other Samples",
					x = "Expression Value",
					y = "Density"
				) +
				ggplot2::theme_minimal(base_size = 12)
			
			p2 <- ggplot2::ggplot(comparison_df, ggplot2::aes(x = Group, y = Expression, fill = Group)) +
				ggplot2::geom_violin(alpha = 0.7) +
				ggplot2::geom_boxplot(width = 0.2, alpha = 0.8, outlier.alpha = 0.3) +
				ggplot2::scale_fill_manual(values = c("Pooled Normal" = "#2ca02c", "Other Samples" = "#1f77b4")) +
				ggplot2::labs(
					title = "Expression Distribution Comparison",
					x = "",
					y = "Expression Value"
				) +
				ggplot2::theme_minimal(base_size = 12) +
				ggplot2::theme(legend.position = "none")
			
			# Combine plots
			gridExtra::grid.arrange(p1, p2, ncol = 2)
		})
		
		# Update the pn_pc_level slider max when denoising completes
		observe({
			req(rv$denoise_results)
			
			updateNumericInput(
				session,
				"pn_pc_level",
				max = length(rv$denoise_results$denoised_data),
				value = min(input$pn_pc_level, length(rv$denoise_results$denoised_data))
			)
		})
		
		
		# Return reactive values for use by parent app
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