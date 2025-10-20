#' PC Removal Visualizer Module UI
#'
#' @param id Module ID
#' @export
mod_pc_visualizer_ui <- function(id) {
	ns <- NS(id)
	
	tagList( 
		fluidRow(
			actionButton('mod_remove_debug','Mod PC Debug'),
			shinydashboard::box(
				title = "Principal Component Visualization",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				
				p(icon("info-circle"), 
					strong("Note:"), 
					"This module uses the PCA results from the Denoiser tab. ",
					"Please run the Denoiser first to compute principal components."),
				
				hr(),
				
				uiOutput(ns("pca_status_ui"))
			)
		),
		
		# Methodology explanation box
		fluidRow(
			shinydashboard::box(
				title = "How PCA Reconstruction Works",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				h4(icon("book"), " Understanding the Process"),
				
				tags$div(
					style = "padding: 10px; background-color: #f8f9fa; border-left: 4px solid #007bff;",
					h5(strong("1. Data Preparation (Centering & Scaling)")),
					p("Before PCA, the data is transformed:"),
					tags$ul(
						tags$li(strong("Centering:"), " Subtracts the mean of each sample, making the average expression = 0. 
						                This removes baseline differences between samples."),
						tags$li(strong("Scaling:"), " Divides by standard deviation, making variance = 1 for each sample. 
						                This ensures all samples contribute equally regardless of their original magnitude.")
					),
					tags$code("centered_scaled_data = (original_data - mean) / sd")
				),
				
				br(),
				
				tags$div(
					style = "padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107;",
					h5(strong("2. PCA Decomposition")),
					p("PCA breaks down your centered/scaled data into independent components:"),
					tags$ul(
						tags$li(strong("Feature Scores (x):"), " [1745 features × 42 PCs] - Shows how much each feature contributes to each PC"),
						tags$li(strong("Sample Loadings (rotation):"), " [42 samples × 42 PCs] - Shows how much each sample contributes to each PC"),
						tags$li(strong("Standard Deviations (sdev):"), " Amount of variance explained by each PC")
					),
					p(strong("Key insight:"), " Each PC captures a different pattern of variation in your data. PC1 captures the most variance, PC2 the second most, etc.")
				),
				
				br(),
				
				tags$div(
					style = "padding: 10px; background-color: #d1ecf1; border-left: 4px solid #17a2b8;",
					h5(strong("3. Reconstruction Mathematics")),
					p("The centered/scaled data can be perfectly reconstructed by:"),
					tags$code("reconstructed = x %*% t(rotation)"),
					p(),
					p("Where:"),
					tags$ul(
						tags$li("x is [1745 features × 42 PCs]"),
						tags$li("t(rotation) is [42 PCs × 42 samples] (transposed)"),
						tags$li("Result is [1745 features × 42 samples]")
					),
					p(strong("Full equation:"), " Original Data = PC1 + PC2 + PC3 + ... + PC42")
				),
				
				br(),
				
				tags$div(
					style = "padding: 10px; background-color: #f8d7da; border-left: 4px solid #dc3545;",
					h5(strong("4. Component Removal")),
					p("To remove specific PCs (e.g., noise in PC1 and PC2):"),
					tags$ol(
						tags$li("Zero out selected PCs: ", tags$code("x[, c(1,2)] = 0")),
						tags$li("Reconstruct: ", tags$code("reconstructed = x_modified %*% t(rotation)")),
						tags$li("Result: ", tags$code("Reconstructed Data = 0 + 0 + PC3 + ... + PC42"))
					),
					p(strong("What was removed:"), " ", tags$code("Removed = PC1 + PC2"))
				),
				
				br(),
				
				tags$div(
					style = "padding: 10px; background-color: #d4edda; border-left: 4px solid #28a745;",
					h5(strong("5. Reverting Transformations")),
					p("After reconstruction, you can reverse the centering and scaling:"),
					tags$ul(
						tags$li(strong("If 'Center data' is unchecked:"), " Adds back the sample means: ", 
										tags$code("data = data + mean")),
						tags$li(strong("If 'Scale data' is unchecked:"), " Multiplies by standard deviations: ", 
										tags$code("data = data * sd")),
						tags$li(strong("If 'Rescale samples' is checked:"), " Adjusts each sample's total to match the original: ", 
										tags$code("data = data * (original_total / current_total)"))
					),
					p(strong("Note:"), " The PCA was performed on centered AND scaled data. Unchecking these options reverses those transformations to bring your data back toward its original scale.")
				),
				
				br(),
				
				tags$div(
					style = "padding: 10px; background-color: #e8daef; border-left: 4px solid #8e44ad;",
					h5(strong("6. Post-Processing Options")),
					tags$ul(
						tags$li(strong("Shift to remove negatives:"), " Adds a constant to make minimum value = 1. Useful when centering creates negative values: ", 
										tags$code("data = data + (1 - min(data))")),
						tags$li(strong("Convert to RFU:"), " Back-transforms from log2 to original RFU scale: ", 
										tags$code("RFU = 2^(log2_data)"),
										". Only available when data is not centered or scaled.")
					),
					p(strong("Note:"), " These transformations are applied AFTER reconstruction and help interpret results in more familiar units.")
				),
				
				br(),
				
				tags$div(
					style = "padding: 10px; background-color: #e7e8ea; border-left: 4px solid #6c757d;",
					h5(strong("7. Interpretation")),
					tags$ul(
						tags$li(strong("Original Data:"), " Your raw expression values"),
						tags$li(strong("Reconstructed Data:"), " Original data with selected PCs removed (potential noise or batch effects)"),
						tags$li(strong("Removed Components:"), " The signal that was filtered out - this could be technical noise, batch effects, or biological signal depending on what the PCs represent"),
						tags$li(strong("Difference Plot:"), " Shows exactly what changed (Reconstructed - Original)")
					)
				),
				
				br(),
				
				tags$div(
					style = "padding: 15px; background-color: #fff; border: 2px solid #007bff;",
					h5(icon("lightbulb"), strong(" Practical Tips")),
					tags$ul(
						tags$li("Start by examining PC1 and PC2 - they often capture the most variance"),
						tags$li("Use the variance plot to see how much signal each PC contains"),
						tags$li("Check if removed components show batch patterns (use sample group coloring)"),
						tags$li("Compare heatmaps WITHOUT clustering to see if PCs affect specific regions of features"),
						tags$li("The 'Rescale samples' option is useful when you want to maintain original intensity ranges"),
						tags$li("Use 'Shift to remove negatives' when you want all positive values (e.g., for downstream analysis)"),
						tags$li("Use 'Show as RFU' to see results in original fluorescence units (only works with uncentered/unscaled data)")
					)
				)
			)
		),
		
		fluidRow(
			shinydashboard::box(
				title = "Principal Component Selection",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				
				uiOutput(ns("pc_selector_ui")),
				
				hr(),
				
				fluidRow(
					column(
						width = 3,
						checkboxInput(
							ns("center_data"),
							"Center data during PCA",
							value = TRUE
						),
						helpText("Subtract mean from each sample")
					),
					column(
						width = 3,
						checkboxInput(
							ns("scale_data"),
							"Scale data during PCA",
							value = TRUE
						),
						helpText("Divide by standard deviation")
					),
					column(
						width = 3,
						checkboxInput(
							ns("rescale_samples"),
							"Rescale samples to original totals",
							value = TRUE
						),
						helpText("Maintains original total intensity per sample")
					),
					column(
						width = 3,
						actionButton(
							ns("reconstruct"),
							"Reconstruct Data",
							icon = icon("sync"),
							class = "btn-success btn-block btn-lg"
						)
					)
				),
				
				hr(),
				
				h4("Post-Processing Options"),
				p("These options are applied AFTER reconstruction to transform the data for visualization and analysis."),
				
				fluidRow(
					column(
						width = 6,
						checkboxInput(
							ns("shift_to_positive"),
							"Shift data to remove negative values (min = 1)",
							value = FALSE
						),
						helpText("Adds a constant so the minimum value is 1. Useful for log-scale data after centering.")
					),
					column(
						width = 6,
						checkboxInput(
							ns("show_as_rfu"),
							"Show as RFU (back-transform from log2)",
							value = FALSE
						),
						helpText("Converts log2 values back to RFU: 2^(log2_value). Only available when data is not centered/scaled.")
					)
				),
				
				hr(),
				
				fluidRow(
					column(width = 4, valueBoxOutput(ns("box_original"), width = NULL)),
					column(width = 4, valueBoxOutput(ns("box_selected"), width = NULL)),
					column(width = 4, valueBoxOutput(ns("box_variance"), width = NULL))
				)
			)
		),
		
		fluidRow(
			shinydashboard::box(
				title = "Visualization Controls",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				fluidRow(
					column(
						width = 3,
						selectInput(
							ns("sample_group"),
							"Color samples by:",
							choices = NULL
						)
					),
					column(
						width = 3,
						selectInput(
							ns("density_type"),
							"Density plot type:",
							choices = c(
								"Overlay all samples" = "overlay",
								"Facet by group" = "facet",
								"Ridgeline plot" = "ridge"
							),
							selected = "overlay"
						)
					),
					column(
						width = 3,
						selectInput(
							ns("heatmap_scale"),
							"Heatmap scaling:",
							choices = c(
								"None" = "none",
								"Row (features)" = "row",
								"Column (samples)" = "column"
							),
							selected = "none"
						)
					),
					column(
						width = 3,
						checkboxInput(
							ns("show_features"),
							"Show feature names in heatmap",
							value = FALSE
						)
					)
				),
				
				hr(),
				
				fluidRow(
					column(
						width = 6,
						h4("Heatmap Clustering Options"),
						checkboxInput(
							ns("cluster_rows"),
							"Cluster rows (features)",
							value = FALSE
						)
					),
					column(
						width = 6,
						br(),
						checkboxInput(
							ns("cluster_cols"),
							"Cluster columns (samples)",
							value = FALSE
						)
					)
				)
			)
		),
		
		fluidRow(
			column(
				width = 6,
				shinydashboard::box(
					title = "Original Data",
					width = NULL,
					status = "warning",
					solidHeader = TRUE,
					
					tabsetPanel(
						id = ns("tabs_original"),
						tabPanel(
							"Density Plot",
							plotOutput(ns("density_original"), height = "500px")
						),
						tabPanel(
							"Heatmap",
							plotOutput(ns("heatmap_original"), height = "600px")
						),
						tabPanel(
							"Sample Statistics",
							plotOutput(ns("stats_original"), height = "500px")
						)
					)
				)
			),
			column(
				width = 6,
				shinydashboard::box(
					title = "Reconstructed Data (Selected PCs Removed)",
					width = NULL,
					status = "success",
					solidHeader = TRUE,
					
					tabsetPanel(
						id = ns("tabs_reconstructed"),
						tabPanel(
							"Density Plot",
							plotOutput(ns("density_reconstructed"), height = "500px")
						),
						tabPanel(
							"Heatmap",
							plotOutput(ns("heatmap_reconstructed"), height = "600px")
						),
						tabPanel(
							"Sample Statistics",
							plotOutput(ns("stats_reconstructed"), height = "500px")
						),
						tabPanel(
							"Diff",
							plotOutput(ns("difference_plot"), height = "500px")
						)
					)
				)
			)
		),
		
		# Removed Components Visualization
		fluidRow(
			shinydashboard::box(
				title = "Removed Components Visualization",
				width = 12,
				status = "danger",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = FALSE,
				
				p(icon("info-circle"), 
					strong("This section shows the data that was removed from the reconstruction."),
					"Each panel represents the contribution of one removed principal component."),
				
				hr(),
				
				fluidRow(
					column(
						width = 6,
						h4("Density Plots by Component"),
						p("Each facet shows the distribution of values contributed by that PC"),
						plotOutput(ns("removed_density_facet"), height = "600px")
					),
					column(
						width = 6,
						h4("Composite Heatmap of All Removed Components"),
						p("Shows the combined signal from all removed components"),
						plotOutput(ns("removed_heatmap_composite"), height = "600px")
					)
				),
				
				hr(),
				
				h4("Individual Component Heatmaps"),
				p("Explore the contribution of each removed component individually"),
				
				fluidRow(
					column(
						width = 3,
						uiOutput(ns("removed_pc_selector"))
					),
					column(
						width = 9,
						plotOutput(ns("removed_heatmap_individual"), height = "500px")
					)
				)
			)
		),
		
		fluidRow(
			shinydashboard::box(
				title = "Variance Explained by Each PC",
				width = 12,
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = FALSE,
				
				plotOutput(ns("variance_plot"), height = "400px")
			)
		),
		
		fluidRow(
			shinydashboard::box(
				title = "PC Loadings Heatmap",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				numericInput(
					ns("top_features"),
					"Number of top loading features to show:",
					value = 50,
					min = 10,
					max = 200,
					step = 10
				),
				plotOutput(ns("loadings_heatmap"), height = "800px")
			)
		)
	)
}

#' PC Removal Visualizer Module Server
#'
#' @param id Module ID
#' @param eset_raw Reactive expression returning raw ExpressionSet
#' @param denoiser_results Reactive expression returning results from denoiser module
#' @export
mod_pc_visualizer_server <- function(id, eset_raw, denoiser_results) {
	moduleServer(id, function(input, output, session) {
		
		observeEvent(input$mod_remove_debug,{
			browser()
		})
		# Reactive values
		rv <- reactiveValues(
			reconstructed_data = NULL,
			selected_pcs = NULL,
			removed_components = NULL,
			removed_composite = NULL
		)
		
		# Extract PCA results from denoiser
		pca_result <- reactive({
			req(denoiser_results())
			
			results <- denoiser_results()
			
			# Check if PCA results exist
			if (is.null(results$denoise_results)) {
				return(NULL)
			}
			
			if (is.null(results$denoise_results$pca_result)) {
				return(NULL)
			}
			
			return(results$denoise_results$pca_result)
		})
		
		original_data <- reactive({
			req(eset_raw())
			req(denoiser_results())
			
			results <- denoiser_results()
			expr_data <- Biobase::exprs(eset_raw())
			
			return(expr_data)
		})
		
		# Disable RFU option when data is centered or scaled
		observe({
			if (input$center_data || input$scale_data) {
				updateCheckboxInput(session, "show_as_rfu", value = FALSE)
				shinyjs::disable("show_as_rfu")
			} else {
				shinyjs::enable("show_as_rfu")
			}
		})
		
		# Function to apply post-processing transformations
		apply_postprocessing <- function(data, apply_shift = FALSE, apply_rfu = FALSE) {
			if (is.null(data)) return(NULL)
			
			result <- data
			
			# Apply shift to remove negatives (make min = 1)
			if (apply_shift) {
				min_val <- min(result, na.rm = TRUE)
				if (min_val < 1) {
					shift_amount <- 1 - min_val
					result <- result + shift_amount
					message("Applied shift: added ", round(shift_amount, 4), " to make min = 1")
				}
			}
			
			# Convert to RFU (back-transform from log2)
			if (apply_rfu) {
				result <- 2^result
				message("Converted to RFU: 2^(log2_value)")
			}
			
			return(result)
		}
		
		# Reactive for processed original data
		original_data_processed <- reactive({
			req(original_data())
			apply_postprocessing(
				original_data(),
				apply_shift = input$shift_to_positive,
				apply_rfu = input$show_as_rfu
			)
		})
		
		# Reactive for processed reconstructed data
		reconstructed_data_processed <- reactive({
			req(rv$reconstructed_data)
			apply_postprocessing(
				rv$reconstructed_data,
				apply_shift = input$shift_to_positive,
				apply_rfu = input$show_as_rfu
			)
		})
		
		# Reactive for processed removed composite
		removed_composite_processed <- reactive({
			req(rv$removed_composite)
			apply_postprocessing(
				rv$removed_composite,
				apply_shift = input$shift_to_positive,
				apply_rfu = input$show_as_rfu
			)
		})
		
		# Reactive for processed individual removed components
		removed_components_processed <- reactive({
			req(rv$removed_components)
			lapply(rv$removed_components, function(comp) {
				apply_postprocessing(
					comp,
					apply_shift = input$shift_to_positive,
					apply_rfu = input$show_as_rfu
				)
			})
		})
		
		# PCA status indicator
		output$pca_status_ui <- renderUI({
			ns <- session$ns
			
			if (is.null(pca_result())) {
				div(
					class = "alert alert-warning",
					icon("exclamation-triangle"),
					strong(" PCA not computed yet."),
					" Please go to the Denoiser tab and run the denoising analysis first.",
					br(), br(),
					actionButton(
						ns("goto_denoiser"),
						"Go to Denoiser Tab",
						icon = icon("arrow-right"),
						class = "btn-primary"
					)
				)
			} else {
				pca <- pca_result()
				orig <- original_data()
				n_pcs <- ncol(pca$x)
				
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" PCA Results Loaded"),
					br(),
					sprintf("• %d principal components computed", n_pcs),
					br(),
					sprintf("• %d features × %d samples", 
									nrow(orig), 
									ncol(orig))
				)
			}
		})
		
		# Navigate to denoiser tab
		observeEvent(input$goto_denoiser, {
			updateTabItems(session = getDefaultReactiveDomain()$parent, inputId = "sidebar", selected = "denoise")
		})
		
		# Update PC selector UI after PCA is available
		output$pc_selector_ui <- renderUI({
			req(pca_result())
			
			pca <- pca_result()
			ns <- session$ns
			n_pcs <- ncol(pca$x)
			
			tagList(
				h4("Select PCs to REMOVE:"),
				p("Click on PCs you want to remove from the data. You can select any combination."),
				
				fluidRow(
					lapply(1:n_pcs, function(i) {
						var_explained <- (pca$sdev[i]^2 / sum(pca$sdev^2)) * 100
						
						column(
							width = if(n_pcs <= 6) 2 else if(n_pcs <= 12) 1 else 1,
							checkboxInput(
								ns(paste0("pc_", i)),
								HTML(sprintf(
									"<b>PC%d</b><br><small>%.1f%%</small>",
									i, var_explained
								)),
								value = FALSE
							)
						)
					})
				),
				
				hr(),
				
				fluidRow(
					column(
						width = 3,
						numericInput(
							ns("n_sequential"),
							"Select first N PCs:",
							value = 1,
							min = 1,
							max = n_pcs,
							step = 1
						)
					),
					column(
						width = 3,
						br(),
						actionButton(
							ns("apply_sequential"),
							"Apply Sequential",
							class = "btn-warning btn-block"
						)
					),
					column(
						width = 3,
						br(),
						actionButton(
							ns("clear_selection"),
							"Clear All",
							class = "btn-secondary btn-block"
						)
					),
					column(
						width = 3,
						br(),
						actionButton(
							ns("select_all"),
							"Select All",
							class = "btn-info btn-block"
						)
					)
				)
			)
		})
		
		# Sequential selection
		observeEvent(input$apply_sequential, {
			req(pca_result())
			req(input$n_sequential)
			
			n_pcs <- ncol(pca_result()$x)
			
			for (i in 1:n_pcs) {
				updateCheckboxInput(session, paste0("pc_", i), value = (i <= input$n_sequential))
			}
		})
		
		# Clear selection
		observeEvent(input$clear_selection, {
			req(pca_result())
			n_pcs <- ncol(pca_result()$x)
			
			for (i in 1:n_pcs) {
				updateCheckboxInput(session, paste0("pc_", i), value = FALSE)
			}
		})
		
		# Select all
		observeEvent(input$select_all, {
			req(pca_result())
			n_pcs <- ncol(pca_result()$x)
			
			for (i in 1:n_pcs) {
				updateCheckboxInput(session, paste0("pc_", i), value = TRUE)
			}
		})
		
		# Reconstruct data with selected PCs removed
		observeEvent(input$reconstruct, {
			req(pca_result())
			req(original_data())
			
			withProgress(message = "Reconstructing data...", value = 0, {
				
				pca <- pca_result()
				orig_data <- original_data()
				n_pcs <- ncol(pca$x)
				
				# Get selected PCs to remove
				selected_pcs <- sapply(1:n_pcs, function(i) {
					isTRUE(input[[paste0("pc_", i)]])
				})
				
				rv$selected_pcs <- which(selected_pcs)
				
				incProgress(0.2, detail = "Calculating removed components")
				
				# Calculate individual removed components
				if (length(rv$selected_pcs) > 0) {
					feature_scores <- pca$x
					sample_loadings <- pca$rotation
					
					# Store each removed component separately
					rv$removed_components <- lapply(rv$selected_pcs, function(pc_idx) {
						# Reconstruct just this one PC
						component_data <- feature_scores[, pc_idx, drop = FALSE] %*% 
							t(sample_loadings[, pc_idx, drop = FALSE])
						
						# Apply scaling/centering adjustments
						if (!input$scale_data && !is.null(pca$scale) && any(pca$scale != FALSE)) {
							component_data <- sweep(component_data, 2, pca$scale, "*")
						}
						if (!input$center_data && !is.null(pca$center)) {
							component_data <- sweep(component_data, 2, pca$center, "+")
						}
						
						colnames(component_data) <- colnames(orig_data)
						rownames(component_data) <- rownames(orig_data)
						
						return(component_data)
					})
					names(rv$removed_components) <- paste0("PC", rv$selected_pcs)
					
					# Calculate composite (sum of all removed components)
					rv$removed_composite <- Reduce("+", rv$removed_components)
					
				} else {
					rv$removed_components <- NULL
					rv$removed_composite <- NULL
				}
				
				incProgress(0.3, detail = "Removing selected PCs")
				
				if (length(rv$selected_pcs) == 0) {
					# No PCs to remove - return original data
					if (input$center_data || input$scale_data) {
						rv$reconstructed_data <- t(scale(
							t(orig_data), 
							center = input$center_data, 
							scale = input$scale_data
						))
					} else {
						rv$reconstructed_data <- orig_data
					}
					message("No PCs selected - returning ", 
									if(input$center_data) "centered " else "",
									if(input$scale_data) "scaled " else "",
									"original data")
				} else {
					message("Removing PCs: ", paste(rv$selected_pcs, collapse = ", "))
					message("Center: ", input$center_data, ", Scale: ", input$scale_data)
					
					feature_scores <- pca$x
					sample_loadings <- pca$rotation
					
					# Zero out selected PCs
					feature_scores_modified <- feature_scores
					feature_scores_modified[, rv$selected_pcs] <- 0
					
					# Reconstruct
					reconstructed <- feature_scores_modified %*% t(sample_loadings)
					
					# Apply scaling/centering adjustments
					if (!input$scale_data && !is.null(pca$scale) && any(pca$scale != FALSE)) {
						message("Unscaling data...")
						reconstructed <- sweep(reconstructed, 2, pca$scale, "*")
					}
					
					if (!input$center_data && !is.null(pca$center)) {
						message("Uncentering data...")
						reconstructed <- sweep(reconstructed, 2, pca$center, "+")
					}
					
					# Apply sample-level rescaling if requested
					if (input$rescale_samples && !input$center_data && !input$scale_data) {
						incProgress(0.6, detail = "Rescaling samples to match original totals")
						
						message("\n========== RESCALING DEBUG ==========")
						message("Rescale option enabled: TRUE")
						message("Center data: ", input$center_data, " | Scale data: ", input$scale_data)
						
						# Calculate original sample totals (column sums)
						original_totals <- colSums(orig_data)
						reconstructed_totals_before <- colSums(reconstructed)
						
						message("\nBEFORE RESCALING:")
						message("  Original totals    - Range: [", round(min(original_totals), 1), 
										" to ", round(max(original_totals), 1), "]")
						message("  Reconstructed totals - Range: [", round(min(reconstructed_totals_before), 1), 
										" to ", round(max(reconstructed_totals_before), 1), "]")
						message("  Difference - Range: [", round(min(original_totals - reconstructed_totals_before), 1), 
										" to ", round(max(original_totals - reconstructed_totals_before), 1), "]")
						
						# Check for zero totals
						if (any(reconstructed_totals_before == 0)) {
							message("  WARNING: Some reconstructed samples have zero total!")
							reconstructed_totals_before[reconstructed_totals_before == 0] <- 1
						}
						
						# Rescale each sample to match original total
						scaling_factors <- original_totals / reconstructed_totals_before
						
						message("\nSCALING FACTORS:")
						message("  Range: [", round(min(scaling_factors), 4), " to ", round(max(scaling_factors), 4), "]")
						message("  Mean: ", round(mean(scaling_factors), 4))
						message("  All close to 1.0? ", all(abs(scaling_factors - 1) < 0.01))
						
						# Apply the scaling
						reconstructed <- sweep(reconstructed, 2, scaling_factors, "*")
						
						# Verify it worked
						reconstructed_totals_after <- colSums(reconstructed)
						
						message("\nAFTER RESCALING:")
						message("  New totals - Range: [", round(min(reconstructed_totals_after), 1), 
										" to ", round(max(reconstructed_totals_after), 1), "]")
						
						# Check accuracy
						max_error <- max(abs(reconstructed_totals_after - original_totals))
						mean_error <- mean(abs(reconstructed_totals_after - original_totals))
						
						message("\nVERIFICATION:")
						message("  Max error: ", round(max_error, 6))
						message("  Mean error: ", round(mean_error, 6))
						
						if (max_error < 1e-6) {
							message("  ✓✓✓ RESCALING SUCCESSFUL! ✓✓✓")
						} else {
							message("  ⚠⚠⚠ RESCALING MIGHT HAVE ISSUES ⚠⚠⚠")
						}
						
						message("========================================\n")
						
						# Show notification
						showNotification(
							HTML(paste0(
								"<strong>Sample Rescaling Applied</strong><br>",
								"Scaling factors: ", round(min(scaling_factors), 3), " to ", round(max(scaling_factors), 3), "<br>",
								"Max error: ", formatC(max_error, format = "e", digits = 2)
							)),
							type = "message",
							duration = 5
						)
						
					} else if (input$rescale_samples && (input$center_data || input$scale_data)) {
						message("\n⚠ Sample rescaling DISABLED because data is centered or scaled")
						showNotification(
							"Sample rescaling disabled (data is centered/scaled)",
							type = "warning",
							duration = 3
						)
					}
					
					# Apply sample-level rescaling if requested
					# if (input$rescale_samples && !input$center_data && !input$scale_data) {
					# 	incProgress(0.6, detail = "Rescaling samples to match original totals")
					# 	
					# 	original_totals <- colSums(orig_data)
					# 	reconstructed_totals <- colSums(reconstructed)
					# 	
					# 	if (any(reconstructed_totals == 0)) {
					# 		message("Warning: Some reconstructed samples have zero total")
					# 		reconstructed_totals[reconstructed_totals == 0] <- 1
					# 	}
					# 	
					# 	scaling_factors <- original_totals / reconstructed_totals
					# 	reconstructed <- sweep(reconstructed, 2, scaling_factors, "*")
					# 	
					# 	message("Applied sample rescaling. Factors range: ", 
					# 					round(min(scaling_factors), 3), " to ", 
					# 					round(max(scaling_factors), 3))
					# } else if (input$rescale_samples && (input$center_data || input$scale_data)) {
					# 	message("Note: Sample rescaling disabled because data is centered/scaled")
					# }
					
					rv$reconstructed_data <- reconstructed
				}
				
				incProgress(1.0, detail = "Complete!")
			})
			
			# Build notification message
			transform_msg <- paste(
				if(length(rv$selected_pcs) == 0) "No PCs removed" else 
					paste0("PCs ", paste(rv$selected_pcs, collapse = ", "), " removed"),
				if(input$center_data) "• Centered" else "",
				if(input$scale_data) "• Scaled" else "",
				if(input$rescale_samples && !input$center_data && !input$scale_data) "• Sample-rescaled" else "",
				sep = " "
			)
			
			showNotification(
				paste0("✓ ", transform_msg),
				type = "message",
				duration = 5
			)
		})
		
		# Update sample group selector
		observe({
			req(eset_raw())
			metadata <- Biobase::pData(eset_raw())
			choices <- colnames(metadata)
			
			updateSelectInput(
				session,
				"sample_group",
				choices = c("None" = "none", choices),
				selected = if ("Sample_Group" %in% choices) "Sample_Group" else "none"
			)
		})
		
		# UI for selecting individual removed component to view
		output$removed_pc_selector <- renderUI({
			req(rv$removed_components)
			
			ns <- session$ns
			selectInput(
				ns("selected_removed_pc"),
				"Select component to visualize:",
				choices = names(rv$removed_components),
				selected = names(rv$removed_components)[1]
			)
		})
		
		# Value boxes
		output$box_original <- renderValueBox({
			req(pca_result())
			req(original_data())
			
			orig <- original_data()
			
			valueBox(
				value = "Original",
				subtitle = paste(nrow(orig), "features ×",
												 ncol(orig), "samples"),
				icon = icon("database"),
				color = "yellow"
			)
		})
		
		output$box_selected <- renderValueBox({
			req(pca_result())
			
			pca <- pca_result()
			n_pcs <- ncol(pca$x)
			selected_pcs <- sapply(1:n_pcs, function(i) {
				isTRUE(input[[paste0("pc_", i)]])
			})
			n_selected <- sum(selected_pcs)
			
			valueBox(
				value = n_selected,
				subtitle = paste("PCs selected for removal"),
				icon = icon("times-circle"),
				color = if (n_selected > 0) "red" else "green"
			)
		})
		
		output$box_variance <- renderValueBox({
			req(pca_result())
			
			pca <- pca_result()
			n_pcs <- ncol(pca$x)
			selected_pcs <- sapply(1:n_pcs, function(i) {
				isTRUE(input[[paste0("pc_", i)]])
			})
			
			if (sum(selected_pcs) > 0) {
				var_removed <- sum(pca$sdev[selected_pcs]^2) / sum(pca$sdev^2) * 100
			} else {
				var_removed <- 0
			}
			
			valueBox(
				value = sprintf("%.1f%%", var_removed),
				subtitle = "Variance removed",
				icon = icon("chart-line"),
				color = "blue"
			)
		})
		
		# Variance explained plot
		output$variance_plot <- renderPlot({
			req(pca_result())
			
			pca <- pca_result()
			var_explained <- (pca$sdev^2 / sum(pca$sdev^2)) * 100
			cumvar <- cumsum(var_explained)
			
			n_pcs <- length(var_explained)
			selected_pcs <- sapply(1:n_pcs, function(i) {
				isTRUE(input[[paste0("pc_", i)]])
			})
			
			df <- data.frame(
				PC = factor(1:n_pcs, levels = 1:n_pcs),
				Variance = var_explained,
				Cumulative = cumvar,
				Selected = selected_pcs
			)
			
			p1 <- ggplot(df, aes(x = PC, y = Variance, fill = Selected)) +
				geom_bar(stat = "identity") +
				scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
				labs(
					title = "Variance Explained by Each PC",
					y = "% Variance Explained",
					x = "Principal Component"
				) +
				theme_minimal(base_size = 14) +
				theme(legend.position = "top")
			
			p2 <- ggplot(df, aes(x = PC, y = Cumulative, group = 1)) +
				geom_line(linewidth = 1, color = "darkgreen") +
				geom_point(size = 3, color = "darkgreen") +
				labs(
					title = "Cumulative Variance Explained",
					y = "Cumulative % Variance",
					x = "Principal Component"
				) +
				theme_minimal(base_size = 14)
			
			gridExtra::grid.arrange(p1, p2, ncol = 2)
		})
		
		# Density plots - Original
		output$density_original <- renderPlot({
			req(pca_result())
			req(original_data_processed())
			
			title_suffix <- if(input$show_as_rfu) " (RFU)" else if(input$shift_to_positive) " (shifted)" else ""
			
			plot_density_data(
				original_data_processed(),
				eset_raw(),
				input$sample_group,
				input$density_type,
				paste0("Original Data", title_suffix)
			)
		})
		
		# Heatmap - Original
		output$heatmap_original <- renderPlot({
			req(pca_result())
			req(original_data_processed())
			
			title_suffix <- if(input$show_as_rfu) " (RFU)" else if(input$shift_to_positive) " (shifted)" else ""
			
			plot_heatmap_data(
				original_data_processed(),
				eset_raw(),
				group_var = input$sample_group,
				scale_type = input$heatmap_scale,
				show_features = input$show_features,
				cluster_rows = input$cluster_rows,
				cluster_cols = input$cluster_cols,
				title = paste0("Original Data", title_suffix)
			)
		})
		
		# Sample statistics - Original
		output$stats_original <- renderPlot({
			req(pca_result())
			req(original_data_processed())
			
			title_suffix <- if(input$show_as_rfu) " (RFU)" else if(input$shift_to_positive) " (shifted)" else ""
			
			plot_sample_statistics(
				original_data_processed(),
				eset_raw(),
				input$sample_group,
				paste0("Original Data", title_suffix)
			)
		})
		
		# Difference plot
		output$difference_plot <- renderPlot({
			req(reconstructed_data_processed())
			req(original_data_processed())
			
			diff_data <- reconstructed_data_processed() - original_data_processed()
			
			plot_difference_data(
				diff_data,
				eset_raw(),
				input$sample_group
			)
		})
		
		# Density plots - Reconstructed
		output$density_reconstructed <- renderPlot({
			req(reconstructed_data_processed())
			
			title_suffix <- if(input$show_as_rfu) " (RFU)" else if(input$shift_to_positive) " (shifted)" else ""
			
			plot_density_data(
				reconstructed_data_processed(),
				eset_raw(),
				group_var = input$sample_group,
				plot_type = input$density_type,
				title = paste0("Reconstructed (PCs ", paste(rv$selected_pcs, collapse = ", "), " removed)", title_suffix)
			)
		})
		
		# Heatmap - Reconstructed
		output$heatmap_reconstructed <- renderPlot({
			req(reconstructed_data_processed())
			
			title_suffix <- if(input$show_as_rfu) " (RFU)" else if(input$shift_to_positive) " (shifted)" else ""
			
			plot_heatmap_data(
				reconstructed_data_processed(),
				eset_raw(),
				input$sample_group,
				input$heatmap_scale,
				input$show_features,
				cluster_rows = input$cluster_rows,
				cluster_cols = input$cluster_cols,
				paste0("Reconstructed (PCs ", paste(rv$selected_pcs, collapse = ", "), " removed)", title_suffix)
			)
		})
		
		# Sample statistics - Reconstructed
		output$stats_reconstructed <- renderPlot({
			req(reconstructed_data_processed())
			
			title_suffix <- if(input$show_as_rfu) " (RFU)" else if(input$shift_to_positive) " (shifted)" else ""
			
			plot_sample_statistics(
				reconstructed_data_processed(),
				eset_raw(),
				input$sample_group,
				paste0("Reconstructed (PCs ", paste(rv$selected_pcs, collapse = ", "), " removed)", title_suffix)
			)
		})
		
		# Removed components - Faceted density plot
		output$removed_density_facet <- renderPlot({
			req(removed_components_processed())
			
			plot_removed_density_facet(
				removed_components_processed(),
				eset_raw(),
				input$sample_group
			)
		})
		
		# Removed components - Composite heatmap
		output$removed_heatmap_composite <- renderPlot({
			req(removed_composite_processed())
			
			title_suffix <- if(input$show_as_rfu) " (RFU)" else if(input$shift_to_positive) " (shifted)" else ""
			
			plot_heatmap_data(
				removed_composite_processed(),
				eset_raw(),
				group_var = input$sample_group,
				scale_type = input$heatmap_scale,
				show_features = input$show_features,
				cluster_rows = input$cluster_rows,
				cluster_cols = input$cluster_cols,
				title = paste0("Composite of Removed Components (PCs ", paste(rv$selected_pcs, collapse = ", "), ")", title_suffix)
			)
		})
		
		# Individual removed component heatmap
		output$removed_heatmap_individual <- renderPlot({
			req(removed_components_processed())
			req(input$selected_removed_pc)
			
			selected_comp <- removed_components_processed()[[input$selected_removed_pc]]
			
			title_suffix <- if(input$show_as_rfu) " (RFU)" else if(input$shift_to_positive) " (shifted)" else ""
			
			plot_heatmap_data(
				selected_comp,
				eset_raw(),
				group_var = input$sample_group,
				scale_type = input$heatmap_scale,
				show_features = input$show_features,
				cluster_rows = input$cluster_rows,
				cluster_cols = input$cluster_cols,
				title = paste0("Removed Component: ", input$selected_removed_pc, title_suffix)
			)
		})
		
		# PC Loadings heatmap
		output$loadings_heatmap <- renderPlot({
			req(pca_result())
			
			pca <- pca_result()
			plot_loadings_heatmap(
				pca$rotation,
				pca$sdev,
				input$top_features
			)
		})
		
		# Return reconstructed data for potential use elsewhere
		return(reactive({
			list(
				original = original_data(),
				reconstructed = rv$reconstructed_data,
				pcs_removed = rv$selected_pcs,
				removed_components = rv$removed_components,
				removed_composite = rv$removed_composite,
				# Post-processed versions
				original_processed = original_data_processed(),
				reconstructed_processed = reconstructed_data_processed(),
				removed_composite_processed = removed_composite_processed()
			)
		}))
	})
}

# ===================================================================
# HELPER FUNCTIONS FOR VISUALIZATION
# ===================================================================

#' Plot density data
#' @keywords internal
plot_density_data <- function(expr_data, eset, group_var = 'Labels', plot_type = "overlay", title = 'test') {
	require(ggplot2)
	require(tidyr)
	require(dplyr)
	
	# Ensure expr_data is a matrix
	if (!is.matrix(expr_data)) {
		expr_data <- as.matrix(expr_data)
	}
	
	# Check dimensions and transpose if needed
	if (is.null(colnames(expr_data))) {
		stop("expr_data must have column names (sample names)")
	}
	
	# Transpose so samples are rows
	df <- as.data.frame(t(expr_data))
	df$Sample <- rownames(df)
	
	# Add grouping variable
	if (group_var != "none") {
		metadata <- Biobase::pData(eset)
		if (group_var %in% colnames(metadata)) {
			df$Group <- metadata[[group_var]][match(df$Sample, rownames(metadata))]
		} else {
			df$Group <- "All Samples"
		}
	} else {
		df$Group <- "All Samples"
	}
	
	# Reshape to long format
	df_long <- df %>%
		pivot_longer(cols = -c(Sample, Group), names_to = "Feature", values_to = "Expression")
	
	if (plot_type == "overlay") {
		ggplot(df_long, aes(x = Expression, color = Group, group = Sample)) +
			geom_density(alpha = 0.3, linewidth = 0.5) +
			scale_color_brewer(palette = "Set1") +
			labs(title = title, x = "Expression Value", y = "Density") +
			theme_minimal(base_size = 14) +
			theme(legend.position = "top")
		
	} else if (plot_type == "facet") {
		ggplot(df_long, aes(x = Expression, group = Sample, color = Group)) +
			geom_density(alpha = 0.5, linewidth = 0.7) +
			facet_wrap(~Group, ncol = 2) +
			scale_color_brewer(palette = "Set1") +
			labs(title = title, x = "Expression Value", y = "Density") +
			theme_minimal(base_size = 14) +
			theme(legend.position = "none")
		
	} else if (plot_type == "ridge") {
		require(ggridges)
		
		# Sample a subset if too many samples
		if (length(unique(df_long$Sample)) > 50) {
			sampled_samples <- sample(unique(df_long$Sample), 50)
			df_long <- df_long %>% filter(Sample %in% sampled_samples)
			title <- paste(title, "(50 samples shown)")
		}
		
		ggplot(df_long, aes(x = Expression, y = Sample, fill = Group)) +
			ggridges::geom_density_ridges(alpha = 0.7, scale = 2) +
			scale_fill_brewer(palette = "Set1") +
			labs(title = title, x = "Expression Value", y = "Sample") +
			theme_minimal(base_size = 14) +
			theme(axis.text.y = element_blank())
	}
}

#' Plot removed components density - faceted by component
#' @keywords internal
plot_removed_density_facet <- function(removed_components_list, eset, group_var = 'Labels') {
	require(ggplot2)
	require(tidyr)
	require(dplyr)
	
	# Combine all components into one long data frame
	df_list <- lapply(names(removed_components_list), function(pc_name) {
		comp_data <- removed_components_list[[pc_name]]
		
		# Transpose so samples are rows
		df <- as.data.frame(t(comp_data))
		df$Sample <- rownames(df)
		df$Component <- pc_name
		
		# Add grouping variable
		if (group_var != "none") {
			metadata <- Biobase::pData(eset)
			if (group_var %in% colnames(metadata)) {
				df$Group <- metadata[[group_var]][match(df$Sample, rownames(metadata))]
			} else {
				df$Group <- "All Samples"
			}
		} else {
			df$Group <- "All Samples"
		}
		
		# Reshape to long format
		df_long <- df %>%
			pivot_longer(
				cols = -c(Sample, Component, Group), 
				names_to = "Feature", 
				values_to = "Expression"
			)
		
		return(df_long)
	})
	
	# Combine all components
	df_combined <- bind_rows(df_list)
	
	# Create faceted density plot
	ggplot(df_combined, aes(x = Expression, color = Group, group = Sample)) +
		geom_density(alpha = 0.3, linewidth = 0.5) +
		facet_wrap(~Component, scales = "free", ncol = 3) +
		scale_color_brewer(palette = "Set1") +
		labs(
			title = "Distribution of Removed Components by PC",
			x = "Expression Value",
			y = "Density"
		) +
		theme_minimal(base_size = 12) +
		theme(
			legend.position = "top",
			strip.background = element_rect(fill = "lightcoral", color = "darkred"),
			strip.text = element_text(face = "bold", size = 11)
		)
}

#' Plot heatmap data
#' @keywords internal
plot_heatmap_data <- function(expr_data, eset, 
															group_var = 'Labels', 
															scale_type = 'none', 
															show_features = TRUE,
															cluster_rows = FALSE,
															cluster_cols = FALSE,
															title = 'test') {
	require(pheatmap)
	
	# Prepare annotation
	if (group_var != "none") {
		metadata <- Biobase::pData(eset)
		annotation_col <- data.frame(
			Group = metadata[[group_var]],
			row.names = colnames(expr_data)
		)
	} else {
		annotation_col <- NA
	}
	
	# Show all features - no limiting
	
	pheatmap(
		expr_data,
		annotation_col = annotation_col,
		scale = scale_type,
		show_rownames = show_features,
		show_colnames = FALSE,
		cluster_rows = cluster_rows,
		cluster_cols = cluster_cols,
		main = title,
		fontsize = 10,
		fontsize_row = 6
	)
}

#' Plot sample statistics
#' @keywords internal
plot_sample_statistics <- function(expr_data, eset, group_var, title) {
	require(ggplot2)
	require(dplyr)
	require(gridExtra)
	
	# Calculate statistics per sample
	stats_df <- data.frame(
		Sample = colnames(expr_data),
		Mean = colMeans(expr_data),
		Median = apply(expr_data, 2, median),
		SD = apply(expr_data, 2, sd),
		IQR = apply(expr_data, 2, IQR)
	)
	
	# Add grouping
	if (group_var != "none") {
		metadata <- Biobase::pData(eset)
		stats_df$Group <- metadata[[group_var]][match(stats_df$Sample, rownames(metadata))]
	} else {
		stats_df$Group <- "All Samples"
	}
	
	p1 <- ggplot(stats_df, aes(x = Group, y = Mean, fill = Group)) +
		geom_boxplot() +
		geom_jitter(width = 0.2, alpha = 0.5) +
		labs(title = "Sample Means", y = "Mean Expression") +
		theme_minimal(base_size = 12) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
	
	p2 <- ggplot(stats_df, aes(x = Group, y = SD, fill = Group)) +
		geom_boxplot() +
		geom_jitter(width = 0.2, alpha = 0.5) +
		labs(title = "Sample Standard Deviations", y = "SD") +
		theme_minimal(base_size = 12) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
	
	p3 <- ggplot(stats_df, aes(x = Mean, y = SD, color = Group)) +
		geom_point(size = 3, alpha = 0.7) +
		labs(title = "Mean-SD Relationship", x = "Mean Expression", y = "SD") +
		theme_minimal(base_size = 12)
	
	p4 <- ggplot(stats_df, aes(x = Median, y = IQR, color = Group)) +
		geom_point(size = 3, alpha = 0.7) +
		labs(title = "Median-IQR Relationship", x = "Median Expression", y = "IQR") +
		theme_minimal(base_size = 12)
	
	gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, top = title)
}

#' Plot difference data
#' @keywords internal
plot_difference_data <- function(diff_data, eset, group_var) {
	require(ggplot2)
	require(tidyr)
	require(dplyr)
	require(gridExtra)
	
	# Overall distribution of differences
	df_long <- as.data.frame(diff_data) %>%
		pivot_longer(cols = everything(), names_to = "Sample", values_to = "Difference")
	
	p1 <- ggplot(df_long, aes(x = Difference)) +
		geom_histogram(bins = 50, fill = "darkred", alpha = 0.7) +
		labs(
			title = "Distribution of Differences (Reconstructed - Original)",
			x = "Difference in Expression",
			y = "Count"
		) +
		theme_minimal(base_size = 14)
	
	# Per-sample differences
	sample_diffs <- data.frame(
		Sample = colnames(diff_data),
		MeanDiff = colMeans(diff_data),
		AbsMeanDiff = colMeans(abs(diff_data)),
		MaxAbsDiff = apply(abs(diff_data), 2, max)
	)
	
	if (group_var != "none") {
		metadata <- Biobase::pData(eset)
		sample_diffs$Group <- metadata[[group_var]][match(sample_diffs$Sample, rownames(metadata))]
	} else {
		sample_diffs$Group <- "All Samples"
	}
	
	p2 <- ggplot(sample_diffs, aes(x = Group, y = AbsMeanDiff, fill = Group)) +
		geom_boxplot() +
		geom_jitter(width = 0.2, alpha = 0.5) +
		labs(
			title = "Mean Absolute Difference per Sample",
			y = "Mean Absolute Difference"
		) +
		theme_minimal(base_size = 14) +
		theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
	
	gridExtra::grid.arrange(p1, p2, ncol = 1)
}

#' Plot loadings heatmap
#' @keywords internal
plot_loadings_heatmap <- function(loadings, sdev, top_n) {
	require(pheatmap)
	require(dplyr)
	
	# Calculate contribution of each feature to each PC
	contributions <- abs(loadings) * rep(sdev, each = nrow(loadings))
	
	# Get top contributing features
	total_contribution <- rowSums(contributions)
	top_features <- names(sort(total_contribution, decreasing = TRUE)[1:min(top_n, length(total_contribution))])
	
	# Plot heatmap of loadings for top features
	pheatmap(
		loadings[top_features, ],
		scale = "none",
		cluster_rows = TRUE,
		cluster_cols = FALSE,
		main = paste("PC Loadings for Top", length(top_features), "Contributing Features"),
		fontsize_row = 8,
		color = colorRampPalette(c("blue", "white", "red"))(100)
	)
}