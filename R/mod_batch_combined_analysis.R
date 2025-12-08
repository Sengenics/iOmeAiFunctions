#' Combined Batch Analysis Module - UI
#'
#' Combined ANOVA batch effects and distribution confounding tests
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_batch_combined_analysis_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		
		fluidRow(
			box(
				title = "Combined Batch Effect & Confounding Analysis",
				width = 12,
				status = "warning",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = FALSE,
				
				selectInput(
					ns("test_method"),
					"Statistical Test Method:",
					choices = c(
						"PERMANOVA (adonis2) - Tests group differences" = "permanova",
						"Beta Dispersion (betadisper) - Tests variance differences" = "betadisper"
					),
					selected = "permanova"
					#selected = "betadisper"
				),
				
				fluidRow(
					box(
						title = "Combined Visualization",
						width = 12,
						status = "primary",
						solidHeader = TRUE,
						collapsible = TRUE,
						collapsed = FALSE,
						
			
						shinycssloaders::withSpinner(
							plotOutput(ns("combined_plot"), height = "700px"),
							type = 4,  # Spinner style (1-8 available)
							color = "darkblue"  # Customize color
						),
						# ),
						# 
						fluidRow(
							box(
								title = "Interpretation Guide",
								width = 12,
								status = "info",
								solidHeader = TRUE,
								collapsible = TRUE,
								collapsed = TRUE,
								
								p("Each batch column is plotted showing:"),
								tags$ul(
									tags$li(strong("Y-axis (Batch Effect):"), "-log10(ANOVA p-value) - higher = stronger batch effect"),
									tags$li(strong("X-axis (Confounding):"), "-log10(Fisher p-value) - higher = more confounded with sample groups"),
									tags$li(strong("Color:"), "Significance level"),
									tags$li(strong("Size:"), "Number of groups in batch column")
								),
								
								h4("Quadrant Interpretation:"),
								tags$ul(
									tags$li(strong("Top-Right (High Batch Effect + High Confounding):"), 
													"⚠️ CRITICAL - Strong batch effect AND confounded with sample groups Correction may remove real biology! "),
									tags$li(strong("Top-Left (High Batch Effect + Low Confounding):"), 
													"✅ SAFE TO CORRECT - Strong batch effect but evenly distributedCorrection recommended."),
									tags$li(strong("Bottom-Right (Low Batch Effect + High Confounding):"), 
													"⚠️ CAUTION - Uneven distribution but weak effectMay indicate biological correlation."),
									tags$li(strong("Bottom-Left (Low Batch Effect + Low Confounding):"), 
													"✅ NO ACTION NEEDED - No significant batch effect or confounding.")
								))
						))
				),
				
				
				fluidRow(
			
					
				
					box(
						title = "Combined Results Table",
						width = 12,
						#status = "warning",
						#solidHeader = TRUE,
						collapsible = TRUE,
						collapsed = TRUE,
						
						helpText(
							strong("PERMANOVA (recommended):"), "Tests if groups have different expression profiles (location test) ",
							"This is what most batch correction studies use.",
							br(),
							strong("Beta Dispersion:"), "Tests if groups have different variances (dispersion test)",
							"Useful for detecting heteroscedasticity."
						),
						
						p(strong("This analysis answers two questions:")),
						tags$ul(
							tags$li(strong("Batch Effect:"), "Does this column affect protein expression?  (ANOVA)"),
							tags$li(strong("Confounding:"), "Is this column unevenly distributed across sample groups?  (Fisher's test)")
						),
						
						uiOutput(ns("debug_ui")),
					
						
						
						uiOutput(ns("analysis_status")),
						
						hr(),
						
						#h4("Combined Results Table"),
						DTOutput(ns("combined_table")),
					)
				)
		# 	)
		# ),
		# 
		)
	)
	)
}

#' Combined Batch Analysis Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param sample_group_column Reactive character Sample grouping column name
#' @param batch_columns Reactive character vector Batch columns to test
#' @param debug Enable debug mode
#' @export
mod_batch_combined_analysis_server <- function(id,
																							 eset,
																							 sample_group_column,
																							 batch_columns,
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
				message("🔍 DEBUG MODE - Combined Batch Analysis Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • sample_group_column() - Sample grouping column")
				message("  • batch_columns() - Batch columns")
				message("  • combined_results() - Combined analysis results")
				message("\nUseful commands:")
				message("  str(combined_results())")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		
		# Run combined analysis ####
		combined_results <- reactive({
			req(eset())
			req(sample_group_column())
			req(batch_columns())
			
			ExpSet <- eset()
			m <- Biobase::exprs(ExpSet)
			meta <- Biobase::pData(ExpSet)
			sample_group <- sample_group_column()
			# if('ComBat' %in% colnames(meta)){
			# 	batch_cols <- c(batch_columns(),'ComBat')
			# }else{
				batch_cols <- batch_columns()
			#}
			test_method <- input$test_method  # Get selected method
			
			if (length(batch_cols) == 0) {
				return(NULL)
			}
			
			# Validate columns
			if (!sample_group %in% colnames(meta)) {
				warning("Sample group column not found")
				return(NULL)
			}
			
			batch_cols <- intersect(batch_cols, colnames(meta))
			if (length(batch_cols) == 0) {
				return(NULL)
			}
			
			# Run analyses based on selected method
			results_list <- list()
			
			for (batch_col in batch_cols) {
				result <- tryCatch({
					# 1 Batch effect test (method-dependent)
					if (test_method == "permanova") {
						# Use PERMANOVA
						perm_result <- permanova_function(m, meta, batch_col)
						result_df <- as.data.frame(perm_result)
						batch_p <- result_df$`Pr(>F)`[1]
					} else {
						# Use betadisper + ANOVA
						anova_result <- anova_betadine_function(m, meta, batch_col)
						anova_tidy <- broom::tidy(anova_result)
						batch_p <- anova_tidy$p.value[1]
					}
					
					# 2Fisher's test for confounding (same for both methods)
					tab <- table(meta[[sample_group]], meta[[batch_col]])
					fisher_result <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
					fisher_p <- fisher_result$p.value
					
					# 3 Additional info
					n_batch_groups <- length(unique(meta[[batch_col]]))
					n_sample_groups <- length(unique(meta[[sample_group]]))
					
					list(
						batch_column = batch_col,
						batch_p = batch_p,
						fisher_p = fisher_p,
						n_batch_groups = n_batch_groups,
						n_sample_groups = n_sample_groups,
						interpretation = interpret_combined(batch_p, fisher_p)
					)
				}, error = function(e) {
					warning(sprintf("Analysis failed for '%s': %s", batch_col, e$message))
					NULL
				})
				
				if (!is.null(result)) {
					results_list[[batch_col]] <- result
				}
			}
			
			if (length(results_list) == 0) {
				return(NULL)
			}
			
			# Convert to data frame
			method_label <- ifelse(test_method == "permanova", "PERMANOVA_p_value", "ANOVA_p_value")
			
			df <- data.frame(
				Batch_Column = sapply(results_list, function(x) x$batch_column),
				Batch_p_value = signif(sapply(results_list, function(x) x$batch_p), 3),
				Fisher_p_value = signif(sapply(results_list, function(x) x$fisher_p), 3),
				Batch_Groups = sapply(results_list, function(x) x$n_batch_groups),
				Interpretation = sapply(results_list, function(x) x$interpretation),
				stringsAsFactors = FALSE
			)
			
			# Rename column based on method
			colnames(df)[2] <- method_label
			
			df
		})
		
		# Interpretation helper function ####
		interpret_combined <- function(anova_p, fisher_p) {
			batch_sig <- anova_p < 0.05
			confound_sig <- fisher_p < 0.05
			
			if (batch_sig && confound_sig) {
				"⚠️ CRITICAL: Batch effect + Confounding"
			} else if (batch_sig && !confound_sig) {
				"✅ SAFE: Batch effect only"
			} else if (!batch_sig && confound_sig) {
				"⚠️ CAUTION: Confounding only"
			} else {
				"✅ OK: No issues"
			}
		}
		
		# Status message ####
		output$analysis_status <- renderUI({
			req(sample_group_column())
			req(batch_columns())
			
			batch_cols <- batch_columns()
			
			if (length(batch_cols) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No batch columns selected")
					)
				)
			}
			
			if (is.null(combined_results())) {
				return(
					div(
						class = "alert alert-danger",
						icon("times-circle"),
						strong(" Analysis failed")
					)
				)
			}
			
			div(
				class = "alert alert-success",
				icon("check-circle"),
				strong(sprintf(" Analysis complete: '%s' vs %d batch columns", 
											 sample_group_column(), length(batch_cols)))
			)
		})
		
		# Combined table ####
		# Combined table
		output$combined_table <- renderDT({
			req(combined_results())
			
			df <- combined_results()
			
			# Get the batch effect column name dynamically
			batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
			
			datatable(
				df,
				options = list(
					pageLength = 20,
					scrollX = TRUE,
					dom = 'Bfrtip',
					buttons = c('copy', 'csv', 'excel')
				),
				rownames = FALSE,
				caption = "Combined batch effect and confounding analysis"
			) %>%
				formatStyle(
					batch_col_name,  # Use dynamic column name
					backgroundColor = styleInterval(
						c(0.001, 0.01, 0.05),
						c('#ffcccc', '#ffddcc', '#ffffcc', '#ffffff')
					)
				) %>%
				formatStyle(
					'Fisher_p_value',
					backgroundColor = styleInterval(
						c(0.001, 0.01, 0.05),
						c('#ffcccc', '#ffddcc', '#ffffcc', '#ffffff')
					)
				)
		})
		
		
		# Combined visualization ####
		output$combined_plot <- renderPlot({
			req(combined_results())
			
			df <- combined_results()
			
			if (nrow(df) == 0) {
				return(NULL)
			}
			
			# Dynamically get the batch effect column name
			batch_col_name <- grep("_p_value$", colnames(df), value = TRUE)[1]
			method_type <- input$test_method
			
			# Prepare plot data
			df_plot <- df %>%
				rename(batch_p = !!sym(batch_col_name)) %>%  # Rename to generic name
				mutate(
					neg_log_batch = -log10(batch_p),
					neg_log_fisher = -log10(Fisher_p_value),
					batch_sig = batch_p < 0.05,
					confound_sig = Fisher_p_value < 0.05,
					category = case_when(
						batch_sig & confound_sig ~ "Critical: Both significant",
						batch_sig & !confound_sig ~ "Safe: Batch effect only",
						!batch_sig & confound_sig ~ "Caution: Confounding only",
						TRUE ~ "OK: Neither significant"
					)
				)
			
			# Calculate dynamic positions for annotations
			ymax <- max(c(max(df_plot$neg_log_batch), -log10(0.05) * 2))
			xmax <- max(c(max(df_plot$neg_log_fisher), -log10(0.05) * 2))
			a <- -log10(0.05)
			low <- a / 2
			high <- a + low
			xhigh <- ((xmax - a) / 2) + a
			yhigh <- ((ymax - a) / 2) + a
			
			# Create scatter plot
			ggplot(df_plot, aes(x = neg_log_fisher, y = neg_log_batch)) +
				# Quadrant lines
				geom_vline(xintercept = -log10(0.05), linetype = "dashed", color = "gray50", size = 0.8) +
				geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "gray50", size = 0.8) +
				
				# Quadrant labels with dynamic positioning
				annotate("text", x = low, y = yhigh, 
								 label = "Low Confounding\nHigh Batch Effect\n✅ SAFE TO CORRECT", 
								 hjust = 0, vjust = 1, color = "darkgreen", size = 3.5, fontface = "bold") +
				annotate("text", x = xhigh, y = yhigh, 
								 label = "High Confounding\nHigh Batch Effect\n⚠️ CRITICAL", 
								 hjust = 1, vjust = 1, color = "darkred", size = 3.5, fontface = "bold") +
				annotate("text", x = low, y = low, 
								 label = "✅ NO ACTION\nNEEDED", 
								 hjust = 0, vjust = 0, color = "gray50", size = 3.5) +
				annotate("text", x = xhigh, y = low, 
								 label = "⚠️ CAUTION\nPossible biological\ncorrelation", 
								 hjust = 1, vjust = 0, color = "darkorange", size = 3.5) +
				
				# Points
				geom_point(aes(color = category, size = Batch_Groups), alpha = 0.7) +
				
				# Labels
				ggrepel::geom_text_repel(
					aes(label = Batch_Column),
					size = 3.5,
					max.overlaps = 20,
					box.padding = 0.5
				) +
				
				# Colors
				scale_color_manual(
					values = c(
						"Critical: Both significant" = "#d32f2f",
						"Safe: Batch effect only" = "#388e3c",
						"Caution: Confounding only" = "#f57c00",
						"OK: Neither significant" = "#757575"
					)
				) +
				
				# Size
				scale_size_continuous(range = c(3, 10)) +
				
				# Labels
				labs(
					title = sprintf("Batch Effect (%s) vs Confounding: '%s'", 
													ifelse(method_type == "permanova", "PERMANOVA", "Beta Dispersion"),
													sample_group_column()),
					subtitle = "Dashed lines indicate p = 0.05 threshold",
					x = "Confounding with Sample Groups\n-log10(Fisher's p-value) →",
					y = sprintf("← Batch Effect on Expression\n-log10(%s p-value)", 
											ifelse(method_type == "permanova", "PERMANOVA", "ANOVA")),
					color = "Category",
					size = "Number of\nBatch Groups"
				) +
				
				theme_minimal(base_size = 14) +
				theme(
					legend.position = "bottom",
					legend.box = "vertical",
					panel.grid.minor = element_blank(),
					plot.title = element_text(face = "bold"),
					axis.title = element_text(face = "bold")
				) + 
				coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax))
		})
		
		# })
		
		# Return results
		return(list(
			results = combined_results
		))
	})
}
