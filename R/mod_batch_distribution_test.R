#' Batch Distribution Testing Module - UI
#'
#' Tests if sample groups are evenly distributed across batch columns
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_batch_distribution_test_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Sample Group Distribution Across Batch Columns",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = TRUE,
				
				p("Tests whether the selected sample grouping column is evenly distributed across batch columns."),
				p("Uses Fisher's exact test to detect uneven distribution (potential confounding). "),
				
				uiOutput(ns("debug_ui")),
				
				uiOutput(ns("test_status")),
				
				hr(),
				
				h4("Distribution Test Results"),
				DTOutput(ns("distribution_table")),
				# 	)
				# ),
				
				fluidRow(
					box(
						title = "Visualization",
						width = 12,
						status = "primary",
						solidHeader = TRUE,
						collapsible = TRUE,
						
						plotOutput(ns("distribution_plot"), height = "600px")
					)
				)
			)
		)
	)
}

#' Batch Distribution Testing Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param sample_group_column Reactive character.   Sample grouping column name
#' @param batch_columns Reactive character vector.  Batch column names to test
#' @param debug Enable debug mode
#' @export
mod_batch_distribution_test_server <- function(id, 
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
				message("🔍 DEBUG MODE - Batch Distribution Test Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • sample_group_column() - Sample grouping column")
				message("  • batch_columns() - Batch columns to test")
				message("  • distribution_results() - Test results")
				message("\nUseful commands:")
				message("  sample_group_column()")
				message("  batch_columns()")
				message("  str(distribution_results())")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Run distribution tests
		distribution_results <- reactive({
			req(eset())
			req(sample_group_column())
			req(batch_columns())
			
			meta <- Biobase::pData(eset())
			sample_group <- sample_group_column()
			batch_cols <- batch_columns()
			
			# Need at least one batch column
			if (length(batch_cols) == 0) {
				return(NULL)
			}
			
			# Validate columns exist
			if (! sample_group %in% colnames(meta)) {
				warning("Sample group column not found in metadata")
				return(NULL)
			}
			
			missing_cols <- setdiff(batch_cols, colnames(meta))
			if (length(missing_cols) > 0) {
				warning("Batch columns not found: ", paste(missing_cols, collapse = ", "))
				batch_cols <- intersect(batch_cols, colnames(meta))
			}
			
			if (length(batch_cols) == 0) {
				return(NULL)
			}
			
			# Test each batch column
			results_list <- list()
			
			for (batch_col in batch_cols) {
				result <- tryCatch({
					# Create contingency table
					tab <- table(meta[[sample_group]], meta[[batch_col]])
					
					# Calculate percentage distribution
					pct_dist <- apply(tab, 1, function(x) x/sum(x)*100)
					
					# Fisher's exact test with simulation for large tables
					fisher_result <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
					
					# Chi-square test (if possible)
					chisq_result <- tryCatch({
						chisq.test(tab, simulate.p.value = TRUE, B = 10000)
					}, error = function(e) NULL)
					
					list(
						batch_column = batch_col,
						n_sample_groups = nrow(tab),
						n_batch_groups = ncol(tab),
						fisher_p = fisher_result$p.value,
						chisq_p = if (! is.null(chisq_result)) chisq_result$p.value else NA,
						contingency_table = tab,
						percent_distribution = pct_dist
					)
				}, error = function(e) {
					warning(sprintf("Distribution test failed for '%s': %s", batch_col, e$message))
					NULL
				})
				
				if (!is.null(result)) {
					results_list[[batch_col]] <- result
				}
			}
			
			if (length(results_list) == 0) {
				return(NULL)
			}
			
			# Convert to data frame for table display
			df <- data.frame(
				Batch_Column = sapply(results_list, function(x) x$batch_column),
				Sample_Groups = sapply(results_list, function(x) x$n_sample_groups),
				Batch_Groups = sapply(results_list, function(x) x$n_batch_groups),
				Fisher_p_value = signif(sapply(results_list, function(x) x$fisher_p), 3),
				ChiSq_p_value = signif(sapply(results_list, function(x) x$chisq_p), 3),
				stringsAsFactors = FALSE
			)
			
			list(
				df = df,
				details = results_list
			)
		})
		
		# Status message
		output$test_status <- renderUI({
			req(sample_group_column())
			req(batch_columns())
			
			batch_cols <- batch_columns()
			
			if (length(batch_cols) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No batch columns selected"),
						p("Please select batch columns in the Batch Column Selection tab.")
					)
				)
			}
			
			if (is.null(distribution_results())) {
				return(
					div(
						class = "alert alert-danger",
						icon("times-circle"),
						strong(" Testing failed"),
						p("Distribution test encountered an error.")
					)
				)
			}
			
			div(
				class = "alert alert-success",
				icon("check-circle"),
				strong(sprintf(" Testing complete: Sample group '%s' tested against %d batch columns", 
											 sample_group_column(), length(batch_cols)))
			)
		})
		
		# Results table
		output$distribution_table <- renderDT({
			req(distribution_results())
			
			df <- distribution_results()$df
			
			datatable(
				df,
				options = list(
					pageLength = 20,
					scrollX = TRUE,
					dom = 'Bfrtip',
					buttons = c('copy', 'csv', 'excel')
				),
				rownames = FALSE,
				caption = "Distribution test results (lower p-values indicate uneven distribution)"
			) %>%
				formatStyle(
					'Fisher_p_value',
					backgroundColor = styleInterval(
						c(0.001, 0.01, 0.05),
						c('#ffcccc', '#ffddcc', '#ffffcc', '#ffffff')
					)
				)
		})
		
		# Visualization
		output$distribution_plot <- renderPlot({
			req(distribution_results())
			
			df <- distribution_results()$df
			
			if (nrow(df) == 0) {
				return(NULL)
			}
			
			# Create -log10(p-value) plot similar to batch testing
			df_plot <- df %>%
				mutate(
					neg_log_p_fisher = -log10(Fisher_p_value),
					neg_log_p_chisq = -log10(ChiSq_p_value),
					significance = case_when(
						Fisher_p_value < 0.001 ~ "p < 0.001",
						Fisher_p_value < 0.01 ~ "p < 0.01",
						Fisher_p_value < 0.05 ~ "p < 0. 05",
						TRUE ~ "n.s."
					)
				) %>%
				arrange(desc(neg_log_p_fisher))
			
			ggplot(df_plot, aes(x = reorder(Batch_Column, neg_log_p_fisher), 
													y = neg_log_p_fisher, 
													fill = significance)) +
				geom_col() +
				geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
				geom_hline(yintercept = -log10(0.01), linetype = "dashed", color = "darkred") +
				geom_hline(yintercept = -log10(0.001), linetype = "dashed", color = "darkred", size = 1) +
				scale_fill_manual(
					values = c(
						"p < 0. 001" = "#d32f2f",
						"p < 0.01" = "#f57c00",
						"p < 0.05" = "#fbc02d",
						"n.s." = "#757575"
					)
				) +
				coord_flip() +
				labs(
					title = sprintf("Distribution of '%s' Across Batch Columns", sample_group_column()),
					subtitle = "Higher values = more uneven distribution (potential confounding)",
					x = "Batch Column",
					y = "-log10(Fisher's p-value)",
					fill = "Significance",
					caption = "Red dashed lines: p = 0.05, 0.01, 0.001"
				) +
				theme_minimal(base_size = 14) +
				theme(
					legend.position = "bottom",
					panel.grid.major.y = element_blank()
				)
		})
		
		# Return results
		return(list(
			results = distribution_results
		))
	})
}