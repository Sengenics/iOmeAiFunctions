#' Batch Testing Module - UI
#'
#' Run ANOVA tests on selected batch columns
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_batch_testing_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Batch Effect Testing (ANOVA)",
				width = 12,
				status = "warning",
				solidHeader = TRUE,
				
				p("Tests for batch effects using ANOVA on selected phenoData columns."),
				p("Results show which columns have significant effects on protein expression."),
				
				# if (debug) {
				# 	actionButton(
				# 		ns("debug"),
				# 		"Debug",
				# 		icon = icon("bug"),
				# 		class = "btn-warning btn-sm"
				# 	)
				# 	hr()
				# },
				
				uiOutput(ns("debug_ui")),
				
				uiOutput(ns("testing_status")),
				
				hr(),
				
				h4("ANOVA Results"),
				DTOutput(ns("anova_table"))
			)
		),
		
		fluidRow(
			box(
				title = "Visualization",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				collapsible = TRUE,
				
				plotOutput(ns("anova_plot"), height = "600px")
			)
		)
	)
}

#' Batch Testing Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param selected_columns Reactive vector of column names to test
#' @param debug Enable debug mode
#' @export
mod_batch_testing_server <- function(id, eset, selected_columns, debug = FALSE) {
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
				message("🔍 DEBUG MODE - Batch Testing Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • selected_columns() - Columns to test")
				message("  • batch_testing_results() - ANOVA results")
				message("\nUseful commands:")
				message("  str(batch_testing_results())")
				message("  selected_columns()")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Run ANOVA testing
		# Run ANOVA testing
		batch_testing_results <- reactive({
			req(eset())
			req(selected_columns())
			
			annotations <- selected_columns()
			
			# Need at least one column
			if (length(annotations) == 0) {
				return(NULL)
			}
			
			ExpSet <- eset()
			m <- Biobase::exprs(ExpSet)
			meta <- Biobase::pData(ExpSet)
			
			# Validate annotations exist in metadata
			if (!all(annotations %in% colnames(meta))) {
				missing <- setdiff(annotations, colnames(meta))
				message("Warning: Annotations not found in metadata: ", paste(missing, collapse = ", "))
				annotations <- intersect(annotations, colnames(meta))
				
				if (length(annotations) == 0) {
					return(data.frame())
				}
			}
			
			# Diagnostic: Check each annotation
			message("\n═══ Batch Testing Diagnostics ═══")
			for (ann in annotations) {
				groups <- table(meta[[ann]], useNA = "ifany")
				message(sprintf("Annotation: %s", ann))
				message(sprintf("  Groups: %d", length(groups)))
				message(sprintf("  Samples: %s", paste(names(groups), "=", groups, collapse = ", ")))
			}
			message("═════════════════════════════════\n")
			
			# Run ANOVA
			anova_df <- anova_betadine_tidy_function(m, meta, annotations)
			
			return(anova_df)
		})
		# batch_testing_results <- reactive({
		# 	req(eset())
		# 	req(selected_columns())
		# 	
		# 	annotations <- selected_columns()
		# 	
		# 	# Need at least one column
		# 	if (length(annotations) == 0) {
		# 		return(NULL)
		# 	}
		# 	
		# 	ExpSet <- eset()
		# 	m <- Biobase::exprs(ExpSet)
		# 	meta <- Biobase::pData(ExpSet)
		# 	
		# 	# Run ANOVA
		# 	anova_df <- anova_betadine_tidy_function(m, meta, annotations)
		# 	
		# 	return(anova_df)
		# })
		
		# Status message
		output$testing_status <- renderUI({
			req(selected_columns())
			
			annotations <- selected_columns()
			
			if (length(annotations) == 0) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No columns selected"),
						p("Please select columns in the Column Selection tab.")
					)
				)
			}
			
			if (is.null(batch_testing_results()) || nrow(batch_testing_results()) == 0) {
				return(
					div(
						class = "alert alert-danger",
						icon("times-circle"),
						strong(" Testing failed"),
						p("ANOVA computation encountered an error.   Check that selected columns have valid groupings.")
					)
				)
			}
			
			div(
				class = "alert alert-success",
				icon("check-circle"),
				strong(sprintf(" Testing complete: %d columns tested", length(annotations)))
			)
		})
		
		# ANOVA results table
		output$anova_table <- renderDT({
			req(batch_testing_results())
			
			df <- batch_testing_results()
			
			if (nrow(df) == 0) {
				return(datatable(
					data.frame(Message = "No results available"),
					options = list(dom = 't')
				))
			}
			
			datatable(
				df,
				options = list(
					pageLength = 20,
					scrollX = TRUE,
					dom = 'Bfrtip',
					buttons = c('copy', 'csv', 'excel')
				),
				rownames = FALSE,
				caption = "ANOVA results for batch effect testing"
			) %>%
				formatStyle(
					'p.value',
					backgroundColor = styleInterval(
						c(0.001, 0.01, 0.05),
						c('#ffcccc', '#ffddcc', '#ffffcc', '#ffffff')
					)
				) %>%
				formatRound(columns = c('statistic', 'p.value'), digits = 3)
		})
		
		# Visualization
		output$anova_plot <- renderPlot({
			req(batch_testing_results())
			
			df <- batch_testing_results()
			
			if (nrow(df) == 0) {
				return(NULL)
			}
			
			# Create -log10(p-value) plot
			df_plot <- df %>%
				mutate(
					neg_log_p = -log10(p.value),
					significance = case_when(
						p.value < 0.001 ~ "p < 0.001",
						p.value < 0.01 ~ "p < 0.01",
						p.value < 0.05 ~ "p < 0.05",
						TRUE ~ "n.s."
					)
				) %>%
				arrange(desc(neg_log_p))
			
			ggplot(df_plot, aes(x = reorder(Annotation, neg_log_p), y = neg_log_p, fill = significance)) +
				geom_col() +
				geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
				geom_hline(yintercept = -log10(0.01), linetype = "dashed", color = "darkred") +
				scale_fill_manual(
					values = c(
						"p < 0.001" = "#d32f2f",
						"p < 0.01" = "#f57c00",
						"p < 0.05" = "#fbc02d",
						"n.s." = "#757575"
					)
				) +
				coord_flip() +
				labs(
					title = "Batch Effect Significance",
					subtitle = "Higher values indicate stronger batch effects",
					x = "Annotation Column",
					y = "-log10(p-value)",
					fill = "Significance"
				) +
				theme_minimal(base_size = 14) +
				theme(
					legend.position = "bottom",
					panel.grid.major.y = element_blank()
				)
		})
		
		# Return results for other modules
		return(list(
			results = batch_testing_results
		))
	})
}