#' Annotation Analysis Module - UI
#'
#' Analyzes phenoData columns to identify useful grouping variables
#'
#' @param id Module namespace ID
#' @param debug Show debug button
#' @export
mod_annotation_analysis_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Annotation Column Analysis",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				
				p("This analysis identifies which phenoData columns are useful for grouping samples."),
				p("Filters out columns that are: all unique, all the same, or have only 1 sample per group."),
				
				if (debug) {
					actionButton(
						ns("debug"),
						"Debug",
						icon = icon("bug"),
						class = "btn-warning btn-sm"
					)
				},
				
				hr(),
				
				uiOutput(ns("analysis_output"))
			)
		)
	)
}

#' Annotation Analysis Module - Server
#'
#' @param id Module namespace ID
#' @param eset Reactive ExpressionSet
#' @param debug Enable debug mode
#' @export
mod_annotation_analysis_server <- function(id, eset, debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		
		# Debug observer
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Annotation Analysis Module")
				message("═══════════════════════════════════════════════")
				message("\nAvailable objects:")
				message("  • eset() - Selected ExpressionSet")
				message("  • annotation_analysis() - Analysis results")
				message("\nUseful commands:")
				message("  str(eset())")
				message("  colnames(Biobase::pData(eset()))")
				message("  annotation_analysis()$col_names")
				message("═══════════════════════════════════════════════\n")
				browser()
			})
		}
		
		# Main analysis reactive - runs automatically when eset changes
		annotation_analysis <- reactive({
			req(eset())
			
			ExpSet <- eset()
			pheno_df <- Biobase::pData(ExpSet)
			col_names <- colnames(pheno_df)
			
			table_list <- list()
			
			for (name in col_names) {
				table_entry <- table(pheno_df[, name], useNA = "ifany")
				
				if (length(table_entry) > 0) {
					table_list[[name]] <- list(
						sum = sum(table_entry, na.rm = TRUE),
						min = min(table_entry, na.rm = TRUE),
						max = max(table_entry, na.rm = TRUE),
						n_groups = length(table_entry),
						values = paste(table_entry, collapse = ", "),
						names = paste(names(table_entry), collapse = ", ")
					)
				}
			}
			
			# Convert to dataframe
			df <- do.call(rbind, lapply(table_list, as.data.frame))
			df$column_name <- rownames(df)
			
			# Total samples
			total <- nrow(pheno_df)
			
			# Filter for useful grouping columns
			df_filter <- df %>%
				filter(min > 1) %>%           # At least 2 samples per group
				filter(min != total) %>%      # Not all samples in one group
				filter(n_groups > 1) %>%      # More than 1 group
				filter(n_groups < total) %>% # Not all unique
				mutate(duplicates = duplicated(values))
			
			# Get unique grouping patterns
			df_filter_unique <- df_filter %>%
				filter(! duplicates)
			
			# Useful column names
			col_names_useful <- df_filter_unique$column_name
			
			list(
				df = df,
				df_filter = df_filter,
				df_filter_unique = df_filter_unique,
				col_names = col_names_useful,
				total_samples = total
			)
		})
		
		# Render output UI
		output$analysis_output <- renderUI({
			req(annotation_analysis())
			
			result <- annotation_analysis()
			ns <- session$ns
			
			tagList(
				h4(icon("check-circle"), sprintf(" Found %d useful grouping columns", length(result$col_names))),
				
				p(strong("Total Samples:"), result$total_samples),
				
				hr(),
				
				h4("Unique Grouping Patterns"),
				p("These columns have unique grouping patterns and are most useful for analysis:"),
				DTOutput(ns("table_unique")),
				
				hr(),
				
				h4("All Filtered Columns"),
				p("All columns that meet filtering criteria (some may have duplicate grouping patterns):"),
				DTOutput(ns("table_filter")),
				
				hr(),
				
				h4("All phenoData Columns"),
				p("Complete analysis of all phenoData columns:"),
				DTOutput(ns("table_full"))
			)
		})
		
		# Render tables
		output$table_unique <- renderDT({
			req(annotation_analysis())
			
			df <- annotation_analysis()$df_filter_unique %>%
				select(column_name, n_groups, min, max, sum, names) %>%
				arrange(desc(n_groups))
			
			datatable(
				df,
				options = list(
					pageLength = 10,
					scrollX = TRUE,
					dom = 'Bfrtip',
					buttons = c('copy', 'csv')
				),
				rownames = FALSE,
				caption = "Recommended columns for grouping samples"
			) %>%
				formatStyle('column_name', fontWeight = 'bold')
		})
		
		output$table_filter <- renderDT({
			req(annotation_analysis())
			
			df <- annotation_analysis()$df_filter %>%
				select(column_name, n_groups, min, max, sum, duplicates, names) %>%
				arrange(desc(n_groups))
			
			datatable(
				df,
				options = list(
					pageLength = 10,
					scrollX = TRUE,
					dom = 'Bfrtip',
					buttons = c('copy', 'csv')
				),
				rownames = FALSE,
				caption = "All columns meeting filter criteria"
			) %>%
				formatStyle(
					'duplicates',
					backgroundColor = styleEqual(c(TRUE, FALSE), c('#ffcccc', '#ccffcc'))
				)
		})
		
		output$table_full <- renderDT({
			req(annotation_analysis())
			
			df <- annotation_analysis()$df %>%
				select(column_name, n_groups, min, max, sum, names, values) %>%
				arrange(desc(n_groups))
			
			datatable(
				df,
				options = list(
					pageLength = 20,
					scrollX = TRUE,
					scrollY = "400px",
					dom = 'Bfrtip',
					buttons = c('copy', 'csv')
				),
				rownames = FALSE,
				caption = "Complete phenoData column analysis"
			)
		})
		
		# Return useful column names for other modules to use
		return(list(
			useful_columns = reactive({
				req(annotation_analysis())
				annotation_analysis()$col_names
			}),
			analysis_results = annotation_analysis
		))
	})
}