
#' Annotation Distribution Testing Module - UI
#'
#' Tests if client annotations are evenly distributed across internal batch columns
#'
#' @param id Module namespace ID
#' @export
mod_manifest_upload_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		if (debug) {
			actionButton(
				ns("debug"),
				"Debug:  mod_batch_combined_analysis",
				icon = icon("bug"),
				class = "btn-warning",
				style = "width: 100%;"
			)
			
		},
		column(4,
			fileInput(ns("file"), "Upload Excel Manifest", accept = c(".xlsx", ".xls"))
		),
		column(4,numericInput(ns("sheet"), "Sheet Number", value = 2, min = 1, max = 10)),
		column(4,numericInput(ns("header_row"), "Header Row", value = 2, min = 1, max = 100)),

		column(12,h4("Column Selection")),
		column(6,uiOutput(ns("annotation_columns_ui"))
		),
		column(6,uiOutput(ns("batch_columns_ui"))),
		column(12,tabsetPanel(
			tabPanel('Plot',
							 mod_annotation_distribution_ui("annot_dist",debug = debug),
							 ),
			tabPanel('Manifest',
							 DT::dataTableOutput(ns("preview"))
							 )
		))

	)
}



#' Annotation Distribution Testing Module - UI
#'
#' Tests if client annotations are evenly distributed across internal batch columns
#'
#' @param id Module namespace ID
#' @export
mod_manifest_upload_server <- function(id,debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Combined Batch Analysis Module")
				message("═══════════════════════════════════════════════")
				browser()
			})
		}
		
		manifest_data <- reactive({
			req(input$file)
			readxl::read_excel(
				input$file$datapath, 
				sheet = input$sheet,
				skip = input$header_row - 1
			)
		})
		
		output$preview <- DT:: renderDataTable({
			req(manifest_data())
			DT::datatable(manifest_data(), options = list(pageLength = 10))
		})
		
		
		output$batch_columns_ui <- renderUI({
			req(manifest_data())
			cols <- colnames(manifest_data())
			
			default_batch <- c("Assay","Batch_ID","Run_No","Assay_Date (YYYY/MM/DD)")
			selected_batch <- default_batch[default_batch %in% cols]
			
			selectInput(
				session$ns("batch_columns"),
				"Internal Batches",
				choices = cols,
				selected = selected_batch,
				multiple = TRUE
			)
		})
		
		output$annotation_columns_ui <- renderUI({
			req(manifest_data())
			req(input$batch_columns)
			
			cols <- colnames(manifest_data())
			available_cols <- setdiff(cols, input$batch_columns)
			
			selectInput(
				session$ns("annotation_columns"),
				"Client Annotations",
				choices = available_cols,
				selected = "Sample_Group",
				multiple = TRUE
			)
		})
		
		return(list(
			data = manifest_data,
			batch_columns = reactive(input$batch_columns),
			annotation_columns = reactive(input$annotation_columns)
		))
	})
}


#' Annotation Distribution Testing Module - UI
#'
#' Tests if client annotations are evenly distributed across internal batch columns
#'
#' @param id Module namespace ID
#' @export
mod_annotation_distribution_ui <- function(id,debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			box(
				title = "Client Annotation Distribution Across Internal Batches",
				width = 12,
				status = "info",
				solidHeader = TRUE,
				
				plotOutput(ns("distribution_plot"), height = "600px"),
				hr(),
				
				p("Tests whether client annotations are evenly distributed across internal batch columns. "),
				p("Uses Fisher's exact test to detect uneven distribution (potential confounding)."),
				
				
				uiOutput(ns("test_status")),
				
				hr(),
				
				h4("Distribution Test Results"),
				DTOutput(ns("distribution_table")),
				
				hr(),
				
				
				if (debug) {
					actionButton(
						ns("debug"),
						"Debug:  mod_batch_combined_analysis",
						icon = icon("bug"),
						class = "btn-warning",
						style = "width: 100%;"
					)
					
				},
			)
		)
	)
}

#' Annotation Distribution Testing Module - Server
#'
#' @param id Module namespace ID
#' @param manifest_data Reactive data frame from manifest upload
#' @param batch_columns Reactive character vector - Internal batch column names
#' @param annotation_columns Reactive character vector - Client annotation column names
#' @export
mod_annotation_distribution_server <- function(id, 
																							 manifest_data,
																							 batch_columns,
																							 annotation_columns,
																							 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		if (debug) {
			observeEvent(input$debug, {
				message("\n═══════════════════════════════════════════════")
				message("🔍 DEBUG MODE - Combined Batch Analysis Module")
				message("═══════════════════════════════════════════════")
				browser()
			})
		}
		
		# Run distribution tests
		# distribution_results <- reactive({
		# 	req(manifest_data())
		# 	req(batch_columns())
		# 	req(annotation_columns())
		# 	
		# 	data <- as.data.frame(manifest_data())
		# 	batch_cols <- batch_columns()
		# 	annot_cols <- annotation_columns()
		# 	
		# 	if (length(batch_cols) == 0 || length(annot_cols) == 0) {
		# 		return(NULL)
		# 	}
		# 	
		# 	# Test each annotation against each batch column
		# 	results_list <- list()
		# 	
		# 	for (annot_col in annot_cols) {
		# 		for (batch_col in batch_cols) {
		# 			
		# 			result <- tryCatch({
		# 				# Create contingency table
		# 				tab <- table(data[[annot_col]], data[[batch_col]])
		# 				
		# 				# Fisher's exact test with simulation
		# 				fisher_result <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
		# 				
		# 				# Chi-square test
		# 				chisq_result <- tryCatch({
		# 					chisq.test(tab, simulate.p.value = TRUE, B = 10000)
		# 				}, error = function(e) NULL)
		# 				
		# 				list(
		# 					annotation = annot_col,
		# 					batch_column = batch_col,
		# 					n_annotation_groups = nrow(tab),
		# 					n_batch_groups = ncol(tab),
		# 					fisher_p = fisher_result$p.value,
		# 					chisq_p = if (! is.null(chisq_result)) chisq_result$p.value else NA,
		# 					contingency_table = tab
		# 				)
		# 			}, error = function(e) {
		# 				warning(sprintf("Test failed for '%s' vs '%s': %s", annot_col, batch_col, e$message))
		# 				NULL
		# 			})
		# 			
		# 			if (! is.null(result)) {
		# 				key <- paste(annot_col, batch_col, sep = " vs ")
		# 				results_list[[key]] <- result
		# 			}
		# 		}
		# 	}
		# 	
		# 	if (length(results_list) == 0) {
		# 		return(NULL)
		# 	}
		# 	
		# 	# Convert to data frame
		# 	df <- data.frame(
		# 		Annotation = sapply(results_list, function(x) x$annotation),
		# 		Batch_Column = sapply(results_list, function(x) x$batch_column),
		# 		Annotation_Groups = sapply(results_list, function(x) x$n_annotation_groups),
		# 		Batch_Groups = sapply(results_list, function(x) x$n_batch_groups),
		# 		Fisher_p = signif(sapply(results_list, function(x) x$fisher_p), 3),
		# 		ChiSq_p = signif(sapply(results_list, function(x) x$chisq_p), 3),
		# 		stringsAsFactors = FALSE
		# 	)
		# 	
		# 	# Add significance flag
		# 	df$Issue <- ifelse(df$Fisher_p < 0.05, "⚠️ Uneven", "✓ OK")
		# 	
		# 	list(
		# 		df = df,
		# 		details = results_list
		# 	)
		# })
		
		# Run distribution tests
		distribution_results <- reactive({
			req(manifest_data())
			req(batch_columns())
			req(annotation_columns())
			
			data <- as.data.frame(manifest_data())
			batch_cols <- batch_columns()
			annot_cols <- annotation_columns()
			
			if (length(batch_cols) == 0 || length(annot_cols) == 0) {
				return(NULL)
			}
			
			# Create combined annotation column
			if (length(annot_cols) > 1) {
				data$Combined_Annotation <- apply(
					data[, annot_cols, drop = FALSE], 
					1, 
					function(x) paste(x, collapse = "_")
				)
				
				# Test combined annotation
				annot_cols_with_combined <- c(annot_cols, "Combined_Annotation")
			} else {
				annot_cols_with_combined <- annot_cols
			}
			
			# Test each annotation against each batch column
			results_list <- list()
			
			for (annot_col in annot_cols_with_combined) {
				for (batch_col in batch_cols) {
					
					result <- tryCatch({
						# Create contingency table
						tab <- table(data[[annot_col]], data[[batch_col]])
						
						# Fisher's exact test with simulation
						fisher_result <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
						
						# Chi-square test
						chisq_result <- tryCatch({
							chisq.test(tab, simulate.p.value = TRUE, B = 10000)
						}, error = function(e) NULL)
						
						list(
							annotation = annot_col,
							batch_column = batch_col,
							n_annotation_groups = nrow(tab),
							n_batch_groups = ncol(tab),
							fisher_p = fisher_result$p.value,
							chisq_p = if (!is.null(chisq_result)) chisq_result$p.value else NA,
							contingency_table = tab
						)
					}, error = function(e) {
						warning(sprintf("Test failed for '%s' vs '%s':  %s", annot_col, batch_col, e$message))
						NULL
					})
					
					if (!is.null(result)) {
						key <- paste(annot_col, batch_col, sep = " vs ")
						results_list[[key]] <- result
					}
				}
			}
			
			if (length(results_list) == 0) {
				return(NULL)
			}
			
			# Convert to data frame
			df <- data.frame(
				Annotation = sapply(results_list, function(x) x$annotation),
				Batch_Column = sapply(results_list, function(x) x$batch_column),
				Annotation_Groups = sapply(results_list, function(x) x$n_annotation_groups),
				Batch_Groups = sapply(results_list, function(x) x$n_batch_groups),
				Fisher_p = signif(sapply(results_list, function(x) x$fisher_p), 3),
				ChiSq_p = signif(sapply(results_list, function(x) x$chisq_p), 3),
				stringsAsFactors = FALSE
			)
			
			# Add significance flag
			df$Issue <- ifelse(df$Fisher_p < 0.05, "⚠️ Uneven", "✓ OK")
			
			# Mark combined annotation rows
			df$Type <- ifelse(df$Annotation == "Combined_Annotation", "Combined", "Individual")
			
			list(
				df = df,
				details = results_list
			)
		})
		
		
		
		# Status message
		output$test_status <- renderUI({
			req(batch_columns())
			req(annotation_columns())
			
			if (is.null(distribution_results())) {
				return(
					div(
						class = "alert alert-warning",
						icon("exclamation-triangle"),
						strong(" No data available for testing")
					)
				)
			}
			
			df <- distribution_results()$df
			n_issues <- sum(df$Fisher_p < 0.05)
			
			if (n_issues > 0) {
				div(
					class = "alert alert-danger",
					icon("exclamation-triangle"),
					strong(sprintf(" %d potential confounding issue(s) detected!", n_issues)),
					p("Some annotations are not evenly distributed across batch columns.")
				)
			} else {
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" All annotations are evenly distributed across batches")
				)
			}
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
					order = list(list(4, 'asc'))  # Sort by p-value
				),
				rownames = FALSE
			) %>%
				formatStyle(
					'Fisher_p',
					backgroundColor = styleInterval(
						c(0.001, 0.01, 0.05),
						c('#ffcccc', '#ffddcc', '#ffffcc', '#ffffff')
					)
				) %>%
				formatStyle(
					'Issue',
					color = styleEqual(
						c('⚠️ Uneven', '✓ OK'),
						c('red', 'green')
					),
					fontWeight = 'bold'
				)
		})
		
		# Visualization
		output$distribution_plot <- renderPlot({
			req(distribution_results()) 
			
			df <- distribution_results()$df
			
			# Create plot data
			df_plot <- df %>%
				mutate(
					neg_log_p = -log10(Fisher_p),
					significance = case_when(
						Fisher_p < 0.001 ~ "p < 0.001",
						Fisher_p < 0.01 ~ "p < 0.01",
						Fisher_p < 0.05 ~ "p < 0.05",
						TRUE ~ "n. s."
					)
				) %>%
				arrange(Batch_Column, desc(neg_log_p))
			
			ggplot(df_plot, aes(y = Batch_Column, 
													x = neg_log_p, 
													fill = significance)) +
				geom_col() +
				geom_vline(xintercept = -log10(0.05), linetype = "dashed", color = "red") +
				geom_vline(xintercept = -log10(0.01), linetype = "dashed", color = "darkred") +
				geom_vline(xintercept = -log10(0.001), linetype = "dashed", color = "darkred", size = 1) +
				scale_fill_manual(
					values = c(
						"p < 0.001" = "#d32f2f",
						"p < 0.01" = "#f57c00",
						"p < 0.05" = "#fbc02d",
						"n.s." = "#757575"
					)
				) +
				facet_grid(. ~ Annotation) +
				labs(
					title = "Client Annotation Distribution Across Internal Batches",
					subtitle = "Higher values = more uneven distribution (potential confounding)",
					#x = "Client Annotation",
					x = "-log10(Fisher's p-value)",
					fill = "Significance"
				) +
				theme_minimal(base_size = 14) +
				theme(
					legend.position = "bottom",
					strip.text.y = element_text(face = "bold", size = 11, angle = 0),
					axis.text.x = element_text(angle = 45, hjust = 1)
				)
		})
		# Visualization
		# Visualization
		# output$distribution_plot <- renderPlot({
		# 	req(distribution_results())
		# 	
		# 	df <- distribution_results()$df
		# 	
		# 	# Create plot data
		# 	df_plot <- df %>%
		# 		mutate(
		# 			neg_log_p = -log10(Fisher_p),
		# 			significance = case_when(
		# 				Fisher_p < 0.001 ~ "p < 0.001",
		# 				Fisher_p < 0.01 ~ "p < 0.01",
		# 				Fisher_p < 0.05 ~ "p < 0.05",
		# 				TRUE ~ "n. s."
		# 			)
		# 		) %>%
		# 		arrange(Annotation, desc(neg_log_p))
		# 	
		# 	ggplot(df_plot, aes(x = reorder(Batch_Column, neg_log_p), 
		# 											y = neg_log_p, 
		# 											fill = significance)) +
		# 		geom_col() +
		# 		geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
		# 		geom_hline(yintercept = -log10(0.01), linetype = "dashed", color = "darkred") +
		# 		geom_hline(yintercept = -log10(0.001), linetype = "dashed", color = "darkred", size = 1) +
		# 		scale_fill_manual(
		# 			values = c(
		# 				"p < 0.001" = "#d32f2f",
		# 				"p < 0.01" = "#f57c00",
		# 				"p < 0.05" = "#fbc02d",
		# 				"n.s." = "#757575"
		# 			)
		# 		) +
		# 		facet_wrap(Annotation ~ ., scales = "free_x", ncol = 1) +
		# 		coord_flip() +
		# 		labs(
		# 			title = "Client Annotation Distribution Across Internal Batches",
		# 			subtitle = "Higher values = more uneven distribution (potential confounding)",
		# 			x = "Internal Batch Column",
		# 			y = "-log10(Fisher's p-value)",
		# 			fill = "Significance"
		# 		) +
		# 		theme_minimal(base_size = 14) +
		# 		theme(
		# 			legend.position = "bottom",
		# 			panel.grid.major.y = element_blank(),
		# 			strip.text = element_text(face = "bold", size = 12)
		# 		)
		# })
		# output$distribution_plot <- renderPlot({
		# 	req(distribution_results())
		# 	
		# 	df <- distribution_results()$df
		# 	
		# 	# Create plot data
		# 	df_plot <- df %>%
		# 		mutate(
		# 			neg_log_p = -log10(Fisher_p),
		# 			comparison = paste(Annotation, Batch_Column, sep = " vs "),
		# 			significance = case_when(
		# 				Fisher_p < 0.001 ~ "p < 0.001",
		# 				Fisher_p < 0.01 ~ "p < 0.01",
		# 				Fisher_p < 0.05 ~ "p < 0.05",
		# 				TRUE ~ "n.s."
		# 			)
		# 		) %>%
		# 		arrange(desc(neg_log_p))
		# 	
		# 	ggplot(df_plot, aes(x = reorder(comparison, neg_log_p), 
		# 											y = neg_log_p, 
		# 											fill = significance)) +
		# 		geom_col() +
		# 		geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
		# 		geom_hline(yintercept = -log10(0.01), linetype = "dashed", color = "darkred") +
		# 		geom_hline(yintercept = -log10(0.001), linetype = "dashed", color = "darkred", size = 1) +
		# 		scale_fill_manual(
		# 			values = c(
		# 				"p < 0.001" = "#d32f2f",
		# 				"p < 0.01" = "#f57c00",
		# 				"p < 0.05" = "#fbc02d",
		# 				"n.s." = "#757575"
		# 			)
		# 		) +
		# 		coord_flip() +
		# 		labs(
		# 			title = "Client Annotation Distribution Across Internal Batches",
		# 			subtitle = "Higher values = more uneven distribution (potential confounding)",
		# 			x = "Annotation vs Batch Column",
		# 			y = "-log10(Fisher's p-value)",
		# 			fill = "Significance"
		# 		) +
		# 		theme_minimal(base_size = 14) +
		# 		theme(
		# 			legend.position = "bottom",
		# 			panel.grid.major.y = element_blank()
		# 		)
		# })
		
		return(list(
			results = distribution_results
		))
	})
}