#' Limma Contrast Selection UI Module
#'
#' UI component for selecting variables, building contrasts, and configuring limma analysis
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   limmaContrastUI("limma1")
#' )
#' }
limmaContrastUI <- function(id) {
	ns <- NS(id)
	
	tagList(
		fluidRow(
			column(6,
						 selectInput(ns("variable"), 
						 						"Select Comparison Variable:",
						 						choices = NULL)
			),
			column(6,
						 checkboxInput(ns("use_contrast"), 
						 							"Specify Custom Contrast", 
						 							value = FALSE)
			)
		),
		
		# Custom contrast builder
		conditionalPanel(
			condition = paste0("input['", ns("use_contrast"), "'] == true"),
			fluidRow(
				column(12,
							 h4("Build Contrast"),
							 helpText("Select groups and specify their coefficients (e.g., 1 for positive, -1 for negative, 0.5 for averaged)")
				)
			),
			fluidRow(
				column(12,
							 uiOutput(ns("contrast_builder"))
				)
			),
			fluidRow(
				column(12,
							 verbatimTextOutput(ns("contrast_formula"))
				)
			)
		),
		
		# Covariate selection
		fluidRow(
			column(6,
						 selectInput(ns("covariate1"), 
						 						"Covariate 1 (optional):",
						 						choices = c("None" = ""))
			),
			column(6,
						 selectInput(ns("covariate2"), 
						 						"Covariate 2 (optional):",
						 						choices = c("None" = ""))
			)
		),
		
		# Analysis parameters
		fluidRow(
			column(6,
						 numericInput(ns("p_val"), "P-value threshold:", 
						 						 value = 0.05, min = 0, max = 1, step = 0.01)
			),
			column(6,
						 numericInput(ns("fc_cut"), "Fold change cutoff:", 
						 						 value = 1.1, min = 1, step = 0.1)
			)
		),
		
		fluidRow(
			column(6,
						 checkboxInput(ns("eb_trend"), "eBayes trend", value = TRUE)
			),
			column(6,
						 checkboxInput(ns("eb_robust"), "eBayes robust", value = TRUE)
			)
		),
		
		# Sample distribution summary
		fluidRow(
			column(12,
						 h4("Sample Distribution"),
						 tableOutput(ns("group_summary"))
			)
		),
		
		# Run button
		fluidRow(
			column(12,
						 actionButton(ns("run_limma"), 
						 						 "Run Limma Analysis", 
						 						 class = "btn-primary",
						 						 width = "100%")
			)
		)
	)
}


#' Limma Contrast Selection Server Module
#'
#' Server logic for contrast specification and limma configuration
#'
#' @param id Character string, namespace ID matching the UI
#' @param eSet Reactive expression returning an ExpressionSet object
#' @param feature_select Reactive expression returning character vector of features to analyze
#'
#' @return Reactive expression returning limma analysis results
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import limma
#' @export
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   results <- limmaContrastServer("limma1", 
#'                                   reactive(my_eset), 
#'                                   reactive(selected_features))
#' }
#' }
limmaContrastServer <- function(id, eSet, feature_select) {
	moduleServer(id, function(input, output, session) {
		
		# Extract metadata from ExpressionSet
		metadata <- reactive({
			req(eSet())
			pData(eSet())
		})
		
		# Update variable choices
		observe({
			req(metadata())
			choices <- colnames(metadata())
			updateSelectInput(session, "variable", choices = choices)
			
			# Update covariate choices
			covariate_choices <- c("None" = "", choices)
			updateSelectInput(session, "covariate1", choices = covariate_choices)
			updateSelectInput(session, "covariate2", choices = covariate_choices)
		})
		
		# Get levels of selected variable
		variable_levels <- reactive({
			req(input$variable, metadata())
			var_data <- metadata()[[input$variable]]
			
			# Make syntactically valid names
			if(!is.numeric(var_data) & !is.integer(var_data)){
				var_data[!is.na(var_data)] <- make.names(var_data[!is.na(var_data)])
			}
			
			if(is.factor(var_data) | is.character(var_data)){
				unique(as.character(var_data[!is.na(var_data)]))
			} else {
				NULL
			}
		})
		
		# Dynamic contrast builder UI
		output$contrast_builder <- renderUI({
			req(input$use_contrast)
			req(variable_levels())
			
			ns <- session$ns
			levels <- variable_levels()
			
			lapply(levels, function(level) {
				fluidRow(
					column(6,
								 tags$label(level)
					),
					column(6,
								 numericInput(ns(paste0("coef_", level)), 
								 						 NULL, 
								 						 value = 0, 
								 						 step = 0.5)
					)
				)
			})
		})
		
		# Build contrast matrix
		contrast_matrix <- reactive({
			req(input$use_contrast)
			req(variable_levels())
			
			levels <- variable_levels()
			coefficients <- sapply(levels, function(level) {
				coef <- input[[paste0("coef_", level)]]
				if(is.null(coef)) return(0)
				coef
			})
			
			# Only create contrast if at least one coefficient is non-zero
			if(any(coefficients != 0)){
				# Build contrast string
				contrast_terms <- paste0(coefficients, "*", levels)
				contrast_terms <- contrast_terms[coefficients != 0]
				contrast_string <- paste(contrast_terms, collapse = " ")
				
				# Create contrast matrix
				tryCatch({
					makeContrasts(contrasts = contrast_string, levels = levels)
				}, error = function(e) {
					NULL
				})
			} else {
				NULL
			}
		})
		
		# Display contrast formula
		output$contrast_formula <- renderText({
			req(input$use_contrast)
			req(variable_levels())
			
			levels <- variable_levels()
			coefficients <- sapply(levels, function(level) {
				coef <- input[[paste0("coef_", level)]]
				if(is.null(coef)) return(0)
				coef
			})
			
			if(any(coefficients != 0)){
				terms <- mapply(function(coef, level) {
					if(coef == 0) return(NULL)
					if(coef == 1) return(level)
					if(coef == -1) return(paste0("-", level))
					paste0(coef, "*", level)
				}, coefficients, levels)
				
				terms <- terms[!sapply(terms, is.null)]
				paste("Contrast:", paste(terms, collapse = " "))
			} else {
				"Specify coefficients to build contrast"
			}
		})
		
		# Group summary table
		output$group_summary <- renderTable({
			req(input$variable, metadata())
			
			var_data <- metadata()[[input$variable]]
			summary_df <- as.data.frame(table(var_data))
			colnames(summary_df) <- c(input$variable, "N")
			
			summary_df
		}, rownames = FALSE)
		
		# Run limma analysis when button clicked
		limma_results <- eventReactive(input$run_limma, {
			req(eSet(), feature_select())
			
			withProgress(message = 'Running limma analysis...', {
				tryCatch({
					limma_analysis(
						eSet = eSet(),
						variable = input$variable,
						feature_select = feature_select(),
						covariate1 = if(input$covariate1 == "") NULL else input$covariate1,
						covariate2 = if(input$covariate2 == "") NULL else input$covariate2,
						user_contrast = if(input$use_contrast) contrast_matrix() else NULL,
						EB_trend = input$eb_trend,
						EB_robust = input$eb_robust,
						FC_cut = input$fc_cut,
						p_val = input$p_val
					)
				}, error = function(e) {
					showNotification(paste("Error:", e$message), type = "error")
					NULL
				})
			})
		})
		
		return(limma_results)
	})
}


#' Limma Volcano Plot UI Module
#'
#' UI component for interactive volcano plot of limma results
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
limmaVolcanoUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			column(3,
						 numericInput(ns("max_labels"), "Max labels:", 
						 						 value = 20, min = 0, max = 100)
			),
			column(3,
						 numericInput(ns("point_size"), "Point size:", 
						 						 value = 2, min = 0.5, max = 5, step = 0.5)
			),
			column(3,
						 selectInput(ns("color_palette"), "Color palette:",
						 						choices = c("Dark2", "Set1", "Set2", "Paired"))
			),
			column(3,
						 downloadButton(ns("download_plot"), "Download", class = "btn-sm")
			)
		),
		plotOutput(ns("volcano_plot"), height = "600px")
	)
}


#' Limma Volcano Plot Server Module
#'
#' Server logic for generating interactive volcano plots
#'
#' @param id Character string, namespace ID matching the UI
#' @param limma_results Reactive expression returning limma analysis results
#'
#' @return NULL (generates plot output)
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import ggplot2
#' @import ggrepel
#' @export
limmaVolcanoServer <- function(id, limma_results) {
	moduleServer(id, function(input, output, session) {
		
		volcano_plot <- reactive({
			req(limma_results())
			
			results <- limma_results()
			TT <- results$topTable
			p_val <- results$p_threshold
			FC_cut <- results$fc_threshold
			max_labels <- input$max_labels
			
			# Prepare data
			sigs_ordered <- TT[order(TT$P.Value),]
			sigs_ordered$genelabels <- sigs_ordered$P.Value < p_val & 
				abs(sigs_ordered$logFC) > log2(FC_cut)
			sigs_ordered$threshold <- sigs_ordered$genelabels
			sigs_ordered$symbol <- rownames(sigs_ordered)
			
			# Limit number of labels
			if(sum(sigs_ordered$genelabels) > max_labels){
				label_idx <- which(sigs_ordered$genelabels)[1:max_labels]
				sigs_ordered$genelabels <- FALSE
				sigs_ordered$genelabels[label_idx] <- TRUE
			}
			
			# Create plot
			p <- ggplot(sigs_ordered) +
				geom_point(aes(x = logFC, y = -log10(P.Value), colour = threshold),
									 size = input$point_size) +
				scale_color_brewer(palette = input$color_palette) +
				geom_text_repel(aes(x = logFC, y = -log10(P.Value),
														label = ifelse(genelabels, symbol, "")), 
												max.overlaps = 50) +
				geom_vline(xintercept = c(-log2(FC_cut), log2(FC_cut)), 
									 linetype = "dashed", alpha = 0.5) +
				geom_hline(yintercept = -log10(p_val), 
									 linetype = "dashed", alpha = 0.5) +
				ggtitle("Differential Expression Volcano Plot") +
				xlab("log2 fold change") + 
				ylab("-log10 p-value") +
				theme_bw() +
				theme(legend.position = "none",
							plot.title = element_text(size = rel(1.5), hjust = 0.5),
							axis.title = element_text(size = rel(1.25)))
			
			return(p)
		})
		
		output$volcano_plot <- renderPlot({
			volcano_plot()
		})
		
		output$download_plot <- downloadHandler(
			filename = function() {
				paste0("volcano_plot_", Sys.Date(), ".pdf")
			},
			content = function(file) {
				ggsave(file, volcano_plot(), width = 10, height = 7)
			}
		)
	})
}


#' Limma Heatmap UI Module
#'
#' UI component for interactive heatmap of significant features
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
limmaHeatmapUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			column(3,
						 checkboxInput(ns("row_center"), "Row-center data", value = FALSE)
			),
			column(3,
						 checkboxInput(ns("cluster_cols"), "Cluster columns", value = FALSE)
			),
			column(3,
						 checkboxInput(ns("show_rownames"), "Show row names", value = TRUE)
			),
			column(3,
						 downloadButton(ns("download_plot"), "Download", class = "btn-sm")
			)
		),
		fluidRow(
			column(12,
						 uiOutput(ns("add_anno_ui"))
			)
		),
		fluidRow(
			column(12,
						 plotOutput(ns("heatmap_plot"), height = "800px")
			)
		)
	)
}


#' Limma Heatmap Server Module
#'
#' Server logic for generating interactive heatmaps
#'
#' @param id Character string, namespace ID matching the UI
#' @param limma_results Reactive expression returning limma analysis results
#'
#' @return NULL (generates plot output)
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import pheatmap
#' @export
limmaHeatmapServer <- function(id, limma_results) {
	moduleServer(id, function(input, output, session) {
		
		# UI for additional annotations
		output$add_anno_ui <- renderUI({
			req(limma_results())
			
			ns <- session$ns
			metadata <- limma_results()$metadata
			variable <- limma_results()$variable
			
			# Get other columns for annotation
			other_cols <- setdiff(colnames(metadata), variable)
			
			if(length(other_cols) > 0){
				selectInput(ns("add_anno"), 
										"Additional annotations:",
										choices = c("None" = "", other_cols),
										multiple = TRUE)
			}
		})
		
		# Prepare plot data
		plot_data_prep <- reactive({
			req(limma_results())
			
			add_anno <- if(is.null(input$add_anno) || input$add_anno == "") {
				NULL
			} else {
				input$add_anno
			}
			
			tryCatch({
				prepare_limma_plot_data(
					limma_results = limma_results(),
					add_anno = add_anno,
					row_center = input$row_center
				)
			}, error = function(e) {
				showNotification(paste("Error preparing plot data:", e$message), type = "error")
				NULL
			})
		})
		
		heatmap_plot <- reactive({
			req(plot_data_prep())
			
			plot_prep <- plot_data_prep()
			
			# Create heatmap
			pheatmap(plot_prep$plot_data,
							 annotation_col = plot_prep$plot_metadata,
							 annotation_colors = plot_prep$annotation_colors,
							 cluster_cols = input$cluster_cols,
							 cluster_rows = TRUE,
							 show_rownames = input$show_rownames,
							 show_colnames = FALSE,
							 gaps_col = if(!input$cluster_cols) plot_prep$gap_col else NULL,
							 main = "Significant Features Heatmap")
		})
		
		output$heatmap_plot <- renderPlot({
			req(heatmap_plot())
			heatmap_plot()
		})
		
		output$download_plot <- downloadHandler(
			filename = function() {
				paste0("heatmap_", Sys.Date(), ".pdf")
			},
			content = function(file) {
				pdf(file, width = 12, height = 10)
				print(heatmap_plot())
				dev.off()
			}
		)
	})
}


#' Limma Violin Plot UI Module
#'
#' UI component for violin plots of significant features
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
limmaViolinUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			column(4,
						 numericInput(ns("n_features"), "Number of features to plot:", 
						 						 value = 9, min = 1, max = 50)
			),
			column(4,
						 selectInput(ns("sort_by"), "Sort features by:",
						 						choices = c("P-value" = "P.Value", 
						 												"Log FC" = "logFC",
						 												"Abs Log FC" = "abs_logFC"))
			),
			column(4,
						 downloadButton(ns("download_plot"), "Download", class = "btn-sm")
			)
		),
		fluidRow(
			column(3,
						 numericInput(ns("n_col"), "Columns:", 
						 						 value = 3, min = 1, max = 6)
			),
			column(3,
						 numericInput(ns("n_row"), "Rows:", 
						 						 value = 3, min = 1, max = 6)
			),
			column(3,
						 numericInput(ns("plot_height"), "Plot height (px):", 
						 						 value = 800, min = 400, max = 1200, step = 100)
			),
			column(3,
						 checkboxInput(ns("free_scales"), "Free scales", value = TRUE)
			)
		),
		uiOutput(ns("violin_plot_ui"))
	)
}


#' Limma Violin Plot Server Module
#'
#' Server logic for generating violin plots
#'
#' @param id Character string, namespace ID matching the UI
#' @param limma_results Reactive expression returning limma analysis results
#'
#' @return NULL (generates plot output)
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import ggplot2
#' @import ggforce
#' @import reshape2
#' @export
limmaViolinServer <- function(id, limma_results) {
	moduleServer(id, function(input, output, session) {
		
		# Dynamic UI for plot height
		output$violin_plot_ui <- renderUI({
			ns <- session$ns
			plotOutput(ns("violin_plot"), height = paste0(input$plot_height, "px"))
		})
		
		violin_plot <- reactive({
			req(limma_results())
			
			results <- limma_results()
			TT <- results$topTable
			sig_features <- results$sig_features
			
			if(length(sig_features) < 1){
				return(NULL)
			}
			
			# Sort and select features
			if(input$sort_by == "abs_logFC"){
				TT$abs_logFC <- abs(TT$logFC)
				TT <- TT[order(-TT$abs_logFC),]
			} else {
				TT <- TT[order(TT[[input$sort_by]]),]
			}
			
			sig_features_subset <- intersect(rownames(TT), sig_features)[1:min(input$n_features, length(sig_features))]
			sig_features_subset <- sig_features_subset[!is.na(sig_features_subset)]
			
			# Prepare data
			df <- melt(as.matrix(results$expression[sig_features_subset, 
																							rownames(results$metadata), drop = FALSE]))
			colnames(df)[1:2] <- c("feature", "Sample")
			
			meta_df <- results$metadata
			meta_df$Sample <- rownames(meta_df)
			
			merge_df <- merge(meta_df, df, by = "Sample")
			
			# Define colors
			violin_cols <- c("#009E73", "#BEAED4", "#80B1D3", "goldenrod2", 
											 "coral2", "palevioletred2")
			color_select <- violin_cols[1:nlevels(as.factor(results$metadata[, results$variable]))]
			
			# Create plot
			p <- ggplot(merge_df, aes(x = .data[[results$variable]], 
																y = value, 
																color = .data[[results$variable]])) +
				geom_violin(alpha = 0.5) +
				scale_colour_manual(values = color_select) +
				scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
				theme_minimal() +
				geom_point(position = position_jitter(seed = 1, width = 0.2)) +
				theme(legend.position = "none") +
				facet_wrap(~ feature, 
									 ncol = input$n_col, 
									 nrow = input$n_row,
									 scales = if(input$free_scales) "free" else "fixed")
			
			return(p)
		})
		
		output$violin_plot <- renderPlot({
			req(violin_plot())
			violin_plot()
		})
		
		output$download_plot <- downloadHandler(
			filename = function() {
				paste0("violin_plot_", Sys.Date(), ".pdf")
			},
			content = function(file) {
				req(violin_plot())
				ggsave(file, violin_plot(), 
							 width = 12, 
							 height = input$plot_height / 100)
			}
		)
	})
}


#' Limma Results Summary UI Module
#'
#' UI component for displaying limma results summary and tables
#'
#' @param id Character string, namespace ID for the module
#'
#' @return Shiny UI elements
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @export
limmaResultsUI <- function(id) {
	ns <- NS(id)
	tagList(
		fluidRow(
			column(12,
						 h3("Analysis Summary"),
						 verbatimTextOutput(ns("summary_text"))
			)
		),
		fluidRow(
			column(12,
						 h4("Design Matrix"),
						 verbatimTextOutput(ns("design_matrix"))
			)
		),
		fluidRow(
			column(12,
						 h4("Top Results"),
						 numericInput(ns("n_top"), "Show top N features:", 
						 						 value = 50, min = 10, max = 500, step = 10),
						 DT::dataTableOutput(ns("top_results_table"))
			)
		),
		fluidRow(
			column(12,
						 downloadButton(ns("download_results"), "Download Full Results")
			)
		)
	)
}


#' Limma Results Summary Server Module
#'
#' Server logic for displaying limma results summary
#'
#' @param id Character string, namespace ID matching the UI
#' @param limma_results Reactive expression returning limma analysis results
#'
#' @return NULL (generates output)
#'
#' @note
#' Version 1.0.0
#' File: limma_ShinyModule.R
#'
#' @import shiny
#' @import DT
#' @export
limmaResultsServer <- function(id, limma_results) {
	moduleServer(id, function(input, output, session) {
		
		# Summary text
		output$summary_text <- renderPrint({
			req(limma_results())
			
			results <- limma_results()
			
			cat("Limma Analysis Results\n")
			cat("======================\n\n")
			cat("Variable:", results$variable, "\n")
			cat("Total features analyzed:", nrow(results$topTable), "\n")
			cat("Significant features (p <", results$p_threshold, 
					"& |FC| >", results$fc_threshold, "):", 
					length(results$sig_features), "\n\n")
			
			cat("Group Information:\n")
			for(i in seq_along(results$contrast_info$groups)){
				cat("  ", results$contrast_info$groups[i], ": ", 
						results$contrast_info$n_per_group[i], "samples\n")
			}
			
			if(!is.null(results$contrast_info$contrast_matrix)){
				cat("\nContrast Matrix:\n")
				print(results$contrast_info$contrast_matrix)
			}
		})
		
		# Design matrix
		output$design_matrix <- renderPrint({
			req(limma_results())
			print(limma_results()$design)
		})
		
		# Top results table
		output$top_results_table <- DT::renderDataTable({
			req(limma_results())
			
			TT <- limma_results()$topTable
			TT_display <- head(TT, input$n_top)
			
			# Format for display
			TT_display$logFC <- round(TT_display$logFC, 3)
			TT_display$AveExpr <- round(TT_display$AveExpr, 3)
			TT_display$t <- round(TT_display$t, 3)
			TT_display$P.Value <- formatC(TT_display$P.Value, format = "e", digits = 2)
			TT_display$adj.P.Val <- formatC(TT_display$adj.P.Val, format = "e", digits = 2)
			
			DT::datatable(
				TT_display,
				options = list(
					scrollX = TRUE,
					pageLength = 25
				),
				rownames = TRUE
			)
		})
		
		# Download handler
		output$download_results <- downloadHandler(
			filename = function() {
				paste0("limma_results_", Sys.Date(), ".csv")
			},
			content = function(file) {
				req(limma_results())
				write.csv(limma_results()$topTable, file, row.names = TRUE)
			}
		)
	})
}