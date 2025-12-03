#' Enhanced Heatmap Display Module UI
#'
#' Display heatmap with data tables and download options.
#'
#' @param id Character; module namespace ID
#' @param debug Logical; show debug button
#' @return tagList with display UI
#' @export
#'
#' @note Version 2.0 - Enhanced from HeatmapDisplayModuleUI pattern
#' @export
mod_heatmap_display_enhanced_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		if (debug) {
			fluidRow(
				column(
					width = 12,
					actionButton(
						ns("debug_display"),
						"🔍 Debug Display",
						icon = icon("bug"),
						class = "btn-warning btn-sm",
						style = "margin-bottom:10px;"
					)
				)
			)
		},
		fluidRow(
			column(2, downloadButton(ns("download_heatmap"), "PNG")),
			column(2, downloadButton(ns("download_heatmap_pdf"), "PDF")),
			column(2, downloadButton(ns("download_data"), "Data (RDS)")),
			column(2, actionButton(ns("save_report_pdf"), "Report PDF", icon = icon("file-pdf"))),
			column(2, numericInput(ns("report_width"), "Report Width", 20, min = 5, max = 50)),
			column(2, numericInput(ns("report_height"), "Report Height", 20, min = 5, max = 50))
		),
		tabsetPanel(
			id = ns("display_tabs"),
			selected = "Heatmap",
			# tabPanel(
			# 	"Data",
			# 	tabsetPanel(
			# 		selected = "Expression Matrix",
			# 		tabPanel("featureData", DTOutput(ns("heatmap_fData"))),
			# 		tabPanel("phenoData", DTOutput(ns("heatmap_pData"))),
			# 		tabPanel("Expression Matrix", DTOutput(ns("heatmap_exprs")))
			# 	)
			# ),
			
			tabPanel(
				"Data",
				downloadButton(ns("download_excel"), "All Data (Excel)", class = "btn-success btn-sm"),
				tabsetPanel(
					selected = "Expression Matrix",
					tabPanel("featureData", 
									 downloadButton(ns("download_tsv_fdata"), "featureData (.tsv)", class = "btn-primary btn-sm"),
									 DTOutput(ns("heatmap_fData"))),
					tabPanel("phenoData", 
									 downloadButton(ns("download_tsv_pdata"), "phenoData (.tsv)", class = "btn-primary btn-sm"),
									 DTOutput(ns("heatmap_pData"))),
					tabPanel("Expression Matrix", 
									 downloadButton(ns("download_tsv_exprs"), "Expression Matrix (.tsv)", class = "btn-primary btn-sm"),
									 DTOutput(ns("heatmap_exprs"))),
					tabPanel('Matrix Comparison',
									 fluidRow(
									 	column(12,
									 				 box(
									 				 	title = "Compare Matrix with Uploaded File",
									 				 	width = 12,
									 				 	status = "warning",
									 				 	#collapsible = TRUE,
									 				 	#collapsed = TRUE,
									 				 	fluidRow(
									 				 		column(6,
									 				 					 fileInput(
									 				 					 	ns("upload_compare_matrix"),
									 				 					 	"Upload Matrix File (CSV/TSV)",
									 				 					 	accept = c(".csv", ".tsv", ".txt")
									 				 					 ),
									 				 					 radioButtons(
									 				 					 	ns("compare_format"),
									 				 					 	"File Format:",
									 				 					 	choices = c("CSV" = "csv", "TSV" = "tsv"),
									 				 					 	selected = "tsv",
									 				 					 	inline = TRUE
									 				 					 )
									 				 		),
									 				 		column(6,
									 				 					 actionButton(
									 				 					 	ns("run_comparison"),
									 				 					 	"Run Comparison",
									 				 					 	icon = icon("balance-scale"),
									 				 					 	class = "btn-warning"
									 				 					 ),
									 				 					 br(), br(),
									 				 					 uiOutput(ns("comparison_summary"))
									 				 		)
									 				 	),
									 				 	conditionalPanel(
									 				 		condition = "output.uploaded_matrix_available",
									 				 		ns = ns,
									 				 		hr(),
									 				 		h4("Uploaded Matrix Preview"),
									 				 		DTOutput(ns("uploaded_matrix_table"))
									 				 	),
									 				 	hr(),
									 				 	verbatimTextOutput(ns("comparison_results")),
									 				 	uiOutput(ns("comparison_plots_ui"))
									 				 )
									 	)
									 ))
				)
			),
			tabPanel(
				"Heatmap",
					tags$style(HTML("
				    .shiny-spinner-output-container {
				      position: sticky ! important;
				      top: 20px !important;
				      z-index: 1000 !important;
				    }
				  ")),
				uiOutput(ns("zoom_control")), 
				shinycssloaders::withSpinner(
					uiOutput(ns("heatmap_ui")),
					type = 4,
					color = "darkblue"
				)
				#uiOutput(ns("heatmap_ui"))
			)
		)
	)
}


#' Enhanced Heatmap Display Module Server
#'
#' Server logic for rendering and downloading heatmap.
#'
#' @param id Character; module namespace ID
#' @param eset Reactive ExpressionSet
#' @param assay_name Reactive character; assay to display
#' @param controls List of reactive values from mod_heatmap_controls_enhanced_server
#' @param plot_path Reactive or static character; path to save PDF reports
#' @param debug Logical; enable debug
#'
#' @return Reactive expression with filtered ExpSet and plot
#' @export
#'
#' @note Version 2.0 - Enhanced from HeatmapDisplayModule pattern
#' @export
mod_heatmap_display_enhanced_server <- function(id,
																								eset,
																								assay_name,
																								controls,
																								plot_path = NULL,
																								debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# Debug handler
		observeEvent(input$debug_display, {
			message("\n╔═══════════════════════════════════════════════════════════╗")
			message("║       🔍 DEBUG MODE - Enhanced Heatmap Display           ║")
			message("╚═══════════════════════════════════════════════════════════╝")
			message("Generate: ", controls$generate_heatmap())
			message("Filtered ExpSet dims: ", if (! is.null(filtered_eset())) dim(filtered_eset()))
			browser()
		})
		
		# Apply subsetting to ExpSet
		filtered_eset <- reactive({
			req(eset())
			req(controls$generate_heatmap())
			
			ExpSet_filtered <- eset()
			
			# Row filtering
			subset_row <- controls$subset_row()
			subset_row_values <- controls$subset_row_values()
			if (!is.null(subset_row) && subset_row != "None" && !is.null(subset_row_values)) {
				tryCatch({
					row_filter <- Biobase::fData(ExpSet_filtered) %>%
						data.frame() %>%
						filter(!!sym(subset_row) %in% subset_row_values) %>%
						pull(Protein)
					ExpSet_filtered <- ExpSet_filtered[featureNames(ExpSet_filtered) %in% row_filter, ]
				}, error = function(e) {
					message("Row filtering failed: ", e$message)
				})
			}
			
			# Column filtering
			subset_column <- controls$subset_column()
			subset_column_values <- controls$subset_column_values()
			if (!is.null(subset_column) && subset_column != "None" && !is.null(subset_column_values)) {
				tryCatch({
					column_filter <- Biobase::pData(ExpSet_filtered) %>%
						data.frame() %>%
						filter(!!sym(subset_column) %in% subset_column_values) %>%
						pull(Sample)
					ExpSet_filtered <- ExpSet_filtered[, sampleNames(ExpSet_filtered) %in% column_filter]
				}, error = function(e) {
					message("Column filtering failed: ", e$message)
				})
			}
			
			message("Filtered ExpSet: ", nrow(ExpSet_filtered), " features × ", ncol(ExpSet_filtered), " samples")
			
			if (isTRUE(controls$round_matrix())) {
				digits <- controls$round_digits()
				
				# Round the expression matrix
				expr_matrix <- Biobase::exprs(ExpSet_filtered)
				expr_matrix_rounded <- round(expr_matrix, digits = digits)
				
				# Update the ExpSet with rounded values
				Biobase::exprs(ExpSet_filtered) <- expr_matrix_rounded
				
				message("Applied rounding: ", digits, " decimal places")
			}
			
			ExpSet_filtered
		})
		
		# Individual TSV downloads
		output$download_tsv_exprs <- downloadHandler(
			filename = function() paste0(controls$heatmap_plot_name(), "_expression_matrix.tsv"),
			content = function(file) {
				req(filtered_eset())
				exprs_data <- Biobase::exprs(filtered_eset()) %>%
					as.data.frame() %>%
					rownames_to_column("Protein")
				data.table::fwrite(exprs_data, file, sep = "\t")
			}
		)
		
		output$download_tsv_pdata <- downloadHandler(
			filename = function() paste0(controls$heatmap_plot_name(), "_phenoData.tsv"),
			content = function(file) {
				req(filtered_eset())
				req(controls$column_annotations())
				pdata <- Biobase::pData(filtered_eset()) %>%
					as.data.frame() %>%
					select(all_of(controls$column_annotations())) %>%
					rownames_to_column("Sample")
				data.table::fwrite(pdata, file, sep = "\t")
			}
		)
		
		output$download_tsv_fdata <- downloadHandler(
			filename = function() paste0(controls$heatmap_plot_name(), "_featureData.tsv"),
			content = function(file) {
				req(filtered_eset())
				req(controls$row_annotations())
				tryCatch({
					fdata <- Biobase::fData(filtered_eset()) %>%
						as.data.frame() %>%
						select(all_of(controls$row_annotations())) %>%
						rownames_to_column("Protein")
					data.table::fwrite(fdata, file, sep = "\t")
				}, error = function(e) {
					# If no fData, create empty file
					data.table::fwrite(data.frame(Message = "No feature data available"), file, sep = "\t")
				})
			}
		)
		
		# Excel workbook with all sheets
		output$download_excel <- downloadHandler(
			filename = function() paste0(controls$heatmap_plot_name(), "_all_data.xlsx"),
			content = function(file) {
				req(filtered_eset())
				
				# Prepare expression matrix
				exprs_data <- Biobase::exprs(filtered_eset()) %>%
					as.data.frame() %>%
					rownames_to_column("Protein")
				
				# Prepare phenoData
				pdata <- Biobase::pData(filtered_eset()) %>%
					as.data.frame() %>%
					rownames_to_column("Sample")
				
				# Only include selected annotation columns if any
				if (!is.null(controls$column_annotations()) && length(controls$column_annotations()) > 0) {
					cols_to_keep <- c("Sample", controls$column_annotations())
					cols_to_keep <- cols_to_keep[cols_to_keep %in% colnames(pdata)]
					pdata <- pdata %>% select(all_of(cols_to_keep))
				}
				
				# Prepare featureData
				fdata <- tryCatch({
					fd <- Biobase::fData(filtered_eset()) %>%
						as.data.frame() %>%
						rownames_to_column("Protein")
					
					# Only include selected annotation columns if any
					if (!is.null(controls$row_annotations()) && length(controls$row_annotations()) > 0) {
						cols_to_keep <- c("Protein", controls$row_annotations())
						cols_to_keep <- cols_to_keep[cols_to_keep %in% colnames(fd)]
						fd <- fd %>% select(all_of(cols_to_keep))
					}
					fd
				}, error = function(e) {
					data.frame(Message = "No feature data available")
				})
				
				# Create workbook
				wb <- openxlsx::createWorkbook()
				
				# Add sheets
				openxlsx::addWorksheet(wb, "Expression Matrix")
				openxlsx::writeData(wb, "Expression Matrix", exprs_data)
				
				openxlsx::addWorksheet(wb, "phenoData")
				openxlsx::writeData(wb, "phenoData", pdata)
				
				openxlsx::addWorksheet(wb, "featureData")
				openxlsx::writeData(wb, "featureData", fdata)
				
				# Save workbook
				openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
			}
		)
		
		# Matrix comparison feature
		uploaded_matrix <- reactive({
			req(input$upload_compare_matrix)
			
			file_path <- input$upload_compare_matrix$datapath
			format <- input$compare_format
			
			tryCatch({
				if (format == "csv") {
					data <- data.table::fread(file_path, sep = ",")
				} else {
					data <- data.table::fread(file_path, sep = "\t")
				}
				
				# Convert to matrix (assuming first column is row names)
				if (ncol(data) > 1) {
					row_names <- data[[1]]
					mat <- as.matrix(data[, -1])
					rownames(mat) <- row_names
					
					message("Uploaded matrix: ", nrow(mat), " x ", ncol(mat))
					return(mat)
				} else {
					stop("File must have at least 2 columns")
				}
			}, error = function(e) {
				showNotification(
					paste("Error reading file:", e$message),
					type = "error",
					duration = 10
				)
				return(NULL)
			})
		})
		
		# Flag for conditionalPanel
		output$uploaded_matrix_available <- reactive({
			! is.null(uploaded_matrix())
		})
		outputOptions(output, "uploaded_matrix_available", suspendWhenHidden = FALSE)
		
		# Display uploaded matrix as table
		output$uploaded_matrix_table <- renderDT({
			req(uploaded_matrix())
			
			mat <- uploaded_matrix()
			
			# Convert to data frame with row names as first column
			df <- as.data.frame(mat) %>%
				rownames_to_column("Protein")
			
			datatable(
				df,
				options = list(
					scrollX = TRUE,
					scrollY = "400px",
					pageLength = 10,
					dom = 'Bfrtip',
					buttons = c('copy', 'csv')
				),
				rownames = FALSE,
				caption = paste0("Uploaded Matrix: ", nrow(mat), " rows × ", ncol(mat), " columns")
			)
		})
		
		comparison_result <- eventReactive(input$run_comparison, {
			req(uploaded_matrix())
			req(filtered_eset())
			
			# Get current heatmap matrix
			current_matrix <- Biobase::exprs(filtered_eset())
			uploaded_mat <- uploaded_matrix()
			
			# Comparison analysis
			result <- list()
			
			# Dimension check
			result$dims_match <- identical(dim(current_matrix), dim(uploaded_mat))
			result$current_dims <- dim(current_matrix)
			result$uploaded_dims <- dim(uploaded_mat)
			
			# If dimensions match, do detailed comparison
			if (result$dims_match) {
				# Check row/col names
				result$rownames_match <- identical(rownames(current_matrix), rownames(uploaded_mat))
				result$colnames_match <- identical(colnames(current_matrix), colnames(uploaded_mat))
				
				# Align matrices if names don't match but overlap exists
				common_rows <- intersect(rownames(current_matrix), rownames(uploaded_mat))
				common_cols <- intersect(colnames(current_matrix), colnames(uploaded_mat))
				
				result$common_rows <- length(common_rows)
				result$common_cols <- length(common_cols)
				
				if (length(common_rows) > 0 && length(common_cols) > 0) {
					# Subset to common elements
					m1 <- current_matrix[common_rows, common_cols]
					m2 <- uploaded_mat[common_rows, common_cols]
					
					# Calculate differences
					diff_matrix <- m1 - m2
					
					result$identical <- identical(m1, m2)
					result$all_equal <- all.equal(m1, m2)
					result$max_abs_diff <- max(abs(diff_matrix), na.rm = TRUE)
					result$mean_abs_diff <- mean(abs(diff_matrix), na.rm = TRUE)
					result$median_abs_diff <- median(abs(diff_matrix), na.rm = TRUE)
					
					# Count significant differences
					result$n_diff_0.001 <- sum(abs(diff_matrix) > 0.001, na.rm = TRUE)
					result$n_diff_0.01 <- sum(abs(diff_matrix) > 0.01, na.rm = TRUE)
					result$n_diff_0.1 <- sum(abs(diff_matrix) > 0.1, na.rm = TRUE)
					result$n_diff_1 <- sum(abs(diff_matrix) > 1, na.rm = TRUE)
					
					# Store difference matrix for plotting
					result$diff_matrix <- diff_matrix
					result$m1 <- m1
					result$m2 <- m2
					
					# Find top differences
					abs_diff <- abs(diff_matrix)
					top_idx <- order(abs_diff, decreasing = TRUE, na.last = TRUE)[1:min(20, length(abs_diff))]
					top_diff <- data.frame(
						Row = rownames(diff_matrix)[row(diff_matrix)[top_idx]],
						Col = colnames(diff_matrix)[col(diff_matrix)[top_idx]],
						Current = m1[top_idx],
						Uploaded = m2[top_idx],
						Difference = diff_matrix[top_idx],
						Abs_Diff = abs_diff[top_idx]
					)
					result$top_differences <- top_diff[! is.na(top_diff$Abs_Diff), ]
					
				} else {
					result$no_overlap <- TRUE
				}
			}
			
			return(result)
		})
		
		output$comparison_summary <- renderUI({
			req(comparison_result())
			res <- comparison_result()
			
			if (!res$dims_match) {
				div(
					class = "alert alert-danger",
					icon("times-circle"),
					strong(" Dimension Mismatch"),
					p(paste("Current:", paste(res$current_dims, collapse = " x "))),
					p(paste("Uploaded:", paste(res$uploaded_dims, collapse = " x ")))
				)
			} else if (isTRUE(res$no_overlap)) {
				div(
					class = "alert alert-warning",
					icon("exclamation-triangle"),
					strong(" No Overlapping Row/Column Names")
				)
			} else if (res$identical) {
				div(
					class = "alert alert-success",
					icon("check-circle"),
					strong(" Matrices are IDENTICAL")
				)
			} else if (isTRUE(res$all_equal)) {
				div(
					class = "alert alert-info",
					icon("info-circle"),
					strong(" Matrices are Equal"),
					p("(within floating-point tolerance)")
				)
			} else {
				div(
					class = "alert alert-warning",
					icon("exclamation-triangle"),
					strong(" Matrices DIFFER"),
					p(sprintf("Max difference: %.6e", res$max_abs_diff))
				)
			}
		})
		
		output$comparison_results <- renderPrint({
			req(comparison_result())
			res <- comparison_result()
			
			cat("═══════════════════════════════════════════════════════════\n")
			cat("MATRIX COMPARISON RESULTS\n")
			cat("═══════════════════════════════════════════════════════════\n\n")
			
			cat("DIMENSIONS:\n")
			cat("  Current Matrix:  ", paste(res$current_dims, collapse = " x "), "\n")
			cat("  Uploaded Matrix: ", paste(res$uploaded_dims, collapse = " x "), "\n")
			cat("  Dimensions Match:", res$dims_match, "\n\n")
			
			if (!res$dims_match) {
				cat("❌ Cannot compare - dimensions differ\n")
				return()
			}
			
			if (isTRUE(res$no_overlap)) {
				cat("❌ Cannot compare - no overlapping row/column names\n")
				return()
			}
			
			cat("ALIGNMENT:\n")
			cat("  Row Names Match:   ", res$rownames_match, "\n")
			cat("  Column Names Match:", res$colnames_match, "\n")
			cat("  Common Rows:       ", res$common_rows, "\n")
			cat("  Common Columns:    ", res$common_cols, "\n\n")
			
			cat("COMPARISON:\n")
			cat("  Identical:         ", res$identical, "\n")
			cat("  All Equal:         ", isTRUE(res$all_equal), "\n")
			if (! isTRUE(res$all_equal)) {
				cat("  Equality Result:   ", as.character(res$all_equal), "\n")
			}
			cat("\n")
			
			cat("DIFFERENCE STATISTICS:\n")
			cat("  Max Absolute Diff:    ", sprintf("%.6e", res$max_abs_diff), "\n")
			cat("  Mean Absolute Diff:   ", sprintf("%.6e", res$mean_abs_diff), "\n")
			cat("  Median Absolute Diff: ", sprintf("%.6e", res$median_abs_diff), "\n\n")
			
			cat("DIFFERENCE COUNTS:\n")
			cat("  |diff| > 0.001:  ", res$n_diff_0.001, "\n")
			cat("  |diff| > 0.01:   ", res$n_diff_0.01, "\n")
			cat("  |diff| > 0.1:    ", res$n_diff_0.1, "\n")
			cat("  |diff| > 1.0:    ", res$n_diff_1, "\n\n")
			
			if (!is.null(res$top_differences) && nrow(res$top_differences) > 0) {
				cat("TOP DIFFERENCES:\n")
				print(head(res$top_differences, 10))
			}
			
			cat("\n═══════════════════════════════════════════════════════════\n")
		})
		
		output$comparison_plots_ui <- renderUI({
			req(comparison_result())
			res <- comparison_result()
			
			if (is.null(res$diff_matrix)) {
				return(NULL)
			}
			
			tagList(
				h4("Difference Visualizations"),
				fluidRow(
					column(6, plotOutput(ns("diff_histogram"))),
					column(6, plotOutput(ns("diff_scatter")))
				),
				fluidRow(
					column(12, 
								 downloadButton(ns("download_diff_matrix"), "Download Difference Matrix (.tsv)", 
								 							 class = "btn-warning btn-sm")
					)
				)
			)
		})
		
		output$diff_histogram <- renderPlot({
			req(comparison_result())
			res <- comparison_result()
			req(res$diff_matrix)
			
			diff_vec <- as.vector(res$diff_matrix)
			
			hist(diff_vec, 
					 breaks = 50,
					 main = "Distribution of Differences",
					 xlab = "Current - Uploaded",
					 col = "steelblue",
					 border = "white")
			abline(v = 0, col = "red", lwd = 2, lty = 2)
		})
		
		output$diff_scatter <- renderPlot({
			req(comparison_result())
			res <- comparison_result()
			req(res$m1)
			req(res$m2)
			
			plot(as.vector(res$m2), as.vector(res$m1),
					 xlab = "Uploaded Matrix",
					 ylab = "Current Matrix",
					 main = "Value Comparison",
					 pch = 20,
					 col = rgb(0, 0, 1, 0.3))
			abline(0, 1, col = "red", lwd = 2)
		})
		
		output$download_diff_matrix <- downloadHandler(
			filename = function() paste0(controls$heatmap_plot_name(), "_differences.tsv"),
			content = function(file) {
				req(comparison_result())
				res <- comparison_result()
				req(res$diff_matrix)
				
				diff_data <- as.data.frame(res$diff_matrix) %>%
					rownames_to_column("Protein")
				data.table::fwrite(diff_data, file, sep = "\t")
			}
		)
		
		# Generate heatmap plot
		heatmap_plot <- reactive({
			req(filtered_eset())
			req(controls$generate_heatmap())
			
			ExpSet_use <- filtered_eset()
			
			# Determine plot dimensions
			if (controls$size() == "full") {
				height <- 150 + (nrow(ExpSet_use) * controls$size_numeric())
				width <- 300 + (ncol(ExpSet_use) * controls$size_numeric() * 2)
				fontsize <- controls$font_size()
			} else {
				height <- 800
				width <- 1000
				fontsize <- 8
			}
			#height = 100
			# Get assay data
			# assay <- if (is.reactive(assay_name)) assay_name() else assay_name
			# m <- Biobase::assayDataElement(ExpSet_use, assay)
			
			# Get assay data - ensure it's a character string
			assay_to_use <- if (is.reactive(assay_name)) {
				assay_name()
			} else {
				assay_name
			}
			
			# Validate assay name
			req(assay_to_use)
			available_assays <- Biobase::assayDataElementNames(ExpSet_use)
			if (! assay_to_use %in% available_assays) {
				assay_to_use <- available_assays[1]  # fallback to first assay
			}
			
			m <- Biobase::assayDataElement(ExpSet_use, assay_to_use)

			
			# Apply intensity range clipping
			min_range <- controls$minimum_range()
			max_range <- controls$maximum_range()
			if (!is.null(min_range) && !is.null(max_range)) {
				m[m < min_range] <- min_range
				m[m > max_range] <- max_range
			}
			
			# if (isTRUE(controls$round_matrix())) {
			# 	digits <- controls$round_digits()
			# 	m <- round(m, digits = digits)
			# }
			
			# Build annotation columns
			annot_col <- NULL
			annot_colors <- NULL
			if (! is.null(controls$column_annotations()) && length(controls$column_annotations()) > 0) {
				annot_col <- Biobase::pData(ExpSet_use)[, controls$column_annotations(), drop = FALSE]
				annot_col[] <- lapply(annot_col, factor)
				annot_colors <- color_distinct(annot_col, colnames(annot_col))
			}
			
			# Build annotation rows
			annot_row <- NULL
			if (!is.null(controls$row_annotations()) && length(controls$row_annotations()) > 0) {
				tryCatch({
					annot_row <- Biobase::fData(ExpSet_use)[, controls$row_annotations(), drop = FALSE]
					annot_row[] <- lapply(annot_row, factor)
					annot_row_colors <- color_distinct(annot_row, colnames(annot_row))
					if (!is.null(annot_colors)) {
						annot_colors <- c(annot_colors, annot_row_colors)
					} else {
						annot_colors <- annot_row_colors
					}
				}, error = function(e) {
					message("Row annotation failed: ", e$message)
				})
			}
			
			# Generate pheatmap
			p <- pheatmap::pheatmap(
				mat = m,
				annotation_col = annot_col,
				annotation_row = annot_row,
				annotation_colors = annot_colors,
				cluster_rows = controls$cluster_rows(),
				cluster_cols = controls$cluster_columns(),
				clustering_distance_rows = controls$cluster_rows_method(),
				clustering_distance_cols = controls$cluster_columns_method(),
				show_rownames = controls$show_rownames(),
				show_colnames = controls$show_colnames(),
				fontsize = fontsize,
				border_color = NA
			)
			
			list(plot = p, height = height, width = width)
		})
		
		
		# Zoom control (separate from heatmap UI)
		output$zoom_control <- renderUI({
			div(
				style = "margin-bottom: 10px; padding: 10px; background-color: #f5f5f5; border-radius: 4px;",
				sliderInput(
					ns("zoom_level"),
					"Zoom Level:",
					min = 0,
					max = 300,
					value = 100,
					step = 5,
					post = "%",
					width = "1200"
				)
			)
		})
		
		# Heatmap UI (references zoom_level but doesn't re-render it)
		output$heatmap_ui <- renderUI({
			req(heatmap_plot())
			
			zoom <- input$zoom_level
			if (is.null(zoom)) zoom <- 100
			
			div(
				style = "overflow: auto; max-height: 900px; border: 2px solid #3c8dbc; border-radius: 4px; background: #fff;",
				div(
					style = sprintf("transform: scale(%s); transform-origin: top left; width: fit-content;",
													zoom / 100),
					plotOutput(ns("heatmap_plot"), 
										 height = heatmap_plot()$height, 
										 width = heatmap_plot()$width)
				)
			)
		})
		
		# Render heatmap UI
		
		# output$heatmap_ui <- renderUI({
		# 	req(heatmap_plot())
		# 	
		# 		plotOutput(ns("heatmap_plot"), height = heatmap_plot()$height, width = heatmap_plot()$width)
		# })
		
		# output$heatmap_ui <- renderUI({
		# 	req(heatmap_plot())
		# 	
		# 	tagList(
		# 		# Zoom controls
		# 		div(
		# 			style = "margin-bottom: 10px; padding: 10px; background-color: #f5f5f5; border-radius: 4px;",
		# 			sliderInput(
		# 				ns("zoom_level"),
		# 				"Zoom:",
		# 				min = 50,
		# 				max = 200,
		# 				value = 100,
		# 				step = 10,
		# 				post = "%",
		# 				width = "300px"
		# 			)
		# 		),
		# 		
		# 		# Scrollable container with zoom
		# 		div(
		# 			style = sprintf(
		# 				"overflow: auto; max-height: 800px; max-width: 100%%; border: 1px solid #ddd; padding: 10px; background: white;"
		# 			),
		# 			div(
		# 				style = sprintf("transform: scale(%s); transform-origin: top left; width: %s%%; height: %s%%;",
		# 												input$zoom_level / 100,
		# 												100 / (input$zoom_level / 100),
		# 												100 / (input$zoom_level / 100)),
		# 				plotOutput(ns("heatmap_plot"), 
		# 									 height = heatmap_plot()$height, 
		# 									 width = heatmap_plot()$width)
		# 			)
		# 		)
		# 	)
		# })
		# output$heatmap_ui <- renderUI({
		# 	req(heatmap_plot())
		# 	withSpinner(
		# 		plotOutput(ns("heatmap_plot"), height = heatmap_plot()$height, width = heatmap_plot()$width),
		# 		type = 4,
		# 		color = "darkblue"
		# 	)
		# })
		
		# output$heatmap_ui <- renderUI({
		# 	req(heatmap_plot())
		# 	shinycssloaders::withSpinner(
		# 		div(
		# 			style = "position: relative; width: 100%;",
		# 			plotOutput(ns("heatmap_plot"), height = heatmap_plot()$height, width = heatmap_plot()$width)
		# 		),
		# 		type = 4,
		# 		color = "darkblue",
		# 		proxy.height = "200px"  # Shows spinner in a fixed height area at top
		# 	)
		# })
		
		output$heatmap_plot <- renderPlot({
			req(heatmap_plot())
			heatmap_plot()$plot
		}, height = function() {
			if (! is.null(heatmap_plot())) heatmap_plot()$height else 800
		}, width = function() {
			if (!is.null(heatmap_plot())) heatmap_plot()$width else 1000
		})
		
		# Data tables
		output$heatmap_exprs <- renderDT({
			req(filtered_eset())
			Biobase::exprs(filtered_eset()) %>%
				as.data.frame() %>%
				rownames_to_column("Protein")
		})
		
		output$heatmap_pData <- renderDT({
			req(filtered_eset())
			req(controls$column_annotations())
			Biobase::pData(filtered_eset()) %>%
				as.data.frame() %>%
				select(all_of(controls$column_annotations())) %>%
				rownames_to_column("Sample")
		})
		
		output$heatmap_fData <- renderDT({
			req(filtered_eset())
			req(controls$row_annotations())
			tryCatch({
				Biobase::fData(filtered_eset()) %>%
					as.data.frame() %>%
					select(all_of(controls$row_annotations())) %>%
					rownames_to_column("Protein")
			}, error = function(e) data.frame())
		})
		
		# Download handlers
		scaling_factor <- 2
		
		output$download_heatmap <- downloadHandler(
			filename = function() paste0(controls$heatmap_plot_name(), ".png"),
			content = function(file) {
				req(heatmap_plot())
				png(file, height = heatmap_plot()$height * scaling_factor,
						width = heatmap_plot()$width * scaling_factor, res = 96)
				print(heatmap_plot()$plot)
				dev.off()
			}
		)
		
		output$download_heatmap_pdf <- downloadHandler(
			filename = function() paste0(controls$heatmap_plot_name(), ".pdf"),
			content = function(file) {
				req(heatmap_plot())
				pdf(file, height = heatmap_plot()$height / 50,
						width = heatmap_plot()$width / 50)
				print(heatmap_plot()$plot)
				dev.off()
			}
		)
		
		output$download_data <- downloadHandler(
			filename = function() paste0(controls$heatmap_plot_name(), "_data.rds"),
			content = function(file) {
				req(filtered_eset())
				saveRDS(filtered_eset(), file)
			}
		)
		
		observeEvent(input$save_report_pdf, {
			req(heatmap_plot())
			req(plot_path)
			
			path <- if (is.reactive(plot_path)) plot_path() else plot_path
			plot_name <- paste0(controls$heatmap_plot_name(), "_report.pdf")
			save_path <- file.path(path, plot_name)
			
			if (controls$size() == "full") {
				h <- heatmap_plot()$height / 50
				w <- heatmap_plot()$width / 50
			} else {
				h <- input$report_height
				w <- input$report_width
			}
			
			pdf(save_path, width = w, height = h)
			print(heatmap_plot()$plot)
			dev.off()
			
			showNotification(
				paste("Report saved to:", save_path),
				type = "message",
				duration = 5
			)
		})
		
		return(list(
			filtered_eset = filtered_eset,
			heatmap_plot = heatmap_plot
		))
	})
}