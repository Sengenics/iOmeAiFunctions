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
			tabPanel(
				"Data",
				tabsetPanel(
					selected = "Expression Matrix",
					tabPanel("featureData", DTOutput(ns("heatmap_fData"))),
					tabPanel("phenoData", DTOutput(ns("heatmap_pData"))),
					tabPanel("Expression Matrix", DTOutput(ns("heatmap_exprs")))
				)
			),
			tabPanel(
				"Heatmap",
				uiOutput(ns("heatmap_ui"))
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
			ExpSet_filtered
		})
		
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
		
		# Render heatmap UI
		output$heatmap_ui <- renderUI({
			req(heatmap_plot())
			withSpinner(
				plotOutput(ns("heatmap_plot"), height = heatmap_plot()$height, width = heatmap_plot()$width),
				type = 4,
				color = "darkblue"
			)
		})
		
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