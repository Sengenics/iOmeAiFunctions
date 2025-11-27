#' Enhanced Heatmap Controls Module UI
#'
#' Comprehensive heatmap configuration UI with annotation selection,
#' subsetting, clustering, ordering, and display controls. 
#'
#' @param id Character; module namespace ID
#' @param debug Logical; show debug button (default FALSE)
#' @return tagList with control UI elements
#' @export
#'
#' @note Version 2.0 - Enhanced from Heatmap_AnnotationSelectionUI pattern
#' @export
mod_heatmap_controls_enhanced_ui <- function(id, debug = FALSE) {
	ns <- NS(id)
	
	tagList(
		if (debug) {
			fluidRow(
				column(
					width = 12,
					actionButton(
						ns("debug_controls"),
						"🔍 Debug Controls",
						icon = icon("bug"),
						class = "btn-warning btn-sm",
						style = "margin-bottom:10px;"
					)
				)
			)
		},
		fluidRow(
			# Column controls
			column(
				width = 4,
				h4("Column (Sample) Controls"),
				uiOutput(ns("column_annotations_ui")),
				uiOutput(ns("subset_column_ui")),
				uiOutput(ns("subset_column_values_ui")),
				uiOutput(ns("column_cut_ui")),
				uiOutput(ns("column_order_ui")),
				uiOutput(ns("cluster_columns_ui")),
				uiOutput(ns("cluster_columns_method_ui")),
				uiOutput(ns("show_colnames_ui"))
			),
			
			# Row controls
			column(
				width = 4,
				h4("Row (Feature) Controls"),
				uiOutput(ns("row_annotations_ui")),
				uiOutput(ns("subset_row_ui")),
				uiOutput(ns("subset_row_values_ui")),
				uiOutput(ns("row_cut_ui")),
				uiOutput(ns("row_order_ui")),
				uiOutput(ns("cluster_rows_ui")),
				uiOutput(ns("cluster_rows_method_ui")),
				uiOutput(ns("show_rownames_ui"))
			),
			
			# Display controls
			column(
				width = 4,
				h4("Display Controls"),
				uiOutput(ns("size_ui")),
				uiOutput(ns("size_numeric_ui")),
				uiOutput(ns("font_size_ui")),
				uiOutput(ns("range_ui")),
				uiOutput(ns("generate_heatmap_switch_ui")),
				uiOutput(ns("heatmap_plot_name_ui"))
			)
		)
	)
}


#' Enhanced Heatmap Controls Module Server
#'
#' Server logic for comprehensive heatmap configuration. 
#'
#' @param id Character; module namespace ID
#' @param eset Reactive ExpressionSet
#' @param default_column_annotations Character vector; default pData columns
#' @param default_row_annotations Character vector; default fData columns
#' @param default_cluster_column Logical; default column clustering
#' @param default_cluster_row Logical; default row clustering
#' @param default_show_rownames Logical; default show row names
#' @param default_show_colnames Logical; default show column names
#' @param default_size Character; "fit" or "full"
#' @param debug Logical; enable debug button
#'
#' @return List of reactive expressions with all heatmap parameters
#' @export
#'
#' @note Version 2.0 - Enhanced from Heatmap_AnnotationSelectionServer pattern
#' @export
mod_heatmap_controls_enhanced_server <- function(id,
																								 eset,
																								 default_column_annotations = NULL,
																								 default_row_annotations = NULL,
																								 default_cluster_column = TRUE,
																								 default_cluster_row = TRUE,
																								 default_show_rownames = FALSE,
																								 default_show_colnames = TRUE,
																								 default_size = "fit",
																								 debug = FALSE) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# Debug handler
		observeEvent(input$debug_controls, {
			message("\n╔═══════════════════════════════════════════════════════════╗")
			message("║       🔍 DEBUG MODE - Enhanced Heatmap Controls          ║")
			message("╚═══════════════════════════════════════════════════════════╝")
			message("Column annotations: ", paste(input$column_annotations, collapse = ", "))
			message("Row annotations: ", paste(input$row_annotations, collapse = ", "))
			message("Subset column: ", input$subset_column)
			message("Subset row: ", input$subset_row)
			message("Cut column: ", input$cut_column)
			message("Cut row: ", input$cut_row)
			message("Cluster columns: ", input$cluster_columns)
			message("Cluster rows: ", input$cluster_rows)
			message("Size: ", input$size)
			message("Generate: ", input$generate_heatmap)
			browser()
		})
		
		# Column Annotations UI
		output$column_annotations_ui <- renderUI({
			req(eset())
			choices <- colnames(Biobase::pData(eset()))
			selectInput(
				ns("column_annotations"),
				"Column Annotations",
				choices = choices,
				selected = default_column_annotations,
				multiple = TRUE
			)
		})
		
		# Subset Column UI
		output$subset_column_ui <- renderUI({
			selectInput(
				ns("subset_column"),
				"Subset by Column",
				choices = c("None", "Sample", input$column_annotations),
				selected = "None"
			)
		})
		
		output$subset_column_values_ui <- renderUI({
			req(input$subset_column)
			if (input$subset_column != "None") {
				req(eset())
				choices <- Biobase::pData(eset()) %>%
					data.frame() %>%
					pull(!!sym(input$subset_column)) %>%
					unique()
				selectInput(
					ns("subset_column_values"),
					"Column Subset Values",
					choices = choices,
					selected = choices,
					multiple = TRUE
				)
			}
		})
		
		# Row Annotations UI
		output$row_annotations_ui <- renderUI({
			req(eset())
			tryCatch({
				choices <- colnames(Biobase::fData(eset()))
				selectInput(
					ns("row_annotations"),
					"Row Annotations",
					choices = choices,
					selected = default_row_annotations,
					multiple = TRUE
				)
			}, error = function(e) {
				p(em("No fData available"))
			})
		})
		
		# Subset Row UI
		output$subset_row_ui <- renderUI({
			selectInput(
				ns("subset_row"),
				"Subset by Row",
				choices = c("None", "Protein", input$row_annotations),
				selected = "None"
			)
		})
		
		output$subset_row_values_ui <- renderUI({
			req(input$subset_row)
			if (input$subset_row != "None") {
				req(eset())
				tryCatch({
					choices <- Biobase::fData(eset()) %>%
						data.frame() %>%
						pull(!! sym(input$subset_row)) %>%
						unique()
					selectInput(
						ns("subset_row_values"),
						"Row Subset Values",
						choices = choices,
						selected = choices,
						multiple = TRUE
					)
				}, error = function(e) NULL)
			}
		})
		
		# Cut Column UI
		output$column_cut_ui <- renderUI({
			req(input$column_annotations)
			selectInput(
				ns("cut_column"),
				"Cut Column",
				choices = c("None", "Sample", input$column_annotations),
				selected = "None"
			)
		})
		
		# Cut Row UI
		output$row_cut_ui <- renderUI({
			req(input$row_annotations)
			selectInput(
				ns("cut_row"),
				"Cut Row",
				choices = c("None", "Protein", input$row_annotations),
				selected = "None"
			)
		})
		
		# Order Column UI
		output$column_order_ui <- renderUI({
			req(input$column_annotations)
			selectInput(
				ns("order_column"),
				"Order Column",
				choices = c("None", "Sample", input$column_annotations),
				selected = "None"
			)
		})
		
		# Order Row UI
		output$row_order_ui <- renderUI({
			req(input$row_annotations)
			selectInput(
				ns("order_row"),
				"Order Row",
				choices = c("None", "Protein", input$row_annotations),
				selected = "None"
			)
		})
		
		# Cluster Columns UI
		output$cluster_columns_ui <- renderUI({
			checkboxInput(
				ns("cluster_columns"),
				"Cluster Columns",
				value = default_cluster_column
			)
		})
		
		output$cluster_columns_method_ui <- renderUI({
			if (isTRUE(input$cluster_columns)) {
				selectInput(
					ns("cluster_columns_method"),
					"Column Cluster Method",
					choices = c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
					selected = "euclidean"
				)
			}
		})
		
		# Cluster Rows UI
		output$cluster_rows_ui <- renderUI({
			checkboxInput(
				ns("cluster_rows"),
				"Cluster Rows",
				value = default_cluster_row
			)
		})
		
		output$cluster_rows_method_ui <- renderUI({
			if (isTRUE(input$cluster_rows)) {
				selectInput(
					ns("cluster_rows_method"),
					"Row Cluster Method",
					choices = c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
					selected = "euclidean"
				)
			}
		})
		
		# Show Names UI
		output$show_colnames_ui <- renderUI({
			checkboxInput(
				ns("show_colnames"),
				"Show Column Names",
				value = default_show_colnames
			)
		})
		
		output$show_rownames_ui <- renderUI({
			checkboxInput(
				ns("show_rownames"),
				"Show Row Names",
				value = default_show_rownames
			)
		})
		
		# Size UI
		output$size_ui <- renderUI({
			selectInput(
				ns("size"),
				"Size Mode",
				choices = c("Fit to screen" = "fit", "Full (scrollable)" = "full"),
				selected = default_size
			)
		})
		
		output$size_numeric_ui <- renderUI({
			if (! is.null(input$size) && input$size == "full") {
				numericInput(
					ns("size_numeric"),
					"Cell Height",
					value = 2.5,
					min = 0.5,
					max = 10,
					step = 0.5
				)
			}
		})
		
		output$font_size_ui <- renderUI({
			if (!is.null(input$size) && input$size == "full") {
				numericInput(
					ns("font_size"),
					"Font Size",
					value = 8,
					min = 4,
					max = 20,
					step = 1
				)
			}
		})
		
		# Range UI
		output$range_ui <- renderUI({
			req(eset())
			expr_data <- Biobase::exprs(eset())
			min_expr <- floor(min(expr_data, na.rm = TRUE))
			max_expr <- ceiling(max(expr_data, na.rm = TRUE))
			sliderInput(
				ns("range"),
				"Intensity Range",
				min = min_expr,
				max = max_expr,
				value = c(min_expr, max_expr)
			)
		})
		
		# Generate Switch UI
		output$generate_heatmap_switch_ui <- renderUI({
			switchInput(
				inputId = ns("generate_heatmap"),
				label = HTML("<strong>Generate Heatmap</strong>"),
				onLabel = "On",
				offLabel = "Off",
				value = TRUE,
				width = "100%",
				size = "normal",
				labelWidth = 100
			)
		})
		
		# Plot Name UI
		output$heatmap_plot_name_ui <- renderUI({
			textInput(
				ns("heatmap_plot_name"),
				"Save Plot Name",
				value = "Heatmap"
			)
		})
		
		# Return all reactive values
		return(list(
			column_annotations = reactive(input$column_annotations),
			subset_column = reactive(input$subset_column),
			subset_column_values = reactive(input$subset_column_values),
			row_annotations = reactive(input$row_annotations),
			subset_row = reactive(input$subset_row),
			subset_row_values = reactive(input$subset_row_values),
			cut_column = reactive(if (is.null(input$cut_column) || input$cut_column == "None") NULL else input$cut_column),
			cut_row = reactive(if (is.null(input$cut_row) || input$cut_row == "None") NULL else input$cut_row),
			order_column = reactive(if (is.null(input$order_column) || input$order_column == "None") NULL else input$order_column),
			order_row = reactive(if (is.null(input$order_row) || input$order_row == "None") NULL else input$order_row),
			cluster_columns = reactive(isTRUE(input$cluster_columns)),
			cluster_rows = reactive(isTRUE(input$cluster_rows)),
			cluster_columns_method = reactive(input$cluster_columns_method %||% "euclidean"),
			cluster_rows_method = reactive(input$cluster_rows_method %||% "euclidean"),
			show_colnames = reactive(isTRUE(input$show_colnames)),
			show_rownames = reactive(isTRUE(input$show_rownames)),
			size = reactive(input$size %||% "fit"),
			size_numeric = reactive(input$size_numeric %||% 2.5),
			font_size = reactive(input$font_size %||% 8),
			minimum_range = reactive(if (! is.null(input$range)) input$range[1] else NULL),
			maximum_range = reactive(if (!is.null(input$range)) input$range[2] else NULL),
			generate_heatmap = reactive(isTRUE(input$generate_heatmap)),
			heatmap_plot_name = reactive(input$heatmap_plot_name %||% "Heatmap")
		))
	})
}