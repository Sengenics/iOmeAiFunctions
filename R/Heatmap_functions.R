#' Prepare Data for Heatmap with Annotation Colors
#'
#' This function subsets and reorders a matrix and metadata for heatmap visualization,
#' assigning distinct annotation colors to selected metadata columns.
#'
#' @param m A numeric matrix (e.g., expression data), with columns as samples.
#' @param meta A data frame containing sample metadata. Must include a 'Sample' column.
#' @param s_cols A vector of column names (character) or indices (numeric) indicating metadata variables for annotation.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{m}{The reordered matrix with columns matching the metadata samples.}
#'   \item{meta}{The selected metadata columns (as factors) used for annotation.}
#'   \item{anno_col}{A named list of color mappings for the annotation variables.}
#' }
#' @export
#'
#' @note
#' Version 1.0 from
#' Heatmap_functions.R
Heatmap_data_function <- function(m, meta, s_cols) {
	meta <- as.data.frame(meta)
	rownames(meta) <- meta$Sample
	# Reorder matrix columns to match metadata
	m <- m[, rownames(meta)]

	# Convert selected metadata columns to factor
	meta[, s_cols] <- lapply(meta[, s_cols, drop = FALSE], factor)
	# Keep only annotation-relevant columns in meta
	meta <- meta[, s_cols, drop = FALSE]
	# Generate annotation color list
	anno_col <- color_distinct(meta_colors = meta,
														 variables = colnames(meta))


	return(list(
		m = m,
		meta = meta,
		anno_col = anno_col,
		rows = dim(m)[1]
	))
}


#' Heatmap UI Module
#'
#' This UI module renders a heatmap output container for the corresponding server-side module.
#'
#' @param id A unique string identifier for the module namespace.
#'
#' @return A `uiOutput()` element containing the heatmap plot.
#' @export
#'
#' @note
#' Version 1.0 from  
#' heatmap_functions.R
heatmap_module_ui <- function(id) {
	ns <- NS(id)
	uiOutput(ns("heatmap_mod_ui"))
}

#' Heatmap Server Module
#'
#' This server module generates interactive heatmaps with metadata annotations.
#' It supports zoomed and detailed views and allows customization of clustering and label display.
#'
#' @param id A unique string identifier for the module namespace.
#' @param data_list A reactive expression returning a list with elements: `m`, `meta`, `anno_col`, and `rows`.
#' @param input_values A reactive expression for user-selected UI state (e.g. heatmap view toggle).
#' @param show_rownames Logical. Whether to show row names in the detailed heatmap.
#' @param show_colnames Logical. Whether to show column names in the detailed heatmap.
#' @param clustering_distance_cols A string specifying the distance metric for column clustering.
#'
#' @return No return value. Side effect: renders heatmap via `renderPlot()`.
#' @export
#'
#' @note
#' Version 1.0 from  
#' heatmap_functions.R
heatmap_module_server <- function(id,
																	data_list,
																	input_values,
																	show_rownames = FALSE,
																	show_colnames = FALSE,
																	clustering_distance_cols = "euclidean") {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		heatmap_data <- reactive({
			data <- data_list()
			if (data$rows != 0) {
				# Create simplified and detailed versions of the heatmap
				simple <- pheatmap::pheatmap(data$m,
																		 annotation_col = data$meta,
																		 annotation_colors = data$anno_col,
																		 cluster_cols = TRUE,
																		 clustering_distance_cols = clustering_distance_cols,
																		 show_rownames = show_rownames,
																		 show_colnames = show_colnames)
				
				detail <- pheatmap::pheatmap(data$m,
																		 annotation_col = data$meta,
																		 annotation_colors = data$anno_col,
																		 cluster_cols = TRUE,
																		 clustering_distance_cols = clustering_distance_cols,
																		 show_rownames = TRUE,
																		 show_colnames = TRUE)
				
				if (input_values()$ncf_heatmap_view == "zoom") {
					p <- simple
					row_width <- 0.01
					plot_height <- heatmap_zoom_height
				} else {
					p <- detail
					row_width <- 0.15
					plot_height <- plot_height_function(nrow(data$m), row_width)
				}
				
				list(
					simple = simple,
					detail = detail,
					rows = nrow(data$m),
					plot_height = plot_height
				)
			}
		})
		
		output$heatmap_mod_ui <- renderUI({
			heatmap_list <- heatmap_data()
			
			if (input_values()$ncf_heatmap_view == "zoom") {
				p <- heatmap_list$simple
				row_width <- 0.01
				plot_height <- heatmap_zoom_height
			} else {
				p <- heatmap_list$detail
				row_width <- 0.15
				plot_height <- plot_height_function(heatmap_list$rows, row_width)
			}
			
			output$heatmap_mod_plot <- renderPlot({
				p
			}, height = plot_height)
			
			plotOutput(ns("heatmap_mod_plot"), height = plot_height) %>%
				shinycssloaders::withSpinner(type = 4, color = style_defaults$spinner_colour)
		})
		return(heatmap_data)
	})
}
