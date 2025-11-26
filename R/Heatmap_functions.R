#' Prepare Data for Heatmap with Annotation Colors
#'
#' This function subsets and reorders a matrix and metadata for heatmap visualization,
#' assigning distinct annotation colors to selected metadata columns.
#'
#' @param m A numeric matrix (e.g., expression data), with columns as samples. 
#' @param meta A data frame containing sample metadata.  Must include a 'Sample' column.
#' @param s_cols A vector of column names (character) or indices (numeric) indicating metadata variables for annotation.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{m}{The reordered matrix with columns matching the metadata samples. }
#'   \item{meta}{The selected metadata columns (as factors) used for annotation.}
#'   \item{anno_col}{A named list of color mappings for the annotation variables.}
#' }
#' @export
#'
#' @note
#' Version 1.0 from
#' Heatmap_functions.R
#' @export
Heatmap_data_function <- function(m, meta, s_cols) {
	meta <- as.data.frame(meta)
	rownames(meta) <- meta$Sample
	# Reorder matrix columns to match metadata
	m <- m[, rownames(meta)]
	
	s_cols = s_cols[s_cols %in% colnames(meta)]
	(s_cols = sort(s_cols,decreasing = T))
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
#' @export
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
#' @param input_values A reactive expression for user-selected UI state (e.g.  heatmap view toggle).
#' @param show_rownames Logical.  Whether to show row names in the detailed heatmap. 
#' @param show_colnames Logical. Whether to show column names in the detailed heatmap. 
#' @param clustering_distance_cols A string specifying the distance metric for column clustering.
#'
#' @return No return value.  Side effect: renders heatmap via `renderPlot()`.
#' @export
#'
#' @note
#' Version 1. 0 from  
#' heatmap_functions.R
#' @export
heatmap_module_server <- function(id,
																	data_list,
																	heatmap_view = 'zoom',
																	show_rownames = FALSE,
																	show_colnames = FALSE,
																	clustering_distance_cols = "euclidean",
																	view = 'basic') {
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
				
				
				# if (heatmap_view() == "zoom") {
				# 	p <- simple
				# 	row_width <- 0.01
				# 	plot_height <- heatmap_zoom_height
				# } else {
				# 	p <- detail
				# 	row_width <- 0.15
				# 	plot_height <- plot_height_function(nrow(data$m), row_width)
				# }
				
				list(
					simple = simple,
					detail = detail,
					rows = nrow(data$m)
				)
			}
		})
		
		output$heatmap_mod_ui <- renderUI({
			heatmap_list <- heatmap_data()
			
			if (heatmap_view() == "zoom") {
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
			
			if(view() == 'basic'){
				lst = list(
					plotOutput(ns("heatmap_mod_plot"), height = plot_height) %>%
						shinycssloaders::withSpinner(type = 4, color = style_defaults$spinner_colour)
				)
			}else{
				lst = list(
					tabsetPanel(selected = "Heatmap",
											tabPanel('Overview'),
											tabPanel('Heatmap',
															 plotOutput(ns("heatmap_mod_plot"), height = plot_height) %>%
															 	shinycssloaders::withSpinner(type = 4, color = style_defaults$spinner_colour)
											)
					)
				)
			}
			do.call(tagList,lst)
		})
		return(heatmap_data)
	})
}


# ============================================================================
# NEW FUNCTIONS BELOW - ExpressionSet Support
# ============================================================================

#' Prepare Heatmap Data from ExpressionSet
#'
#' Extract and prepare data from an ExpressionSet object for heatmap visualization.
#' Supports filtering of samples and features, and extraction of annotations from
#' both pData (sample annotations) and fData (feature annotations). 
#'
#' @param eset ExpressionSet object from Biobase
#' @param assay_name Character; name of assay to extract (default "exprs")
#' @param sample_cols Character vector; pData column names for sample annotations
#' @param feature_cols Character vector; fData column names for feature annotations (row annotations)
#' @param sample_indices Integer vector; indices of samples to include (default all)
#' @param feature_indices Integer vector; indices of features to include (default all)
#'
#' @return List with elements:
#' \describe{
#'   \item{m}{Numeric matrix of expression data (features x samples)}
#'   \item{sample_meta}{Data frame of sample annotations for heatmap columns}
#'   \item{feature_meta}{Data frame of feature annotations for heatmap rows}
#'   \item{sample_anno_col}{Named list of colors for sample annotations}
#'   \item{feature_anno_col}{Named list of colors for feature annotations}
#'   \item{rows}{Number of features (rows)}
#'   \item{cols}{Number of samples (columns)}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' library(Biobase)
#' data(sample. ExpressionSet)
#' 
#' # Basic usage
#' hm_data <- heatmap_from_eset(
#'   sample.ExpressionSet,
#'   assay_name = "exprs",
#'   sample_cols = c("type", "score"),
#'   feature_cols = NULL
#' )
#' 
#' # With filtering
#' hm_data <- heatmap_from_eset(
#'   sample.ExpressionSet,
#'   assay_name = "exprs",
#'   sample_cols = c("type"),
#'   feature_cols = c("Chromosome"),
#'   sample_indices = 1:10,
#'   feature_indices = 1:50
#' )
#' }
#'
#' @note
#' Version 1.0 - Created for ExpressionSet-based heatmap module
#' Requires properly structured ExpressionSet with pData and optionally fData
#' @export
heatmap_from_eset <- function(eset,
															assay_name = "exprs",
															sample_cols = NULL,
															feature_cols = NULL,
															sample_indices = NULL,
															feature_indices = NULL) {
	
	# Validate input
	if (! inherits(eset, "ExpressionSet")) {
		stop("eset must be an ExpressionSet object")
	}
	
	# Extract expression matrix
	if (assay_name == "exprs") {
		m <- Biobase::exprs(eset)
	} else {
		# Try to get named assay data
		if (!assay_name %in% Biobase::assayDataElementNames(eset)) {
			stop(paste("Assay", assay_name, "not found in ExpressionSet"))
		}
		m <- Biobase::assayDataElement(eset, assay_name)
	}
	
	# Apply feature filtering
	if (!is.null(feature_indices)) {
		m <- m[feature_indices, , drop = FALSE]
	}
	
	# Apply sample filtering
	if (!is.null(sample_indices)) {
		m <- m[, sample_indices, drop = FALSE]
	}
	
	# Extract sample metadata (pData)
	sample_meta <- Biobase::pData(eset)
	if (! is.null(sample_indices)) {
		sample_meta <- sample_meta[sample_indices, , drop = FALSE]
	}
	
	# Filter to selected annotation columns
	if (!is.null(sample_cols) && length(sample_cols) > 0) {
		sample_cols <- sample_cols[sample_cols %in% colnames(sample_meta)]
		if (length(sample_cols) > 0) {
			sample_meta <- sample_meta[, sample_cols, drop = FALSE]
			# Convert to factors
			sample_meta[] <- lapply(sample_meta, factor)
		} else {
			sample_meta <- NULL
		}
	} else {
		sample_meta <- NULL
	}
	
	# Generate sample annotation colors
	sample_anno_col <- NULL
	if (!is.null(sample_meta) && ncol(sample_meta) > 0) {
		sample_anno_col <- color_distinct(
			meta_colors = sample_meta,
			variables = colnames(sample_meta)
		)
	}
	
	# Extract feature metadata (fData) - for row annotations
	feature_meta <- NULL
	feature_anno_col <- NULL
	
	if (!is.null(feature_cols) && length(feature_cols) > 0) {
		tryCatch({
			fdata <- Biobase::fData(eset)
			
			if (!is.null(feature_indices)) {
				fdata <- fdata[feature_indices, , drop = FALSE]
			}
			
			# Filter to selected annotation columns
			feature_cols <- feature_cols[feature_cols %in% colnames(fdata)]
			
			if (length(feature_cols) > 0) {
				feature_meta <- fdata[, feature_cols, drop = FALSE]
				rownames(feature_meta) <- rownames(m)
				
				# Convert to factors
				feature_meta[] <- lapply(feature_meta, factor)
				
				# Generate feature annotation colors
				feature_anno_col <- color_distinct(
					meta_colors = feature_meta,
					variables = colnames(feature_meta)
				)
			}
		}, error = function(e) {
			warning("Could not extract fData: ", e$message)
		})
	}
	
	return(list(
		m = m,
		sample_meta = sample_meta,
		feature_meta = feature_meta,
		sample_anno_col = sample_anno_col,
		feature_anno_col = feature_anno_col,
		rows = nrow(m),
		cols = ncol(m)
	))
}


#' Heatmap ExpressionSet Module UI
#'
#' UI module for ExpressionSet-based heatmap visualization with filtering support.
#'
#' @param id Character; module namespace ID
#'
#' @return tagList with heatmap UI elements
#' @export
#'
#' @note
#' Version 1. 0 - Created for ExpressionSet-based heatmap module
#' @export
heatmap_eset_module_ui <- function(id) {
	ns <- NS(id)
	uiOutput(ns("heatmap_eset_ui"))
}


#' Heatmap ExpressionSet Module Server
#'
#' Server module for ExpressionSet-based heatmap with multi-level filtering,
#' sample and feature annotations, and mode/feature flag support.
#'
#' @param id Character; module namespace ID
#' @param eset Reactive ExpressionSet object
#' @param assay_name Reactive character; which assay to visualize
#' @param sample_filter List from mod_data_filter_server for sample filtering
#' @param feature_filter List from mod_data_filter_server for feature filtering
#' @param sample_anno_cols Reactive character vector; pData columns to annotate
#' @param feature_anno_cols Reactive character vector; fData columns to annotate (row annotations)
#' @param mode Character or reactive; "basic" or "advanced" (default "basic")
#' @param features List or reactive; feature flags for future enhancements
#'
#' @return Reactive expression returning heatmap data list
#' @export
#'
#' @examples
#' \dontrun{
#' # In server
#' sample_filter <- mod_data_filter_server("sample_filter", eset, "pData")
#' feature_filter <- mod_data_filter_server("feature_filter", eset, "fData")
#' 
#' heatmap_eset_module_server(
#'   "heatmap",
#'   eset = reactive(my_eset),
#'   assay_name = reactive("exprs"),
#'   sample_filter = sample_filter,
#'   feature_filter = feature_filter,
#'   sample_anno_cols = reactive(c("Sample_Group", "Disease_State")),
#'   feature_anno_cols = reactive(c("Protein_Family")),
#'   mode = "basic",
#'   features = list()
#' )
#' }
#'
#' @note
#' Version 1.0 - Created for ExpressionSet-based heatmap module
#' Supports both sample (column) and feature (row) annotations
#' @export
heatmap_eset_module_server <- function(id,
																			 eset,
																			 assay_name,
																			 sample_filter,
																			 feature_filter,
																			 sample_anno_cols,
																			 feature_anno_cols,
																			 mode = "basic",
																			 features = list()) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
		
		# Normalize mode to reactive
		mode_reactive <- reactive({
			if (is.reactive(mode)) mode() else mode
		})
		
		# Normalize features to reactive
		features_reactive <- reactive({
			if (is.reactive(features)) {
				f <- features()
			} else {
				f <- features
			}
			
			# Merge with defaults (none for now, ready for future)
			default_features <- list()
			
			for (name in names(default_features)) {
				if (is.null(f[[name]])) {
					f[[name]] <- default_features[[name]]
				}
			}
			
			return(f)
		})
		
		# Prepare heatmap data from ExpressionSet
		heatmap_data <- reactive({
			req(eset())
			
			# Get filtered indices
			sample_idx <- if (! is.null(sample_filter)) {
				sample_filter$filtered_indices()
			} else {
				NULL
			}
			
			feature_idx <- if (!is.null(feature_filter)) {
				feature_filter$filtered_indices()
			} else {
				NULL
			}
			
			# Get annotation columns
			s_cols <- if (is.reactive(sample_anno_cols)) {
				sample_anno_cols()
			} else {
				sample_anno_cols
			}
			
			f_cols <- if (is.reactive(feature_anno_cols)) {
				feature_anno_cols()
			} else {
				feature_anno_cols
			}
			
			# Get assay name
			assay <- if (is.reactive(assay_name)) {
				assay_name()
			} else {
				assay_name
			}
			
			# Prepare data
			heatmap_from_eset(
				eset = eset(),
				assay_name = assay,
				sample_cols = s_cols,
				feature_cols = f_cols,
				sample_indices = sample_idx,
				feature_indices = feature_idx
			)
		})
		
		# Render heatmap UI
		output$heatmap_eset_ui <- renderUI({
			req(heatmap_data())
			
			data <- heatmap_data()
			
			if (data$rows == 0 || data$cols == 0) {
				return(div(
					class = "alert alert-warning",
					icon("exclamation-triangle"),
					strong(" No data to display"),
					p("All samples or features have been filtered out.  Adjust your filters.")
				))
			}
			
			# Calculate plot height
			row_height <- 15  # pixels per row
			min_height <- 400
			max_height <- 2000
			plot_height <- max(min_height, min(max_height, data$rows * row_height))
			
			# Build pheatmap arguments
			heatmap_args <- list(
				mat = data$m,
				cluster_cols = TRUE,
				cluster_rows = TRUE,
				show_rownames = ifelse(data$rows <= 50, TRUE, FALSE),
				show_colnames = ifelse(data$cols <= 50, TRUE, FALSE)
			)
			
			# Add sample annotations
			if (!is.null(data$sample_meta) && ncol(data$sample_meta) > 0) {
				heatmap_args$annotation_col <- data$sample_meta
				heatmap_args$annotation_colors <- data$sample_anno_col
			}
			
			# Add feature annotations (row annotations)
			if (!is.null(data$feature_meta) && ncol(data$feature_meta) > 0) {
				heatmap_args$annotation_row <- data$feature_meta
				if (!is.null(data$feature_anno_col)) {
					if (is.null(heatmap_args$annotation_colors)) {
						heatmap_args$annotation_colors <- data$feature_anno_col
					} else {
						heatmap_args$annotation_colors <- c(
							heatmap_args$annotation_colors,
							data$feature_anno_col
						)
					}
				}
			}
			
			# Render plot
			output$heatmap_eset_plot <- renderPlot({
				do.call(pheatmap::pheatmap, heatmap_args)
			}, height = plot_height)
			
			# Return UI
			tagList(
				div(
					style = "margin-bottom: 10px;",
					strong(paste0("Displaying ", data$rows, " features × ", data$cols, " samples"))
				),
				plotOutput(ns("heatmap_eset_plot"), height = plot_height)
			)
		})
		
		return(heatmap_data)
	})
}