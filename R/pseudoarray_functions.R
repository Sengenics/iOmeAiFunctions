#' Get Single Sample Manifest Information
#'
#' Extracts manifest information for a single sample, selecting relevant columns
#' including sample identifiers, GPR file path, quality metrics, and optional remarks.
#'
#' @param sample Character string specifying the sample name
#' @param datCollate List object containing experiment data with structure:
#'   - `manifest`: Data frame with sample information
#'   - `data$RawData`: Data frame with raw GPR data
#'   - `data$Data`: Data frame with protein annotations
#' @param remark_col Character string specifying which remark column to include.
#'   Default is NULL (no remark column)
#'
#' @return A data frame (tibble) with one row containing the manifest information
#'   for the specified sample, including columns:
#'   - Sample: Sample identifier
#'   - Labels: Sample labels
#'   - GPR: Path to GPR file
#'   - Slide_ID: Slide identifier
#'   - Block: Block number
#'   - Batch_ID: Batch identifier (if present)
#'   - Assay: Assay type
#'   - Quality columns (any column with 'quality' in the name)
#'   - Remark column (if specified)
#'
#' @details
#' This function filters the manifest to a single sample and selects only
#' relevant columns. It automatically detects:
#' - Batch_ID columns (using grep)
#' - Quality-related columns (case-insensitive search for 'quality')
#' - Optional remark columns specified by the user
#'
#' @examples
#' \dontrun{
#' # Get manifest for a single sample
#' sample_info <- single_manifest_function("Sample1", datCollate)
#' 
#' # Include a specific remark column
#' sample_info <- single_manifest_function("Sample1", datCollate, 
#'                                          remark_col = "QC_Remark")
#' }
#'
#' @author DrGarnett
#' @export
single_manifest_function <- function(sample, datCollate, remark_col = NULL) { 
	
	# Extract manifest from datCollate
	manifest <- datCollate$manifest
	
	# Filter to specified sample
	manifest <- manifest %>% 
		filter(Sample == sample)
	
	# Define base columns to include
	base_cols <- c("Sample", "Labels", "GPR", "Slide_ID", "Block")
	
	# Find Batch_ID columns
	batch_cols <- grep('Batch_ID', colnames(manifest), value = TRUE)
	
	# Find quality columns (case-insensitive)
	quality_cols <- colnames(manifest)[grep('quality', tolower(colnames(manifest)))]
	
	# Add Assay if present
	assay_col <- if("Assay" %in% colnames(manifest)) "Assay" else character(0)
	
	# Combine all column names
	col_names <- c(base_cols, batch_cols, assay_col, quality_cols)
	
	# Add remark column if specified and exists
	if (!is.null(remark_col) && remark_col %in% colnames(manifest)) {
		col_names <- c(col_names, remark_col)
	}
	
	# Select only columns that exist in manifest
	col_names <- col_names[col_names %in% colnames(manifest)]
	
	# Select and return
	manifest <- manifest %>% 
		dplyr::select(one_of(col_names))
	
	return(manifest)
}


#' Generate Pseudo Array Grid for Quality Control
#'
#' Creates a set of three pseudo array plots (Background, Foreground, and Net Intensity)
#' for a single sample, suitable for quality control visualization in a grid layout.
#'
#' @param sample Character string specifying the sample name
#' @param datCollate List object containing experiment data with structure:
#'   - `manifest`: Data frame with sample information
#'   - `data$RawData`: Data frame with raw GPR data
#'   - `data$Data`: Data frame with protein annotations
#'   - `param$Wavelength`: Character string ("532" or "635") specifying wavelength
#' @param data_type Character vector specifying which data types to include.
#'   Options: c('all'), c('feature'), c('feature', 'ctrl'), etc.
#'   Default is c('all') which includes 'feature' and 'ctrl'
#' @param scale Character string specifying scale type: "RFU" for linear or
#'   "Log2" for log2-transformed. Default is "RFU"
#' @param point_size Numeric value for base point size. Default is 0.75
#' @param show_flags Logical indicating whether to show quality flags as different
#'   point shapes in the NetI plot. Default is TRUE
#' @param flagged_proteins Character vector of protein names that should be flagged
#'   in the NetI plot. Default is NULL
#' @param selected_proteins Character vector of protein names that should be highlighted
#'   in the NetI plot. Default is NULL
#' @param use_custom_scale Logical indicating whether to use custom scale limits
#'   for consistent scaling across plots. Default is FALSE
#' @param custom_min Numeric value for custom minimum scale limit. Default is NULL
#' @param custom_max Numeric value for custom maximum scale limit. Default is NULL
#'
#' @return A named list with three ggplot objects:
#'   \item{BG}{Background intensity pseudo array plot}
#'   \item{FG}{Foreground intensity pseudo array plot}
#'   \item{NetI}{Net intensity pseudo array plot (with legend removed)}
#'
#' @details
#' This function is designed for quality control dashboards where you want to
#' display BG, FG, and NetI side-by-side for comparison. All three plots use
#' the same scale type (RFU or Log2) and data type filters for consistency.
#' 
#' The NetI plot has its legend removed to save space in grid layouts, but
#' the BG and FG plots retain their legends.
#' 
#' All three plots share the same color scale limits (if use_custom_scale = TRUE)
#' to ensure comparable color intensity across the trio.
#'
#' @examples
#' \dontrun{
#' # Generate QC pseudo array grid
#' plots <- QC_pseudo_array_grid_function("Sample1", datCollate, 
#'                                         data_type = c('all'), 
#'                                         scale = "RFU")
#' 
#' # Display in grid using cowplot or patchwork
#' library(cowplot)
#' plot_grid(plots$BG, plots$FG, plots$NetI, nrow = 1)
#' 
#' # Or with patchwork
#' library(patchwork)
#' plots$BG | plots$FG | plots$NetI
#' 
#' # With log2 scale and custom limits
#' plots <- QC_pseudo_array_grid_function("Sample1", datCollate,
#'                                         data_type = c('feature'),
#'                                         scale = "Log2",
#'                                         use_custom_scale = TRUE,
#'                                         custom_min = 0,
#'                                         custom_max = 16)
#' 
#' # In Shiny with flagged proteins
#' plots <- QC_pseudo_array_grid_function(
#'   sample = input$selected_sample,
#'   datCollate = values$datCollate,
#'   data_type = input$data_type,
#'   scale = input$scale,
#'   show_flags = input$show_flags,
#'   flagged_proteins = input$flaggedProteins,
#'   selected_proteins = input$selected_protein
#' )
#' }
#'
#' @seealso 
#' \code{\link{dat_GPR_BG_single_function}} for background plots
#' \code{\link{dat_GPR_FG_single_function}} for foreground plots
#' \code{\link{dat_GPR_NetI_single_function}} for net intensity plots
#' @author DrGarnett
#' @export
QC_pseudo_array_grid_function <- function(sample, 
																					datCollate,
																					wavelength,
																					data_type = c('all'),
																					scale = 'RFU',
																					point_size = 0.75,
																					show_flags = TRUE,
																					flagged_proteins = NULL,
																					selected_proteins = NULL,
																					scale_mode = "auto_per_plot",
																					custom_min = NULL,
																					custom_max = NULL) {
	
	print('QC_pseudo_array_grid_function')
	
	# Initialize list to store plots
	pseudo_array_list <- list()
	
	# Generate Background plot
	pseudo_array_list$BG <- dat_GPR_BG_single_function(
		sample = sample,
		datCollate = datCollate,
		wavelength = wavelength,
		data_type = data_type,
		scale = scale,
		scale_mode = scale_mode,
		custom_min = custom_min,
		custom_max = custom_max
	)
	
	# Generate Foreground plot
	pseudo_array_list$FG <- dat_GPR_FG_single_function(
		sample = sample,
		datCollate = datCollate,
		wavelength = wavelength,
		data_type = data_type,
		scale = scale,
		point_size = point_size,
		scale_mode = scale_mode,
		custom_min = custom_min,
		custom_max = custom_max
	)
	
	# Generate Net Intensity plot
	pseudo_array_list$NetI <- dat_GPR_NetI_single_function(
		sample = sample,
		datCollate = datCollate,
		wavelength = wavelength,
		flagged_proteins = flagged_proteins,
		selected_proteins = selected_proteins,
		selected_data_type = data_type,
		scale = scale,
		point_size = point_size,
		scale_mode = scale_mode,
		custom_min = custom_min,
		custom_max = custom_max,
		show_flags = show_flags
	) + theme(legend.position = "none")  # Remove legend for cleaner grid display
	
	return(pseudo_array_list)
}

#' Generate Single Sample QC Grid Layout
#'
#' Creates a comprehensive quality control grid layout for a single sample,
#' including manifest information, TIFF image, quality metrics plots, density
#' plots, control probe plots, and pseudo array visualizations (BG, FG, NetI).
#'
#' @param sample Character string specifying the sample name
#' @param datCollate List object containing experiment data with structure:
#'   - `manifest`: Data frame with sample information
#'   - `data$RawData`: Data frame with raw GPR data
#'   - `data$Data`: Data frame with protein annotations
#'   - `param$Wavelength`: Character string ("532" or "635") specifying wavelength
#' @param dat List object containing GPR file data with structure:
#'   - `data$gpr[[gpr_name]]$GPR_edit`: Data frame with edited GPR data including Block info
#' @param QC List object containing QC data for density plots
#' @param image_file_path Character string specifying the path to the TIFF image file.
#'   If NULL or file doesn't exist, the grid layout will be adjusted to exclude the image
#' @param image_wavelength_index Integer specifying which wavelength/page of the TIFF 
#'   to display. Default is 1
#' @param jpg_brightness Numeric value for image brightness adjustment (0-200).
#'   Default is 100 (no change)
#' @param pseudo_scale Character string specifying scale type for pseudo arrays:
#'   "RFU" for linear or "Log2" for log2-transformed. Default is "RFU"
#' @param pseudo_data_type Character vector specifying which data types to include
#'   in pseudo arrays. Default is c('all')
#' @param show_flags Logical indicating whether to show quality flags as different
#'   point shapes in the NetI plot. Default is TRUE
#' @param flagged_proteins Character vector of protein names that should be flagged.
#'   Default is NULL
#' @param selected_proteins Character vector of protein names that should be highlighted.
#'   Default is NULL
#' @param use_custom_scale Logical indicating whether to use custom scale limits.
#'   Default is FALSE
#' @param custom_min Numeric value for custom minimum scale limit. Default is NULL
#' @param custom_max Numeric value for custom maximum scale limit. Default is NULL
#' @param point_size Numeric value for point size in pseudo arrays. Default is 0.75
#'
#' @return A grid.arrange object displaying the complete QC layout. The function
#'   creates a grid with:
#'   - Top: Image info table (if image provided) and sample manifest table
#'   - Middle: TIFF image (if provided), flagged plot, density plot, control probe plot
#'   - Bottom: Three pseudo arrays (BG, FG, NetI) side by side
#'
#' @details
#' The layout adapts based on:
#' 1. **Number of blocks**: Adjusts plot dimensions based on array block count
#'    - Single block (1): Uses larger pseudo array dimensions (1204 × 512)
#'    - Multiple blocks (>1): Scales dimensions proportionally (512 / n_blocks)
#' 
#' 2. **Image availability**: 
#'    - With image: Full layout including image display area
#'    - Without image: Condensed layout without image row
#' 
#' The function uses complex grid layouts with different proportions:
#' - With image: 6:27 ratio for tables, various ratios for plots
#' - Without image: Full-width tables and plots
#' 
#' Image processing:
#' - Loads TIFF using helper function \code{get_slide_images_function}
#' - Applies brightness adjustment using \code{magick::image_modulate}
#' - Converts to rasterGrob for grid display
#'
#' @examples
#' \dontrun{
#' # Basic usage with image
#' grid_plot <- single_grid_function(
#'   sample = "Sample1",
#'   datCollate = datCollate,
#'   dat = dat,
#'   QC = QC,
#'   image_file_path = "path/to/image.tif"
#' )
#' 
#' # Without image
#' grid_plot <- single_grid_function(
#'   sample = "Sample1",
#'   datCollate = datCollate,
#'   dat = dat,
#'   QC = QC,
#'   image_file_path = NULL
#' )
#' 
#' # With custom pseudo array settings
#' grid_plot <- single_grid_function(
#'   sample = "Sample1",
#'   datCollate = datCollate,
#'   dat = dat,
#'   QC = QC,
#'   image_file_path = "path/to/image.tif",
#'   pseudo_scale = "Log2",
#'   pseudo_data_type = c('feature'),
#'   show_flags = TRUE,
#'   jpg_brightness = 120
#' )
#' 
#' # In Shiny server
#' output$qc_grid <- renderPlot({
#'   single_grid_function(
#'     sample = input$selected_sample,
#'     datCollate = values$datCollate,
#'     dat = values$dat,
#'     QC = values$QC,
#'     image_file_path = input$image_path,
#'     image_wavelength_index = input$image_wavelength_index,
#'     jpg_brightness = input$jpg_brightness,
#'     pseudo_scale = input$single_pseudo_scale,
#'     pseudo_data_type = input$single_pseudo_data_type,
#'     show_flags = input$single_Flags,
#'     flagged_proteins = input$flaggedProteins,
#'     selected_proteins = input$single_label_protein
#'   )
#' })
#' }
#'
#' @seealso 
#' \code{\link{QC_pseudo_array_grid_function}} for pseudo array generation
#' \code{\link{single_manifest_function}} for manifest extraction
#' \code{\link{create_bg_fg_density_plot}} for density plots
#' @author DrGarnett
#' @export
single_grid_function <- function(sample, 
																 datCollate,
																 dat,
																 QC,
																 image_file_path = NULL,
																 image_wavelength_index = 1,
																 jpg_brightness = 100,
																 pseudo_scale = 'RFU',
																 pseudo_data_type = c('all'),
																 show_flags = TRUE,
																 flagged_proteins = NULL,
																 selected_proteins = NULL,
																 scale_mode = "auto_per_plot",
																 custom_min = NULL,
																 custom_max = NULL,
																 point_size = 0.75,
																 flagged_df,
																 flagged_QC_type) { 
	
	print('single_grid_function')   
	print(sample)
	print(image_file_path)
	
	# Get sample manifest
	single_manifest <- single_manifest_function(sample, datCollate)
	
	gpr <- single_manifest %>% 
		pull(GPR)
	
	# Get GPR data and determine block numbers
	df <- dat$data$gpr[[gpr]]$GPR_edit
	df$Block <- as.character(df$Block)
	block_numbers <- length(unique(df$Block))
	
	# Set layout dimensions based on number of blocks
	if(block_numbers != 1){
		full_pseudo_length <- 512
		pseudo_length <- full_pseudo_length / block_numbers / 5
		plot_length <- full_pseudo_length / block_numbers / 4
		table_height <- 8
		nrow <- c(33, 27)
	} else {
		pseudo_length <- 1204
		plot_length <- 512
		table_height <- 64
		nrow <- c(33, 27)
	}
	
	# Try to load and process TIFF image
	jpgImgU <- tryCatch({
		tif_sample_list <- list()
		tif_sample_list[[sample]] <- image_file_path
		jpg_file_path <- image_file_path
		
		# Create image info table
		image_info <- data.frame(
			'image_file' = basename(jpg_file_path),
			'brightness' = jpg_brightness
		)
		
		# Get image from TIFF file
		page <- image_wavelength_index
		jpg_image <- get_slide_images_function(
			values = list(dat = dat),  # Create minimal values structure
			input = list(image_wavelength_index = page),
			tif_sample_list,
			sample
		)$image_list[[1]]
		
		# Apply brightness adjustment
		edited_jpg_image <- image_modulate(
			jpg_image,
			brightness = jpg_brightness,
			saturation = 100,
			hue = 100
		)
		
		rasterGrob(edited_jpg_image)
	},
	error = function(cond){
		warning("Could not load image: ", image_file_path, "\n", cond$message)
		NULL
	})
	
	# Generate pseudo array plots
	pseudo_array <- QC_pseudo_array_grid_function(
		sample = sample,
		datCollate = datCollate,
		wavelength = dat$param$Wavelength,
		data_type = pseudo_data_type,
		scale = pseudo_scale,
		point_size = point_size,
		show_flags = show_flags,
		flagged_proteins = flagged_proteins,
		selected_proteins = selected_proteins,
		scale_mode = scale_mode,
		custom_min = custom_min,
		custom_max = custom_max
	)
	
	# Create grid layout based on whether image is available
	if(!is.null(jpgImgU)){	
		# Layout WITH image
		hlay <- t(matrix(
			c(
				rep(c(rep(1, 6), rep(2, 27)), table_height),         # Image info table & manifest
				rep(c(rep(3, 6), rep(4, 13), rep(5, 7), rep(6, 7)), plot_length),  # Image & plots
				rep(c(rep(3, 6), rep(7, 27)), pseudo_length)         # Image continues & pseudo arrays
			),
			nrow = nrow[1]
		))
		
		# Create image info table
		image_info_table <- data.frame(
			'image_file' = basename(image_file_path),
			'brightness' = jpg_brightness,
			check.names = FALSE
		)
		
		gs <- list(
			tableGrob(image_info_table, rows = NULL),
			tableGrob(single_manifest, rows = NULL),
			jpgImgU,
			#single_flagged_plot_function(sample, datCollate),
			single_flagged_plot_function(sample,
																	 flagged_df,
																	 flagged_QC_type),
			create_bg_fg_density_plot(datCollate, QC, sample) +
				coord_cartesian(xlim = c(4, 12)) + 
				theme(legend.position = 'top'),
			select_ctrl_probe_plot_function(sample, datCollate),
			gridExtra::grid.arrange(grobs = pseudo_array, ncol = 3)
		)
	} else {
		# Layout WITHOUT image
		hlay <- t(matrix(
			c(
				rep(c(rep(1, 27)), 256),                              # Manifest table
				rep(c(rep(2, 13), rep(3, 7), rep(4, 7)), plot_length * 4),  # Plots
				rep(c(rep(5, 27)), pseudo_length * 4)                 # Pseudo arrays
			),
			nrow = nrow[2]
		))
		
		gs <- list(
			tableGrob(single_manifest, rows = NULL),
			single_flagged_plot_function(sample,
																	 flagged_df,
																	 flagged_QC_type),
			create_bg_fg_density_plot(datCollate, QC, sample) + 
				theme(legend.position = 'top'),
			select_ctrl_probe_plot_function(sample, datCollate),
			gridExtra::grid.arrange(grobs = pseudo_array, ncol = 3)
		)
	}
	
	# Arrange and return grid
	grid.arrange(grobs = gs, layout_matrix = hlay)
}

#' Get Pseudo Array Color Scale Limits with Multiple Modes
#'
#' Calculates appropriate color scale limits for pseudo array plots with support
#' for multiple scaling strategies: auto-scale per plot, global auto-scale, or
#' explicit custom limits.
#'
#' @param sample Character string specifying the sample name to analyze
#' @param datCollate List object containing experiment data
#' @param scale_type Character string specifying scale type: "RFU" or "Log2"
#' @param scale_mode Character string specifying scaling strategy:
#'   - "auto_per_plot": No limits (ggplot2 auto-scales each plot independently) - ORIGINAL BEHAVIOR
#'   - "auto_global": Calculate global min/max from all data, apply to all plots
#'   - "custom": Use user-specified custom_min and custom_max
#'   Default is "auto_per_plot"
#' @param custom_min Numeric value for custom minimum scale limit. Only used if
#'   scale_mode = "custom". Default is NULL
#' @param custom_max Numeric value for custom maximum scale limit. Only used if
#'   scale_mode = "custom". Default is NULL
#'
#' @return A list with four elements:
#'   \item{min}{Minimum value for the color scale (NULL for auto_per_plot)}
#'   \item{max}{Maximum value for the color scale (NULL for auto_per_plot)}
#'   \item{type}{The scale type used ("RFU" or "Log2")}
#'   \item{mode}{The scale mode used}
#'
#' @details
#' **Scaling Modes:**
#' 
#' 1. **auto_per_plot** (ORIGINAL - Maximum contrast per plot):
#'    - Returns NULL for min/max
#'    - ggplot2 auto-scales each plot to its own data range
#'    - Pros: Maximum detail/contrast in each individual plot
#'    - Cons: Colors NOT comparable between plots
#'    - Use when: Examining single samples in isolation
#' 
#' 2. **auto_global** (Consistent auto-scaling):
#'    - Calculates min/max from all data for this sample
#'    - Same scale applied to BG, FG, and NetI plots
#'    - Pros: Colors comparable within a sample's trio of plots
#'    - Cons: May lose some detail if ranges differ greatly
#'    - Use when: Comparing BG/FG/NetI within one sample
#' 
#' 3. **custom** (Match TIFF viewer):
#'    - Uses explicit user-provided min/max
#'    - Pros: Perfect match to TIFF image viewer colors
#'    - Cons: Requires knowing appropriate limits
#'    - Use when: Comparing pseudo arrays to TIFF images
#'
#' @author DrGarnett
#' @export
get_pseudo_array_scale <- function(sample, 
																	 datCollate,
																	 scale_type = "RFU", 
																	 scale_mode = c("auto_per_plot", "auto_global", "custom"),
																	 custom_min = NULL, 
																	 custom_max = NULL) {
	
	# Match scale_mode argument
	scale_mode <- match.arg(scale_mode)
	
	# For auto_per_plot, return NULL limits (original behavior)
	if (scale_mode == "auto_per_plot") {
		return(list(
			min = NULL,
			max = NULL,
			type = scale_type,
			mode = scale_mode
		))
	}
	
	# For custom mode, use provided limits
	if (scale_mode == "custom") {
		if (is.null(custom_min) || is.null(custom_max)) {
			stop("custom_min and custom_max must be provided when scale_mode = 'custom'")
		}
		return(list(
			min = custom_min,
			max = custom_max,
			type = scale_type,
			mode = scale_mode
		))
	}
	
	# For auto_global mode, calculate from data
	raw_data <- datCollate$data$RawData
	protein_data <- datCollate$data$Data
	
	# Get the raw data for this sample
	feature_df <- protein_data %>% 
		dplyr::select(Protein, data) %>% 
		distinct()
	
	df <- raw_data %>% 
		filter(Sample == sample) %>% 
		left_join(feature_df, by = "Protein")
	
	# Calculate scale limits based on type
	if (scale_type == "RFU") {
		# Linear scale
		scale_min <- 0
		scale_max <- max(c(df$FG, df$BG, df$NetI), na.rm = TRUE)
		# Round up to nearest nice number
		scale_max <- ceiling(scale_max / 1000) * 1000
	} else {
		# Log2 scale
		# Remove zeros before log transform
		vals <- c(df$FG[df$FG > 0], df$BG[df$BG > 0], df$NetI[df$NetI > 0])
		scale_min <- floor(min(log2(vals), na.rm = TRUE))
		scale_max <- ceiling(max(log2(vals), na.rm = TRUE))
	}
	
	list(
		min = scale_min,
		max = scale_max,
		type = scale_type,
		mode = scale_mode
	)
}


#' Plot Foreground Intensity Pseudo Array
#'
#' Creates a pseudo array visualization of foreground (FG) intensity values
#' from GPR data. Supports both linear (RFU) and log2-transformed scales
#' with customizable color scale limits.
#'
#' @param sample Character string specifying the sample name
#' @param datCollate List object containing experiment data with structure:
#'   - `manifest`: Data frame with sample information
#'   - `data$RawData`: Data frame with raw GPR data (columns: Sample, X, Y, FG, Block, spot, Protein)
#'   - `data$Data`: Data frame with protein annotations (columns: Protein, data)
#'   - `param$Wavelength`: Character string ("532" or "635") specifying wavelength
#' @param data_type Character vector specifying which data types to include.
#'   Default is c('all') which includes 'feature' and 'ctrl'
#' @param scale Character string specifying scale type: "RFU" for linear or
#'   "Log2" for log2-transformed. Default is "RFU"
#' @param point_size Numeric value for point size. Default is 0.75
#' @param use_custom_scale Logical indicating whether to use custom scale limits.
#'   Default is FALSE
#' @param custom_min Numeric value for custom minimum scale limit. Default is NULL
#' @param custom_max Numeric value for custom maximum scale limit. Default is NULL
#'
#' @return A ggplot object showing the foreground intensity pseudo array with:
#'   - X and Y positions from GPR file
#'   - Color gradient from black to wavelength-appropriate color (green for 532nm, red for 635nm)
#'   - Black background theme
#'   - Reversed y-axis to match array orientation
#'
#' @details
#' The function:
#' - Retrieves GPR data for the specified sample
#' - Filters by data type (feature/ctrl)
#' - Applies appropriate color scale (green for 532nm, red for 635nm wavelength)
#' - Uses explicit scale limits for consistency across multiple plots
#' - Handles log2 transformation by removing zero values
#'
#' @examples
#' \dontrun{
#' # Basic FG plot with auto-detected scale
#' p <- dat_GPR_FG_single_function("Sample1", datCollate)
#' 
#' # FG plot with log2 scale
#' p <- dat_GPR_FG_single_function("Sample1", datCollate, scale = "Log2")
#' 
#' # FG plot with custom scale to match TIFF images
#' p <- dat_GPR_FG_single_function("Sample1", datCollate,
#'                                  scale = "RFU",
#'                                  use_custom_scale = TRUE,
#'                                  custom_min = 0, 
#'                                  custom_max = 65535)
#' }
#'
#' @seealso \code{\link{dat_GPR_BG_single_function}}, \code{\link{dat_GPR_NetI_single_function}}
#' @author DrGarnett
#' @export
dat_GPR_FG_single_function <- function(sample, datCollate,
																			 wavelength = wavelength,
																			 data_type = c('all'), 
																			 scale = 'RFU', 
																			 point_size = 0.75, 
																			 scale_mode = "auto_per_plot",
																			 custom_min = NULL, 
																			 custom_max = NULL) { 
	
	if(data_type == 'all'){
		data_type = c('feature','ctrl')
	}
	
	# Extract data from datCollate
	raw_data <- datCollate$data$RawData
	protein_data <- datCollate$data$Data
	#wavelength <- dat$param$Wavelength
	
	# Get sample info
	gpr <- single_manifest_function(sample, datCollate) %>% pull(GPR)
	
	# Prepare feature data
	feature_df <- protein_data %>% 
		dplyr::select(Protein, data) %>% 
		distinct()
	
	# Get raw data for this sample
	df <- raw_data %>% 
		filter(Sample == sample) %>% 
		left_join(feature_df, by = "Protein") %>% 
		filter(data %in% data_type)
	
	# Get scale limits
	scale_limits <- get_pseudo_array_scale(sample, datCollate,
																				 scale, scale_mode, custom_min, custom_max)
	
	# Determine spot color based on wavelength
	spot_col <- if(wavelength == '532') 'green' else 'red'
	
	# Create plot based on scale type
	if(scale == 'RFU'){
		p <- ggplot(df, aes(x = X, y = Y, col = FG, group = spot, label = Block)) + 
			geom_point(size = point_size) + 
			ggtitle('Foreground') +
			scale_colour_gradient(
				low = 'black', 
				high = spot_col,
				limits = c(scale_limits$min, scale_limits$max),
				oob = scales::squish,
				name = "FG (RFU)"
			)
	} else {
		df <- df %>% filter(FG > 0)  # Remove zeros for log transform
		
		p <- ggplot(df, aes(x = X, y = Y, col = log2(FG), group = spot, label = Block)) + 
			geom_point(size = point_size) + 
			ggtitle('Foreground (Log2)') +
			scale_colour_gradient(
				low = 'black', 
				high = spot_col,
				limits = c(scale_limits$min, scale_limits$max),
				oob = scales::squish,
				name = "Log2(FG)"
			)
	}
	
	# Apply theme
	p <- p + theme(
		plot.background = element_rect(fill = "black"), 
		panel.background = element_rect(fill = 'black'),
		panel.grid.major = element_line(colour = "black"), 
		panel.grid.minor = element_line(colour = "black"),
		axis.ticks.x = element_blank(),
		axis.text.x = element_blank(),
		axis.ticks.y = element_blank(),
		axis.text.y = element_blank(),
		plot.margin = unit(c(0, 0, 0, 0), "cm")
	) + 
		scale_y_reverse()
	
	return(p)
}


#' Plot Background Intensity Pseudo Array
#'
#' Creates a pseudo array visualization of background (BG) intensity values
#' from GPR data. Supports both linear (RFU) and log2-transformed scales
#' with customizable color scale limits.
#'
#' @param sample Character string specifying the sample name
#' @param datCollate List object containing experiment data with structure:
#'   - `manifest`: Data frame with sample information
#'   - `data$RawData`: Data frame with raw GPR data (columns: Sample, X, Y, BG, Block, spot, Protein)
#'   - `data$Data`: Data frame with protein annotations (columns: Protein, data)
#'   - `param$Wavelength`: Character string ("532" or "635") specifying wavelength
#' @param data_type Character vector specifying which data types to include.
#'   Default is c('all') which includes 'feature' and 'ctrl'
#' @param scale Character string specifying scale type: "RFU" for linear or
#'   "Log2" for log2-transformed. Default is "RFU"
#' @param use_custom_scale Logical indicating whether to use custom scale limits.
#'   Default is FALSE
#' @param custom_min Numeric value for custom minimum scale limit. Default is NULL
#' @param custom_max Numeric value for custom maximum scale limit. Default is NULL
#'
#' @return A ggplot object showing the background intensity pseudo array with:
#'   - X and Y positions from GPR file
#'   - Color gradient from black to wavelength-appropriate color
#'   - Different point shapes for RFU (squares) vs Log2 (diamonds)
#'   - Black background theme
#'   - Reversed y-axis to match array orientation
#'
#' @details
#' The function:
#' - Uses square points (shape = 22) for RFU scale
#' - Uses diamond points (shape = 5) for Log2 scale
#' - Applies appropriate color scale based on wavelength
#' - Uses explicit scale limits for consistency
#' - Filters out zero values when using log2 transformation
#'
#' @examples
#' \dontrun{
#' # Basic BG plot
#' p <- dat_GPR_BG_single_function("Sample1", datCollate)
#' 
#' # BG plot with log2 scale
#' p <- dat_GPR_BG_single_function("Sample1", datCollate, scale = "Log2")
#' 
#' # BG plot with custom scale
#' p <- dat_GPR_BG_single_function("Sample1", datCollate,
#'                                  use_custom_scale = TRUE,
#'                                  custom_min = 0, 
#'                                  custom_max = 10000)
#' }
#'
#' @seealso \code{\link{dat_GPR_FG_single_function}}, \code{\link{dat_GPR_NetI_single_function}}
#' @author DrGarnett
#' @export
dat_GPR_BG_single_function <- function(sample, datCollate,
																			 wavelength = wavelength,
																			 data_type = c('all'), 
																			 scale = 'RFU',
																			 scale_mode = "auto_per_plot",
																			 custom_min = NULL, 
																			 custom_max = NULL) {
	
	if(data_type == 'all'){
		data_type = c('feature','ctrl')
	}
	
	# Extract data from datCollate
	raw_data <- datCollate$data$RawData
	protein_data <- datCollate$data$Data
	#wavelength <- datCollate$param$Wavelength
	
	# Get sample info
	gpr <- single_manifest_function(sample, datCollate) %>% pull(GPR)
	
	# Prepare feature data
	feature_df <- protein_data %>% 
		dplyr::select(Protein, data) %>% 
		distinct()
	
	# Get raw data for this sample
	df <- raw_data %>% 
		filter(Sample == sample) %>% 
		left_join(feature_df, by = "Protein") %>% 
		filter(data %in% data_type)
	
	# Get scale limits
	scale_limits <- get_pseudo_array_scale(sample, datCollate,
																				 scale, scale_mode, custom_min, custom_max)
	
	# Determine spot color based on wavelength
	spot_col <- if(wavelength == '532') 'green' else 'red'
	
	# Create plot based on scale type
	if(scale == 'RFU'){
		p <- ggplot(df, aes(x = X, y = Y, col = BG, group = spot, label = Block)) + 
			geom_point(size = 1, shape = 22) + 
			ggtitle('Background') +
			scale_colour_gradient(
				low = 'black', 
				high = spot_col,
				limits = c(scale_limits$min, scale_limits$max),
				oob = scales::squish,
				name = "BG (RFU)"
			)
	} else {
		df <- df %>% filter(BG > 0)  # Remove zeros for log transform
		
		p <- ggplot(df, aes(x = X, y = Y, col = log2(BG), group = spot, label = Block)) + 
			geom_point(size = 1.2, shape = 5) + 
			ggtitle('Background (Log2)') +
			scale_colour_gradient(
				low = 'black', 
				high = spot_col,
				limits = c(scale_limits$min, scale_limits$max),
				oob = scales::squish,
				name = "Log2(BG)"
			)
	}
	
	# Apply theme
	p <- p + theme(
		plot.background = element_rect(fill = "black"), 
		panel.background = element_rect(fill = 'black'),
		panel.grid.major = element_line(colour = "black"), 
		panel.grid.minor = element_line(colour = "black"),
		axis.ticks.x = element_blank(),
		axis.text.x = element_blank(),
		axis.ticks.y = element_blank(),
		axis.text.y = element_blank(),
		plot.margin = unit(c(0, 0, 0, 0), "cm")
	) + 
		scale_y_reverse()
	
	return(p)
}


#' Plot Net Intensity Pseudo Array with Quality Flags
#'
#' Creates a comprehensive pseudo array visualization of net intensity (NetI) values
#' with quality flags, spot categories, and customizable scaling. This is the most
#' feature-rich pseudo array plotting function.
#'
#' @param sample Character string specifying the sample name
#' @param datCollate List object containing experiment data with structure:
#'   - `manifest`: Data frame with sample information
#'   - `data$RawData`: Data frame with raw GPR data (columns: Sample, X, Y, NetI, 
#'     Block, Row, Column, spot, Flags, Dia., Protein)
#'   - `data$Data`: Data frame with protein annotations and flags (columns:
#'     Sample, Protein, data, flag, num_test)
#'   - `param$Wavelength`: Character string ("532" or "635") specifying wavelength
#' @param flagged_proteins Character vector of protein names that should be flagged.
#'   Default is NULL
#' @param selected_proteins Character vector of protein names that should be highlighted.
#'   Default is NULL
#' @param selected_data_type Character string specifying data types to display:
#'   - "all": includes feature, ctrl, failed, flagged, selected
#'   - "feature": includes feature, failed, flagged, selected (excludes ctrl)
#'   Default is "all"
#' @param scale Character string specifying scale type: "RFU" for linear or
#'   "Log2" for log2-transformed. Default is "RFU"
#' @param point_size Numeric value for base point size. Actual size is scaled
#'   by spot diameter if available. Default is 0.75
#' @param use_custom_scale Logical indicating whether to use custom scale limits.
#'   Default is FALSE
#' @param custom_min Numeric value for custom minimum scale limit. Default is NULL
#' @param custom_max Numeric value for custom maximum scale limit. Default is NULL
#' @param show_flags Logical indicating whether to show quality flags as different
#'   point shapes. Default is TRUE
#'
#' @return A ggplot object showing the net intensity pseudo array with:
#'   - Fill color representing NetI values (black to wavelength color gradient)
#'   - Point shapes indicating quality flags (if show_flags = TRUE)
#'   - Point colors indicating spot categories (feature, ctrl, flagged, etc.)
#'   - Size scaled by spot diameter
#'   - Non-feature spots shown as hollow squares
#'   - Black background theme
#'   - Reversed y-axis to match array orientation
#'
#' @details
#' Quality flag shapes (when show_flags = TRUE):
#' - Circle (16): Good spots (Flags = 0)
#' - Triangle (17): Poor morphology (Flags = -25)
#' - Diamond (18): High background (Flags = -50)
#' - Square (15): Low signal-to-noise (Flags = -75)
#' - Asterisk (8): Not found (Flags = -100)
#' - X (4): Other flags
#' 
#' Spot category colors (outline):
#' - Gray: Pass
#' - Blue: Outliers
#' - Orange: Filtered/Negative spots
#' - Black: Features
#' - Wavelength color: Controls
#' - White: Selected spots
#' 
#' The function automatically:
#' - Scales point size by spot diameter (Dia. column) if available
#' - Merges protein-level and spot-level flags
#' - Handles flagged and selected proteins
#' - Filters zero/negative values for log2 scale
#'
#' @examples
#' \dontrun{
#' # Basic NetI plot with auto-scaling
#' p <- dat_GPR_NetI_single_function("Sample1", datCollate)
#' 
#' # NetI plot with log2 scale, no flag shapes
#' p <- dat_GPR_NetI_single_function("Sample1", datCollate,
#'                                    scale = "Log2", 
#'                                    show_flags = FALSE)
#' 
#' # NetI plot with custom scale matching TIFF images
#' p <- dat_GPR_NetI_single_function("Sample1", datCollate,
#'                                    use_custom_scale = TRUE,
#'                                    custom_min = 0, 
#'                                    custom_max = 65535)
#' 
#' # Feature-only NetI plot with flagged proteins
#' p <- dat_GPR_NetI_single_function("Sample1", datCollate,
#'                                    selected_data_type = "feature",
#'                                    flagged_proteins = c("Protein1", "Protein2"))
#' }
#'
#' @seealso \code{\link{dat_GPR_FG_single_function}}, \code{\link{dat_GPR_BG_single_function}}
#' @author DrGarnett
#' @export
dat_GPR_NetI_single_function <- function(sample, datCollate,
																				 wavelength = wavelength,
																				 flagged_proteins = NULL,
																				 selected_proteins = NULL,
																				 selected_data_type = 'all', 
																				 scale = 'RFU', 
																				 point_size = 0.75,
																				 scale_mode = "auto_per_plot",
																				 custom_min = NULL,
																				 custom_max = NULL,
																				 show_flags = TRUE) {
	
	# Set data type filters
	if (selected_data_type == 'all') {
		data_type = c('feature', 'ctrl', 'failed', 'flagged', 'selected')
	} else if (selected_data_type == 'feature') {
		data_type = c('feature', 'failed', 'flagged', 'selected')
	}
	
	# Extract data from datCollate
	raw_data <- datCollate$data$RawData
	protein_data <- datCollate$data$Data
	#wavelength <- datCollate$param$Wavelength
	
	# Get sample info
	gpr <- single_manifest_function(sample, datCollate) %>% pull(GPR)
	
	# Prepare feature data with flagged and selected proteins
	feature_df <- protein_data %>%
		dplyr::select(Protein, data) %>%
		distinct()
	
	# Apply flagged proteins if provided
	if (!is.null(flagged_proteins)) {
		feature_df <- feature_df %>%
			mutate(data = ifelse(Protein %in% flagged_proteins,
													 ifelse(data != 'failed', 'flagged', data),
													 data))
	}
	
	# Apply selected proteins if provided
	if (!is.null(selected_proteins)) {
		feature_df <- feature_df %>%
			mutate(data = ifelse(Protein %in% selected_proteins, 'selected', data))
	}
	
	# Get protein-level flags
	Data_flag <- protein_data %>%
		dplyr::select(Sample, Protein, flag, num_test) %>%
		filter(is.na(num_test)) %>%
		rename(Protein_flag = flag)
	
	# Merge data
	df <- raw_data %>%
		filter(Sample == sample) %>%
		left_join(Data_flag, by = c("Sample", "Protein")) %>%
		left_join(feature_df, by = "Protein") %>%
		filter(data %in% data_type) %>%
		mutate(position = paste(Block, Row, Column)) %>%
		mutate(flag = ifelse(is.na(Protein_flag), flag, Protein_flag))
	
	df$flag[is.na(df$flag)] = 'pass'
	
	# Get scale limits
	scale_limits <- get_pseudo_array_scale(sample, datCollate,
																				 scale, scale_mode, custom_min, custom_max)
	
	# Scale spot size by Dia.
	default_point_size <- 220
	if ('Dia.' %in% colnames(df)) {
		df <- df %>%
			mutate(point_scale = round(point_size * Dia. / default_point_size, 2))
	} else {
		df$point_scale <- point_size
	}
	
	# Map flags to shapes
	df$point_shape <- factor(case_when(
		df$Flags == 0     ~ "Good (0)",
		df$Flags == -25   ~ "Poor Morphology (-25)",
		df$Flags == -50   ~ "High Background (-50)",
		df$Flags == -75   ~ "Low S/N (-75)",
		df$Flags == -100  ~ "Not Found (-100)",
		TRUE              ~ "Other"
	))
	
	# Set wavelength-dependent colors
	if(wavelength == '532'){
		spot_col <- 'green'
		alt_col <- 'red'
		flagged_col <- 'orange'
	} else {
		spot_col <- 'red'
		alt_col <- 'green'
		flagged_col <- 'blue'
	}
	
	# Start plot
	p <- ggplot(data = df)
	
	# Add shape scale if showing flags
	if(show_flags) {
		p <- p + scale_shape_manual(
			name = "Spot Quality (Flags)",
			values = c(
				"Good (0)" = 16,
				"Poor Morphology (-25)" = 17,
				"High Background (-50)" = 18,
				"Low S/N (-75)" = 15,
				"Not Found (-100)" = 8,
				"Other" = 4
			)
		)
	}
	
	# Non-feature spots as squares
	p <- p + 
		geom_point(data = df %>% filter(data != 'feature'), 
							 aes(x = X, y = Y, group = spot, label = position, col = data, size = point_scale),
							 shape = 0) + 
		scale_size_identity()
	
	# Main feature spots with fill
	if(scale == 'RFU'){
		if(show_flags){
			p <- p +
				geom_point(data = df, 
									 aes(x = X, y = Y, fill = NetI, group = spot, label = position, col = flag,
									 		size = point_scale, shape = factor(point_shape))) +
				scale_fill_gradient(
					low = 'black', 
					high = spot_col,
					limits = c(scale_limits$min, scale_limits$max),
					oob = scales::squish,
					name = "NetI (RFU)"
				)
		} else {
			p <- p +
				geom_point(data = df, 
									 aes(x = X, y = Y, fill = NetI, group = spot, label = position, col = flag,
									 		size = point_scale), 
									 shape = 16) +
				scale_fill_gradient(
					low = 'black', 
					high = spot_col,
					limits = c(scale_limits$min, scale_limits$max),
					oob = scales::squish,
					name = "NetI (RFU)"
				)
		}
	} else {
		# Log2 scale - filter out zeros/negatives
		df <- df %>% filter(NetI > 0)
		
		if(show_flags){
			p <- p +
				geom_point(data = df, 
									 aes(x = X, y = Y, fill = log2(NetI), group = spot, label = position, col = flag,
									 		size = point_scale, shape = factor(point_shape))) +
				scale_fill_gradient(
					low = 'black', 
					high = spot_col,
					limits = c(scale_limits$min, scale_limits$max),
					oob = scales::squish,
					name = "Log2(NetI)"
				)
		} else {
			p <- p +
				geom_point(data = df, 
									 aes(x = X, y = Y, fill = log2(NetI), group = spot, label = position, col = flag,
									 		size = point_scale), 
									 shape = 16) +
				scale_fill_gradient(
					low = 'black', 
					high = spot_col,
					limits = c(scale_limits$min, scale_limits$max),
					oob = scales::squish,
					name = "Log2(NetI)"
				)
		}
	}
	
	# Apply theme
	p <- p + theme(
		plot.background = element_rect(fill = "black"), 
		panel.background = element_rect(fill = 'black'),
		panel.grid.major = element_line(colour = "black"), 
		panel.grid.minor = element_line(colour = "black"),
		axis.ticks.x = element_blank(),
		axis.text.x = element_blank(),
		axis.ticks.y = element_blank(),
		axis.text.y = element_blank(),
		plot.margin = unit(c(0, 0, 0, 0), "cm")
	) + 
		scale_y_reverse()
	
	# Color scale for spot category annotations
	p <- p + scale_color_manual(
		breaks = c("pass", "Outlier 1", "Outlier 2", "Spot Filtered", "Negative",
							 "feature", "ctrl", "failed", "flagged", "selected", "OneRep",
							 "HighCV", "Filtered", "low RFU"),
		values = c(
			"gray25", "blue", "blue", "orange1", "orange2", "black", spot_col,
			paste0(alt_col, 1), flagged_col, "white", paste0(alt_col, 2),
			paste0(alt_col, 3), paste0(alt_col, 4), "orange3"
		)
	)
	
	return(p)
}