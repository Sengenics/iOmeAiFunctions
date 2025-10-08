#' Calculate Background and Foreground Quality Control Statistics
#'
#' Computes comprehensive quality control statistics for background (BG) and 
#' foreground (FG) signal intensities from raw microarray data. Calculates
#' distribution metrics, overlap thresholds, and identifies outlier artifacts.
#'
#' @param datCollate A list containing data collection objects with components:
#'   \itemize{
#'     \item \code{data$RawData}: Data.frame with columns Labels, Sample, Protein, BG, FG, NetI
#'     \item \code{data$Data}: Data.frame with columns Protein, data (feature/ctrl), mean
#'     \item \code{param$BG_overlap_metric}: Character specifying metric type (e.g., "Mode_sd", "Median_mad")
#'     \item \code{param$BG_FG_sd_mutiple}: Numeric multiplier for SD/MAD threshold (default 3)
#'   }
#' @param apply_log2 Logical indicating whether to apply log2 transformation. Default is TRUE
#' @param min_spot_count Integer minimum number of spots required per sample. Default is 20
#'
#' @return A list with two components:
#'   \itemize{
#'     \item \code{BGFG_stats}: Data.frame with QC statistics per sample including:
#'       mean, mode, median, SD, MAD, CV, BG_overlap threshold, pepper count, 
#'       BG overlap percentages
#'     \item \code{plot_data}: Data.frame in long format ready for density plotting
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Filters raw data to include only feature spots (excludes controls)
#'   \item Applies log2 transformation to BG, FG, and NetI values
#'   \item Calculates distribution statistics (mean, mode, median, SD, MAD)
#'   \item Computes BG overlap threshold based on specified metric:
#'     \itemize{
#'       \item Mode/Median/Mean_sd: uses standard deviation
#'       \item Mode/Median/Mean_mad: uses median absolute deviation
#'     }
#'   \item Identifies "pepper" artifacts (intensities > mode + 2*SD)
#'   \item Calculates percentage of spots with BG overlap
#' }
#'
#' BG_overlap_metric examples:
#' \itemize{
#'   \item "Mode_sd": FG_mode - (multiplier * FG_sd)
#'   \item "Median_mad": FG_median - (multiplier * FG_mad)
#'   \item "Mean_sd": FG_mean - (multiplier * FG_sd)
#' }
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats median sd mad
#' @examples
#' \dontrun{
#' # Calculate BG/FG statistics
#' qc_results <- calculate_bg_fg_stats(
#'   datCollate = datCollate,
#'   apply_log2 = TRUE,
#'   min_spot_count = 20
#' )
#'
#' # Access statistics
#' stats_df <- qc_results$BGFG_stats
#' plot_data <- qc_results$plot_data
#' }
#' Note : used to be
calculate_bg_fg_stats <- function(datCollate,
																	apply_log2 = TRUE,
																	min_spot_count = 20) {
	
	# Extract feature definitions (exclude controls)
	feature_data <- datCollate$data$Data %>%
		dplyr::select(Protein, data) %>%
		distinct()
	
	# Filter raw data to include only feature spots
	df <- datCollate$data$RawData %>%
		left_join(feature_data, by = "Protein") %>%
		filter(data != "ctrl")
	
	# Apply log2 transformation if requested
	if (apply_log2) {
		test_data <- df %>%
			mutate(
				BGFG = FG - BG,
				FG = log2(FG),
				BG = log2(BG),
				NetI = log2(NetI)
			)
	} else {
		test_data <- df %>%
			mutate(BGFG = FG - BG)
	}
	
	# Handle infinite values (from log2 of zero)
	test_data <- test_data %>%
		mutate(
			FG = ifelse(is.finite(FG), FG, NA_real_),
			BG = ifelse(is.finite(BG), BG, NA_real_),
			NetI = ifelse(is.finite(NetI), NetI, NA_real_)
		)
	
	# Select relevant columns
	test_data <- test_data %>%
		filter(!is.na(Protein)) %>%
		dplyr::select(Labels, Sample, Protein, FG, BG, NetI, BGFG)
	
	# ========== CREATE PLOT DATA EARLY ==========
	# Prepare plot data in long format (needed by BG_count_function)
	plot_data <- test_data %>%
		gather(key = key, value = value, FG:BGFG)
	# ============================================
	
	# Calculate total number of spots for percentage calculations
	spot_total <- length(unique(df$spot))
	
	# Calculate comprehensive statistics per sample
	BGFG_stats <- test_data %>%
		group_by(Sample) %>%
		mutate(count = n()) %>%
		ungroup() %>%
		filter(count > min_spot_count) %>%
		group_by(Labels, Sample) %>%
		summarise(
			count = n(),
			positive = sum(BGFG > 0, na.rm = TRUE),
			
			# Mean statistics
			FG_mean = mean(FG, na.rm = TRUE),
			BG_mean = mean(BG, na.rm = TRUE),
			NetI_mean = mean(NetI, na.rm = TRUE),
			
			# Mode statistics (using custom mode_function)
			FG_mode = mode_function(FG),
			BG_mode = mode_function(BG),
			NetI_mode = mode_function(NetI),
			
			# Median statistics
			FG_median = median(FG, na.rm = TRUE),
			BG_median = median(BG, na.rm = TRUE),
			NetI_median = median(NetI, na.rm = TRUE),
			
			# Standard deviation
			FG_sd = sd(FG, na.rm = TRUE),
			BG_sd = sd(BG, na.rm = TRUE),
			NetI_sd = sd(NetI, na.rm = TRUE),
			
			# Median absolute deviation
			FG_mad = mad(FG, na.rm = TRUE),
			BG_mad = mad(BG, na.rm = TRUE),
			NetI_mad = mad(NetI, na.rm = TRUE),
			
			# Mean BG-FG difference
			BGFG_difference = mean(BGFG, na.rm = TRUE),
			
			.groups = "drop"
		) %>%
		mutate(
			# Threshold calculations
			FG_sd_multiple = FG_mode - (datCollate$param$BG_FG_sd_mutiple * FG_sd),
			FG_mad_multiple = FG_mode - (datCollate$param$BG_FG_sd_mutiple * FG_mad),
			
			# Coefficient of variation (CV) based on mean
			BG_CV = BG_sd / BG_mean * 100,
			FG_CV = FG_sd / FG_mean * 100,
			NetI_CV = NetI_sd / NetI_mean * 100,
			
			# Coefficient of variation (CV) based on mode
			BG_CV_mode = BG_sd / BG_mode * 100,
			FG_CV_mode = FG_sd / FG_mode * 100,
			NetI_CV_mode = NetI_sd / NetI_mode * 100,
			
			# Positive signal percentage
			PositivePercentage = positive / count * 100
		)
	
	# Calculate BG overlap threshold based on specified metric
	BG_overlap_metric <- datCollate$param$BG_overlap_metric
	
	# Check if metric is NULL or missing
	if (is.null(BG_overlap_metric) || is.na(BG_overlap_metric)) {
		warning("BG_overlap_metric not specified, using 'Mode_sd' as default")
		BG_overlap_metric <- "Mode_sd"
	}
	
	sd_multiple <- datCollate$param$BG_FG_sd_mutiple
	
	# Use pattern matching to determine metric type
	if (grepl("mode.*sd", BG_overlap_metric, ignore.case = TRUE)) {
		BGFG_stats <- BGFG_stats %>%
			mutate(BG_overlap = FG_mode - (sd_multiple * FG_sd))
	} else if (grepl("median.*sd", BG_overlap_metric, ignore.case = TRUE)) {
		BGFG_stats <- BGFG_stats %>%
			mutate(BG_overlap = FG_median - (sd_multiple * FG_sd))
	} else if (grepl("mean.*sd", BG_overlap_metric, ignore.case = TRUE)) {
		BGFG_stats <- BGFG_stats %>%
			mutate(BG_overlap = FG_mean - (sd_multiple * FG_sd))
	} else if (grepl("mode.*mad", BG_overlap_metric, ignore.case = TRUE)) {
		BGFG_stats <- BGFG_stats %>%
			mutate(BG_overlap = FG_mode - (sd_multiple * FG_mad))
	} else if (grepl("median.*mad", BG_overlap_metric, ignore.case = TRUE)) {
		BGFG_stats <- BGFG_stats %>%
			mutate(BG_overlap = FG_median - (sd_multiple * FG_mad))
	} else if (grepl("mean.*mad", BG_overlap_metric, ignore.case = TRUE)) {
		BGFG_stats <- BGFG_stats %>%
			mutate(BG_overlap = FG_mean - (sd_multiple * FG_mad))
	} else {
		warning(sprintf("Unknown BG_overlap_metric: %s. Using Mode_sd as default.", BG_overlap_metric))
		BGFG_stats <- BGFG_stats %>%
			mutate(BG_overlap = FG_mode - (sd_multiple * FG_sd))
	}
	
	# Identify "pepper" artifacts (high intensity outliers)
	pepper_data <- datCollate$data$Data %>%
		filter(data == "feature") %>%
		left_join(
			BGFG_stats %>% dplyr::select(Sample, NetI_sd, NetI_mode),
			by = "Sample"
		) %>%
		filter(log2(mean) > (NetI_mode + (NetI_sd * 2))) %>%
		group_by(Sample) %>%
		summarise(Pepper_2sd = n(), .groups = "drop")
	
	# Add pepper count to stats (0 if none found)
	BGFG_stats <- BGFG_stats %>%
		left_join(pepper_data, by = "Sample") %>%
		mutate(Pepper_2sd = ifelse(is.na(Pepper_2sd), 0, Pepper_2sd))
	
	# ========== NOW CALL BG_count_function ==========
	# Calculate BG overlap percentages (now plot_data exists)
	BGFG_stats <- BG_count_function(plot_data, BGFG_stats, "FG_sd_multiple", spot_total)
	BGFG_stats <- BG_count_function(plot_data, BGFG_stats, "BG_overlap", spot_total)
	# ================================================
	
	# Return results
	list(
		BGFG_stats = BGFG_stats,
		plot_data = plot_data
	)
}

#' Create Density Plots for Background and Foreground Signal Analysis
#'
#' This function generates faceted density plots to visualize the distribution
#' of Background (BG) and Foreground (FG) signal intensities across samples.
#' It's primarily used for quality control to identify background overlap issues.
#'
#' @param datCollate A list containing data collection objects with components:
#'   \itemize{
#'     \item \code{data$RawData}: Data.frame with columns Labels, Sample, Protein, BG, FG, NetI
#'     \item \code{data$feature_df}: Data.frame with feature definitions including 'data' column
#'     \item \code{data$RFU_thresold}: Optional data.frame with RFU threshold values
#'     \item \code{data$lowIntensityThresholds}: Optional data.frame with nMAD2NetI values
#'     \item \code{param$BG_overlap_metric}: Character specifying metric ("Mean", "Median", or "Mode")
#'   }
#' @param QC A list containing quality control data with component:
#'   \itemize{
#'     \item \code{BGFG_stats}: Data.frame with BG/FG statistics (mean, median, mode, overlap, sd)
#'   }
#' @param samples Character vector of sample names to include in the plot
#' @param facet_by Character string specifying faceting variable. Default is "Sample"
#' @param xlim Numeric vector of length 2 specifying x-axis limits. Default is c(4, 16)
#' @param scale Character string for scale type: "auto", "full", or "fixed". Default is "auto"
#' @param col_num Integer specifying number of columns for faceting. Default is 3
#' @param flag_ylim Numeric value for flag y-axis limit. Default is 2
#'
#' @return A ggplot2 object showing density distributions with reference lines
#'
#' @details
#' The function creates density plots with the following reference lines:
#' \itemize{
#'   \item Black vertical lines: BG and FG statistics based on selected metric
#'   \item Red dashed line: Upper BG threshold (2SD above BG, if UpperBG column present)
#'   \item Green dashed line: Lower NetI threshold (nMAD2NetI, if available)
#' }
#'
#' The metric used for BG/FG statistics is determined by \code{datCollate$param$BG_overlap_metric}.
#' If not specified, "Mode" is used as default.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # Basic usage with datCollate and QC objects
#' plot <- create_bg_fg_density_plot(
#'   datCollate = datCollate,
#'   QC = QC,
#'   samples = c("Sample1", "Sample2", "Sample3")
#' )
#'
#' # With custom faceting and limits
#' plot <- create_bg_fg_density_plot(
#'   datCollate = datCollate,
#'   QC = QC,
#'   samples = selected_samples,
#'   facet_by = "Protein",
#'   xlim = c(5, 12),
#'   scale = "fixed"
#' )
#' }
#' Note : replaced select_density_plot_function
#' @export
create_bg_fg_density_plot <- function(datCollate,
																			QC,
																			samples,
																			facet_by = "Sample",
																			xlim = c(4, 16),
																			scale = "auto",
																			col_num = 3,
																			flag_ylim = 2) {
	
	# Extract raw data
	RawData <- datCollate$data$RawData
	
	# Define base columns to select from raw data
	raw_data_columns <- c("Labels", "Sample", "Protein", "BG", "FG", "NetI")
	
	# Check for optional UpperBG column (2SD above background)
	has_upper_bg <- "UpperBG" %in% colnames(RawData)
	if (has_upper_bg) {
		raw_data_columns <- c(raw_data_columns, "UpperBG")
	}
	
	# Join with lower intensity thresholds if available
	has_lower_neti <- "lowIntensityThresholds" %in% names(datCollate$data)
	if (has_lower_neti) {
		RawData <- RawData %>%
			left_join(
				datCollate$data$lowIntensityThresholds %>%
					dplyr::select(Sample, nMAD2NetI) %>%
					rename(lowerNetI = nMAD2NetI),
				by = "Sample"
			)
		raw_data_columns <- c(raw_data_columns, "lowerNetI")
	}
	
	# Prepare plot data: filter for selected samples and features only
	density_plot_data <- RawData %>%
		dplyr::select(all_of(raw_data_columns)) %>%
		left_join(datCollate$data$feature_df, by = "Protein") %>%
		filter(data == "feature") %>%
		dplyr::filter(Sample %in% samples)
	
	# Prepare BGFG statistics
	BGFG_stats <- QC$BGFG_stats
	
	# Add RFU threshold if available
	if (!is.null(datCollate$data$RFU_thresold)) {
		BGFG_stats <- BGFG_stats %>%
			left_join(datCollate$data$RFU_thresold, by = c("Sample", "Labels"))
	}
	
	if (!"RFU_threshold" %in% colnames(BGFG_stats)) {
		BGFG_stats$RFU_threshold <- 1
	}
	
	# Determine which metric to use
	BG_overlap_metric <- datCollate$param$BG_overlap_metric
	
	if (is.null(BG_overlap_metric) || is.na(BG_overlap_metric)) {
		BG_overlap_metric <- "Mode"
		message("BG_overlap_metric not specified, using 'Mode' as default")
	}
	
	# Extract the base metric type by pattern matching
	base_metric <- case_when(
		grepl("median", BG_overlap_metric, ignore.case = TRUE) ~ "Median",
		grepl("mode", BG_overlap_metric, ignore.case = TRUE) ~ "Mode",
		grepl("mean", BG_overlap_metric, ignore.case = TRUE) ~ "Mean",
		TRUE ~ NA_character_
	)
	
	# Validate that we found a valid metric
	if (is.na(base_metric)) {
		stop(sprintf(
			"BG_overlap_metric must contain 'mean', 'median', or 'mode'. Got: '%s'",
			BG_overlap_metric
		))
	}
	
	# Map to column names based on base metric
	metric_cols <- switch(
		base_metric,
		"Mode" = c("BG_mode", "FG_mode"),
		"Median" = c("BG_median", "FG_median"),
		"Mean" = c("BG_mean", "FG_mean")
	)
	
	# Prepare vertical line data
	density_vline_data <- BGFG_stats %>%
		dplyr::select(
			Labels, Sample,
			all_of(metric_cols),
			BG_overlap, RFU_threshold, BG_sd
		) %>%
		dplyr::filter(Sample %in% samples) %>%
		dplyr::rename(
			BG = !!sym(metric_cols[1]),
			FG = !!sym(metric_cols[2])
		) %>%
		mutate(metric = BG_overlap_metric)
	
	# Create base density plot
	density_plot <- create_density_plot(
		density_plot_data = density_plot_data,
		density_vline_data = density_vline_data,
		sample_flag_data = NULL,
		flag_ylim = flag_ylim,
		col_num = col_num,
		xlim = xlim,
		facet = facet_by,
		scale = scale
	)
	
	# Add optional reference lines
	if (has_upper_bg) {
		density_plot <- density_plot +
			geom_vline(
				aes(xintercept = log2(UpperBG)),
				data = density_plot_data,
				colour = "red",
				linetype = "dashed",
				alpha = 0.6
			)
	}
	
	if (has_lower_neti) {
		density_plot <- density_plot +
			geom_vline(
				aes(xintercept = log2(lowerNetI)),
				data = density_plot_data,
				colour = "green",
				linetype = "dashed",
				alpha = 0.6
			)
	}
	
	# Apply faceting
	density_plot +
		facet_grid(as.formula(paste(facet_by, "~ .")))
}


#' Create Density Plots for Background and Foreground Distributions
#'
#' Generates density plots showing the distribution of Background (BG), Foreground (FG),
#' and Net Intensity (NetI) values. Can be faceted by sample or show aggregated distributions.
#'
#' @param density_plot_data A data.frame containing the raw intensity data with columns:
#'   Sample, BG, FG, NetI
#' @param density_vline_data A data.frame containing summary statistics for vertical reference
#'   lines. Should include columns: Sample, BG, FG, BG_sd, BG_overlap, and optionally RFU_threshold
#' @param sample_flag_data Optional data.frame for flagged samples (currently not implemented)
#' @param flag_ylim Numeric value for y-axis limit when samples are flagged. Default is 2
#' @param col_num Integer specifying number of columns for faceting. Default is 3
#' @param xlim Numeric vector of length 2 specifying x-axis limits for log2 scale.
#'   Default is c(0, 16)
#' @param facet Character string specifying faceting variable. 
#'   Use "Sample" for individual sample plots or any other value for aggregated view.
#'   Default is "Sample"
#' @param scale Character string for scale type: "auto", "full", or "fixed".
#'   When "full" or "fixed", applies coord_cartesian with xlim. Default is "auto"
#'
#' @return A ggplot2 object showing density distributions
#'
#' @details
#' When \code{facet = "Sample"}:
#' \itemize{
#'   \item Creates separate density curves for each sample
#'   \item BG shown in red, FG in blue, NetI in green
#'   \item Dual y-axes: left for BG density, right for FG/NetI density (scaled)
#'   \item Vertical reference lines show statistics from density_vline_data
#' }
#'
#' When \code{facet != "Sample"}:
#' \itemize{
#'   \item Shows aggregated densities across all samples
#'   \item Individual samples shown as light colors, overall mean in bold
#'   \item Vertical lines show mean statistics across samples
#' }
#'
#' Reference lines:
#' \itemize{
#'   \item Thick red line (alpha 0.25): BG statistic
#'   \item Thin red line (alpha 1.0): BG + 1 standard deviation
#'   \item Thick blue line (alpha 0.25): FG statistic
#'   \item Thin blue line (alpha 1.0): BG overlap threshold
#'   \item Thick black line (alpha 0.25): RFU threshold (if provided)
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stats density

#'
#' @examples
#' \dontrun{
#' # Individual sample plots
#' plot <- create_density_plot(
#'   density_plot_data = raw_data,
#'   density_vline_data = stats_data,
#'   xlim = c(4, 12),
#'   facet = "Sample"
#' )
#'
#' # Aggregated view
#' plot <- create_density_plot(
#'   density_plot_data = raw_data,
#'   density_vline_data = stats_data,
#'   xlim = c(4, 12),
#'   facet = "All"
#' )
#' }
#' Note used to be density_plot_function
#' @export
create_density_plot <- function(density_plot_data,
																density_vline_data = NULL,
																sample_flag_data = NULL,
																flag_ylim = 2,
																col_num = 3,
																xlim = c(0, 16),
																facet = "Sample",
																scale = "auto") {
	
	# Initialize base plot
	density_plot <- ggplot(data = density_plot_data)
	
	if (facet == "Sample") {
		# Individual sample density plots with dual y-axes
		
		samples <- unique(density_plot_data$Sample)
		
		# Initialize data frames for density curves
		df_FG <- data.frame()
		df_BG <- data.frame()
		df_NetI <- data.frame()
		
		# Calculate densities for each sample
		for (entry in samples) {
			sample_data <- density_plot_data %>%
				filter(Sample == entry)
			
			# Calculate BG density
			density_BG <- density(log2(sample_data$BG))
			
			# Calculate FG density
			density_FG <- density(log2(sample_data$FG))
			
			# Calculate NetI density (handle zeros and NAs)
			sample_data$NetI[sample_data$NetI < 1 | is.na(sample_data$NetI)] <- 1
			density_NetI <- density(log2(sample_data$NetI))
			
			# Append to data frames
			df_BG <- df_BG %>%
				rbind(data.frame(
					x = density_BG$x,
					y = density_BG$y,
					Sample = entry,
					key = "BG"
				))
			
			df_FG <- df_FG %>%
				rbind(data.frame(
					x = density_FG$x,
					y = density_FG$y,
					Sample = entry,
					key = "FG"
				))
			
			df_NetI <- df_NetI %>%
				rbind(data.frame(
					x = density_NetI$x,
					y = density_NetI$y,
					Sample = entry,
					key = "NetI"
				))
		}
		
		# Calculate scale factor for dual y-axis
		# Scale FG and NetI relative to BG
		scale_factor <- max(df_BG$y, na.rm = TRUE) / max(df_FG$y, na.rm = TRUE)
		
		# Build plot with density lines and dual y-axes
		density_plot <- density_plot +
			geom_line(data = df_BG, aes(x = x, y = y, col = key)) +
			geom_line(data = df_FG, aes(x = x, y = y * scale_factor, col = key)) +
			geom_line(data = df_NetI, aes(x = x, y = y * scale_factor, col = key)) +
			scale_y_continuous(
				name = "Density (BG)",
				sec.axis = sec_axis(~ . / scale_factor, name = "Density (FG, NetI)")
			) +
			scale_color_manual(
				values = c("BG" = "red", "FG" = "blue", "NetI" = "green"),
				name = "Signal Type"
			)
		
		# Add vertical reference lines if data provided
		if (!is.null(density_vline_data)) {
			if ("BG" %in% colnames(density_vline_data)) {
				density_plot <- density_plot +
					geom_vline(
						data = density_vline_data,
						aes(xintercept = BG),
						alpha = 0.25,
						col = "red",
						linewidth = 1.5
					) +
					geom_vline(
						data = density_vline_data,
						aes(xintercept = BG + BG_sd),
						alpha = 1,
						col = "red",
						linewidth = 0.5
					) +
					geom_vline(
						data = density_vline_data,
						aes(xintercept = FG),
						alpha = 0.25,
						col = "blue",
						linewidth = 1.5
					) +
					geom_vline(
						data = density_vline_data,
						aes(xintercept = BG_overlap),
						alpha = 1,
						col = "blue",
						linewidth = 0.5
					)
			}
			
			if ("RFU_threshold" %in% colnames(density_vline_data)) {
				density_plot <- density_plot +
					geom_vline(
						data = density_vline_data,
						aes(xintercept = RFU_threshold),
						alpha = 0.25,
						col = "black",
						linewidth = 1.5
					)
			}
		}
		
	} else {
		# Aggregated view across all samples
		
		density_plot <- density_plot +
			# Individual sample densities (light colors)
			geom_density(
				data = density_plot_data,
				aes(x = log2(BG), group = Sample),
				col = "pink"
			) +
			geom_density(
				data = density_plot_data,
				aes(x = log2(FG), group = Sample),
				col = "lightskyblue1"
			) +
			# Overall densities (bold colors)
			geom_density(
				data = density_plot_data,
				aes(x = log2(BG)),
				col = "red",
				linewidth = 1.5
			) +
			geom_density(
				data = density_plot_data,
				aes(x = log2(FG)),
				col = "blue",
				linewidth = 1.5
			)
		
		# Add mean vertical reference lines if data provided
		if (!is.null(density_vline_data)) {
			density_plot <- density_plot +
				geom_vline(
					data = density_vline_data,
					aes(xintercept = mean(BG, na.rm = TRUE)),
					col = "red",
					linewidth = 1.5
				) +
				geom_vline(
					data = density_vline_data,
					aes(xintercept = mean(BG + BG_sd, na.rm = TRUE)),
					col = "red",
					linewidth = 1.5
				) +
				geom_vline(
					data = density_vline_data,
					aes(xintercept = mean(FG, na.rm = TRUE)),
					col = "blue",
					linewidth = 1.5
				) +
				geom_vline(
					data = density_vline_data,
					aes(xintercept = mean(BG_overlap, na.rm = TRUE)),
					col = "blue",
					linewidth = 1
				)
		}
	}
	
	# Apply coordinate limits if scale is 'full' or 'fixed'
	if (scale %in% c("full", "fixed")) {
		density_plot <- density_plot +
			coord_cartesian(xlim = xlim) +
			scale_x_continuous(breaks = seq(xlim[1], xlim[2], by = 1))
	} else {
		density_plot <- density_plot +
			scale_x_continuous(breaks = seq(0, 16, by = 2))
	}
	
	# Apply theme and formatting
	density_plot <- density_plot +
		geom_hline(yintercept = 0, colour = "black", linewidth = 0.25) +
		scale_fill_discrete(drop = FALSE) +
		ylab("") +
		xlab("log2 Intensity") +
		theme_minimal() +
		theme(
			axis.title = element_text(size = 14),
			plot.title = element_text(size = 14),
			axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
			legend.position = "bottom"
		)
	
	return(density_plot)
}