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
#' @param overlap_percentile Numeric, percentile to use for overlap calculation (default 95)
#'
#' @return A list with two components:
#'   \itemize{
#'     \item \code{BGFG_stats}: Data.frame with QC statistics per sample
#'     \item \code{plot_data}: Data.frame in long format ready for density plotting
#'   }
#'
#' @note Uses "pass"/"flag" convention for QC status
#' @author Shaun Garnett
#' Calculate Background and Foreground Quality Control Statistics
#'
#' Recapitulates the original raw_density_data_o_function logic with new v2.0.0 metrics added.
#'
#' @param datCollate A list containing data collection objects
#' @param apply_log2 Logical indicating whether to apply log2 transformation. Default is TRUE
#' @param min_spot_count Integer minimum number of spots required per sample. Default is 20
#' @param overlap_percentile Numeric, percentile to use for overlap calculation (default 95)
#'
#' @return A list with two components:
#'   \itemize{
#'     \item \code{BGFG_stats}: Data.frame with QC statistics including:
#'       - Legacy metrics: BG_overlap, CV, SD, etc.
#'       - New v2.0.0 metrics: SBR_log2, SBR_fold, fg_overlap_pct, bg_overlap_pct, 
#'         separation_gap, separation_MAD, overall_BGFG_qc
#'     \item \code{plot_data}: Data.frame in long format ready for density plotting
#'   }
#'
#' @note
#' **Version History:**
#' \itemize{
#'   \item v2.0.0 (2025-01-08): Enhanced by @DrGarnett
#'     \itemize{
#'       \item Added Signal-to-Background Ratio (SBR) metrics (log2 and fold)
#'       \item Added actual distribution overlap percentages (fg_overlap_pct, bg_overlap_pct)
#'       \item Added separation metrics (gap and MAD-based effect size)
#'       \item Added comprehensive QC flags and overall_BGFG_qc decision
#'       \item Maintained full backward compatibility with legacy metrics
#'     }
#' }
#'
#' @author Shaun Garnett
#' @export
calculate_bg_fg_stats <- function(datCollate,
																	apply_log2 = TRUE,
																	min_spot_count = 20,
																	overlap_percentile = 95) {
	
	# Extract feature definitions (exclude controls)
	feature_data <- datCollate$data$Data %>%
		dplyr::select(Protein, data) %>%
		distinct()
	
	# Filter raw data to include only feature spots
	df <- datCollate$data$RawData %>%
		left_join(feature_data, by = "Protein") %>%
		filter(data != "ctrl")
	
	# Apply log2 transformation
	if (apply_log2 == TRUE) {
		test_data <- df %>%
			mutate(BGFG = FG - BG) %>%
			mutate(FG = log2(FG)) %>%
			mutate(BG = log2(BG)) %>%
			mutate(NetI = log2(NetI))
	} else {
		test_data <- df %>%
			mutate(BGFG = FG - BG)
	}
	
	# Handle infinite values
	test_data$FG[!is.finite(test_data$FG)] <- NA
	test_data$BG[!is.finite(test_data$BG)] <- NA
	test_data$NetI[!is.finite(test_data$NetI)] <- NA
	
	# Select relevant columns
	test_data <- test_data %>%
		filter(!is.na(Protein)) %>%
		dplyr::select(Labels, Sample, Protein, FG, BG, NetI, BGFG)
	
	# Create plot data in long format
	plot_data <- test_data %>%
		gather(key = key, value = value, c(4:dim(test_data)[2]))
	
	# Calculate comprehensive statistics per sample
	flag_df <- test_data %>%
		group_by(Sample) %>%
		mutate(count = n()) %>%
		ungroup() %>%
		filter(count > min_spot_count) %>%
		group_by(Labels, Sample) %>%
		summarise(
			count = n(),
			positive = sum(BGFG > 0, na.rm = TRUE),
			
			# ========== LEGACY STATISTICS (original) ==========
			FG_mean = try(mean(FG, na.rm = TRUE)),
			FG_mode = try(mode_function(FG)),
			BG_mean = try(mean(BG, na.rm = TRUE)),
			BG_mode = try(mode_function(BG)),
			NetI_mean = try(mean(NetI, na.rm = TRUE)),
			NetI_mode = try(mode_function(NetI)),
			
			FG_median = try(median(FG, na.rm = TRUE)),
			BG_median = try(median(BG, na.rm = TRUE)),
			NetI_median = try(median(NetI, na.rm = TRUE)),
			
			FG_sd = try(sd(FG, na.rm = TRUE)),
			BG_sd = try(sd(BG, na.rm = TRUE)),
			NetI_sd = try(sd(NetI, na.rm = TRUE)),
			
			FG_mad = try(mad(FG, na.rm = TRUE)),
			BG_mad = try(mad(BG, na.rm = TRUE)),
			NetI_mad = try(mad(NetI, na.rm = TRUE)),
			
			# ========== NEW v2.0.0: PERCENTILES FOR OVERLAP CALCULATION ==========
			BG_p95 = quantile(BG, overlap_percentile / 100, na.rm = TRUE),
			FG_p05 = quantile(FG, (100 - overlap_percentile) / 100, na.rm = TRUE),
			
			BGFG_difference = try(mean(BGFG, na.rm = TRUE)),
			
			.groups = "drop"
		) %>%
		ungroup() %>%
		mutate(
			# ========== LEGACY DERIVED METRICS ==========
			FG_sd_multiple = FG_mode - (datCollate$param$BG_FG_sd_mutiple * FG_sd),
			FG_mad_multiple = FG_mode - (datCollate$param$BG_FG_sd_mutiple * FG_mad),
			BG_CV = BG_sd / BG_mean * 100,
			FG_CV = FG_sd / FG_mean * 100,
			NetI_CV = NetI_sd / NetI_mean * 100,
			BG_CV_mode = BG_sd / BG_mode * 100,
			FG_CV_mode = FG_sd / FG_mode * 100,
			NetI_CV_mode = NetI_sd / NetI_mode * 100,
			PositivePercentage = positive / count * 100,
			
			# ========== NEW METRICS v2.0.0 by @DrGarnett (2025-01-08) ==========
			
			# 1. Signal-to-Background Ratio (SBR)
			SBR_log2 = FG_median - BG_median,  # In log2 space (subtraction = ratio)
			SBR_fold = 2^(FG_median - BG_median),  # As fold change
			
			# 2. Distribution separation gap (positive = good, negative = overlap)
			separation_gap = FG_p05 - BG_p95,
			
			# 3. Separation in MAD units (effect size / Cohen's d analog)
			separation_MAD = (FG_median - BG_median) / sqrt((BG_mad^2 + FG_mad^2) / 2),
			
			# 4. QC Flags based on new metrics
			SBR_fail = SBR_log2 < 1,  # Less than 2-fold separation
			overlap_fail_gap = separation_gap < 0,  # Distributions overlap
			separation_fail = separation_MAD < 2  # Less than 2 MAD separation
		)
	
	# ========== NEW v2.0.0: ACTUAL OVERLAP PERCENTAGES ==========
	overlap_stats <- test_data %>%
		group_by(Labels, Sample) %>%
		summarise(
			# % of FG spots below BG 95th percentile (FG in BG range)
			fg_overlap_pct = sum(FG < quantile(BG, overlap_percentile / 100, na.rm = TRUE), na.rm = TRUE) / n() * 100,
			# % of BG spots above FG 5th percentile (BG in FG range)
			bg_overlap_pct = sum(BG > quantile(FG, (100 - overlap_percentile) / 100, na.rm = TRUE), na.rm = TRUE) / n() * 100,
			.groups = "drop"
		)
	
	# Join overlap stats and add overall QC decision
	flag_df <- flag_df %>%
		left_join(overlap_stats, by = c("Labels", "Sample")) %>%
		mutate(
			# Additional QC flag based on actual overlap percentage
			overlap_fail_pct = fg_overlap_pct > 5,  # More than 5% overlap
			
			# ========== NEW v2.0.0: OVERALL QC DECISION ==========
			# Overall QC status: "flag" if ANY criterion fails, otherwise "pass"
			overall_BGFG_qc = ifelse(
				SBR_fail | overlap_fail_pct | separation_fail,
				"flag",
				"pass"
			)
		)
	
	# ========== LEGACY BG_overlap CALCULATION (original logic) ==========
	BG_overlap_metric <- datCollate$param$BG_overlap_metric
	
	if (BG_overlap_metric == "Mode_sd") {
		flag_df <- flag_df %>%
			mutate(BG_overlap = FG_mode - (datCollate$param$BG_FG_sd_mutiple * FG_sd))
	}
	if (BG_overlap_metric == "Median_sd") {
		flag_df <- flag_df %>%
			mutate(BG_overlap = FG_median - (datCollate$param$BG_FG_sd_mutiple * FG_sd))
	}
	if (BG_overlap_metric == "Mean_sd") {
		flag_df <- flag_df %>%
			mutate(BG_overlap = FG_mean - (datCollate$param$BG_FG_sd_mutiple * FG_sd))
	}
	if (BG_overlap_metric == "Mode_mad") {
		flag_df <- flag_df %>%
			mutate(BG_overlap = FG_mode - (datCollate$param$BG_FG_sd_mutiple * FG_mad))
	}
	if (BG_overlap_metric == "Median_mad") {
		flag_df <- flag_df %>%
			mutate(BG_overlap = FG_median - (datCollate$param$BG_FG_sd_mutiple * FG_mad))
	}
	if (BG_overlap_metric == "Mean_mad") {
		flag_df <- flag_df %>%
			mutate(BG_overlap = FG_mean - (datCollate$param$BG_FG_sd_mutiple * FG_mad))
	}
	
	# ========== PEPPER ARTIFACTS (high intensity outliers) ==========
	pepper_data <- datCollate$data$Data %>%
		filter(data == "feature") %>%
		left_join(flag_df %>% dplyr::select(Sample, NetI_sd, NetI_mode), by = "Sample") %>%
		filter(log2(mean) > (NetI_mode + (NetI_sd * 2))) %>%
		group_by(Sample) %>%
		summarise(Pepper_2sd = n(), .groups = "drop") %>%
		ungroup()
	
	flag_df <- flag_df %>%
		left_join(pepper_data, by = "Sample") %>%
		mutate(Pepper_2sd = ifelse(is.na(Pepper_2sd), 0, Pepper_2sd))
	
	# ========== LEGACY BG OVERLAP PERCENTAGES ==========
	spot_total <- length(unique(df$spot))
	
	flag_df <- BG_count_function(plot_data, flag_df, "FG_sd_multiple", spot_total)
	flag_df <- BG_count_function(plot_data, flag_df, "BG_overlap", spot_total)
	
	# Return results
	list(
		BGFG_stats = flag_df,
		plot_data = plot_data
	)
}

#' Evaluate BG/FG QC Status
#'
#' @param BGFG_stats Data.frame with BG/FG statistics
#' @param metric Character, column name of metric
#' @param threshold Numeric threshold value
#' @param direction Character, "above" or "below"
#' @return BGFG_stats with QC_status column ("pass" or "flag")
#' @export
evaluate_bgfg_qc <- function(BGFG_stats, metric, threshold, direction = "above") {
	
	if (!metric %in% colnames(BGFG_stats)) {
		warning(sprintf("Metric '%s' not found. Using BG_overlap_BG_Percentage", metric))
		metric <- "BG_overlap_BG_Percentage"
	}
	
	values <- BGFG_stats[[metric]]
	
	if (direction == "above") {
		BGFG_stats$QC_status <- ifelse(values > threshold, "flag", "pass")
		BGFG_stats$QC_reason <- sprintf("%s > %.2f", metric, threshold)
	} else if (direction == "below") {
		BGFG_stats$QC_status <- ifelse(values < threshold, "flag", "pass")
		BGFG_stats$QC_reason <- sprintf("%s < %.2f", metric, threshold)
	} else {
		warning(sprintf("Unknown direction '%s'. Using 'above'", direction))
		BGFG_stats$QC_status <- ifelse(values > threshold, "flag", "pass")
		BGFG_stats$QC_reason <- sprintf("%s > %.2f", metric, threshold)
	}
	
	BGFG_stats$QC_metric_used <- metric
	BGFG_stats$QC_threshold <- threshold
	BGFG_stats$QC_direction <- direction
	
	return(BGFG_stats)
}

#' Create Density Plot (Core Function)
#'
#' @export
create_density_plot <- function(density_plot_data,
																density_vline_data = NULL,
																sample_flag_data = NULL,
																flag_ylim = 2,
																col_num = 3,
																xlim = c(0, 16),
																facet = "Sample",
																scale = "auto") {
	
	density_plot <- ggplot(data = density_plot_data)
	
	if (facet == "Sample") {
		samples <- unique(density_plot_data$Sample)
		df_FG <- df_BG <- df_NetI <- data.frame()
		
		for (entry in samples) {
			sample_data <- density_plot_data %>% filter(Sample == entry)
			density_BG <- density(log2(sample_data$BG))
			density_FG <- density(log2(sample_data$FG))
			sample_data$NetI[sample_data$NetI < 1 | is.na(sample_data$NetI)] <- 1
			density_NetI <- density(log2(sample_data$NetI))
			
			df_BG <- rbind(df_BG, data.frame(x = density_BG$x, y = density_BG$y, Sample = entry, key = "BG"))
			df_FG <- rbind(df_FG, data.frame(x = density_FG$x, y = density_FG$y, Sample = entry, key = "FG"))
			df_NetI <- rbind(df_NetI, data.frame(x = density_NetI$x, y = density_NetI$y, Sample = entry, key = "NetI"))
		}
		
		scale_factor <- max(df_BG$y, na.rm = TRUE) / max(df_FG$y, na.rm = TRUE)
		
		density_plot <- density_plot +
			geom_line(data = df_BG, aes(x = x, y = y), color = "red", linewidth = 1) +
			geom_line(data = df_FG, aes(x = x, y = y * scale_factor), color = "blue", linewidth = 1) +
			geom_line(data = df_NetI, aes(x = x, y = y * scale_factor), color = "green", linewidth = 1) +
			scale_y_continuous(
				name = "Density (BG)",
				sec.axis = sec_axis(~ . / scale_factor, name = "Density (FG, NetI)")
			)
		
		if (!is.null(density_vline_data) && "BG" %in% colnames(density_vline_data)) {
			density_plot <- density_plot +
				geom_vline(data = density_vline_data, aes(xintercept = BG), alpha = 0.25, color = "red", linewidth = 1.5) +
				geom_vline(data = density_vline_data, aes(xintercept = BG + BG_sd), alpha = 1, color = "red", linewidth = 0.5) +
				geom_vline(data = density_vline_data, aes(xintercept = FG), alpha = 0.25, color = "blue", linewidth = 1.5) +
				geom_vline(data = density_vline_data, aes(xintercept = BG_overlap), alpha = 1, color = "blue", linewidth = 0.5)
		}
		
		if (!is.null(density_vline_data) && "RFU_threshold" %in% colnames(density_vline_data)) {
			density_plot <- density_plot +
				geom_vline(data = density_vline_data, aes(xintercept = RFU_threshold), alpha = 0.25, color = "black", linewidth = 1.5)
		}
	} else {
		density_plot <- density_plot +
			geom_density(data = density_plot_data, aes(x = log2(BG), group = Sample), color = "pink", linewidth = 0.5) +
			geom_density(data = density_plot_data, aes(x = log2(FG), group = Sample), color = "lightskyblue1", linewidth = 0.5) +
			geom_density(data = density_plot_data, aes(x = log2(BG)), color = "red", linewidth = 1.5) +
			geom_density(data = density_plot_data, aes(x = log2(FG)), color = "blue", linewidth = 1.5)
		
		if (!is.null(density_vline_data)) {
			density_plot <- density_plot +
				geom_vline(aes(xintercept = mean(BG, na.rm = TRUE)), data = density_vline_data, color = "red", linewidth = 1.5) +
				geom_vline(aes(xintercept = mean(FG, na.rm = TRUE)), data = density_vline_data, color = "blue", linewidth = 1.5)
		}
	}
	
	if (scale %in% c("full", "fixed")) {
		density_plot <- density_plot +
			coord_cartesian(xlim = xlim) +
			scale_x_continuous(breaks = seq(xlim[1], xlim[2], by = 1))
	} else {
		density_plot <- density_plot +
			scale_x_continuous(breaks = seq(0, 16, by = 2))
	}
	
	density_plot +
		geom_hline(yintercept = 0, colour = "black", linewidth = 0.25) +
		scale_fill_discrete(drop = FALSE) +
		labs(y = "Density", x = "log2 Intensity") +
		theme_minimal() +
		theme(
			axis.title = element_text(size = 14),
			plot.title = element_text(size = 14),
			axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
			legend.position = "bottom"
		)
}


#' Create BG/FG Density Plot with Dynamic QC Metric Annotations
#'
#' @param datCollate Data collection object
#' @param QC QC object containing BGFG_stats
#' @param samples Vector of sample names to plot
#' @param facet_by Faceting variable (default "Sample")
#' @param xlim X-axis limits
#' @param scale Scale type ("auto", "full", "fixed")
#' @param col_num Number of columns for faceting
#' @param flag_ylim Y-axis limit for flagged samples
#' @param show_new_metrics Whether to show metric annotations
#' @param primary_qc_metric Name of primary QC metric to display (e.g., "BG Overlap (Legacy)", "SBR Fold")
#' @param qc_threshold Threshold value for pass/fail determination
#' @param fail_direction "Above Threshold" or "Below Threshold"
#'
#' @return ggplot object
#' @author Shaun Garnett
#' @export
create_bg_fg_density_plot <- function(datCollate,
																			QC,
																			samples,
																			facet_by = "Sample",
																			xlim = c(4, 16),
																			scale = "auto",
																			col_num = 3,
																			flag_ylim = 2,
																			show_new_metrics = TRUE) {
	print('create_bg_fg_density_plot')
	
	param <- if (!is.null(QC$param)) QC$param else datCollate$param
	bgfg_qc_metric <- param$BGFG_QC_metric
	bgfg_qc_direction <- param$BGFG_QC_direction
	
	# Default to BG_overlap if not specified
	if (is.null(bgfg_qc_metric) || bgfg_qc_metric == "") {
		bgfg_qc_metric <- "BG_overlap"
		message("BGFG_QC_metric not found in param. Defaulting to BG_overlap")
	}
	if(is.null(show_new_metrics)){
		show_new_metrics = FALSE
		if(bgfg_qc_metric != 'BG_overlap'){
			show_new_metrics = TRUE
		}
	}
	
	RawData <- datCollate$data$RawData
	raw_data_columns <- c("Labels", "Sample", "Protein", "BG", "FG", "NetI")
	
	has_upper_bg <- "UpperBG" %in% colnames(RawData)
	if (has_upper_bg) raw_data_columns <- c(raw_data_columns, "UpperBG")
	
	has_lower_neti <- "lowIntensityThresholds" %in% names(datCollate$data)
	if (has_lower_neti) {
		RawData <- RawData %>%
			left_join(datCollate$data$lowIntensityThresholds %>% 
									dplyr::select(Sample, nMAD2NetI) %>% 
									rename(lowerNetI = nMAD2NetI), by = "Sample")
		raw_data_columns <- c(raw_data_columns, "lowerNetI")
	}
	
	density_plot_data <- RawData %>%
		dplyr::select(all_of(raw_data_columns)) %>%
		left_join(datCollate$data$feature_df, by = "Protein") %>%
		filter(data == "feature", Sample %in% samples)
	
	BGFG_stats <- QC$BGFG_stats
	#print(colnames(BGFG_stats))
	
	# if (!is.null(datCollate$data$RFU_thresold)) {
	# 	BGFG_stats <- BGFG_stats %>% left_join(datCollate$data$RFU_thresold, by = c("Sample", "Labels"))
	# }
	
	if (!is.null(datCollate$data$RFU_thresold)) {
		BGFG_stats <- BGFG_stats %>%
			left_join(
				datCollate$data$RFU_thresold %>% dplyr::select(Sample, Labels, RFU_threshold),
				by = c("Sample", "Labels")
			)
	}
	
	#print(colnames(BGFG_stats))
	if (!"RFU_threshold" %in% colnames(BGFG_stats)) BGFG_stats$RFU_threshold <- 1
	
	BG_overlap_metric <- datCollate$param$BG_overlap_metric
	if (is.null(BG_overlap_metric)) {
		BG_overlap_metric <- "Mode"
		message("BG_overlap_metric not specified, using 'Mode' as default")
	}
	

	
	base_metric <- case_when(
		grepl("median", BG_overlap_metric, ignore.case = TRUE) ~ "Median",
		grepl("mode", BG_overlap_metric, ignore.case = TRUE) ~ "Mode",
		grepl("mean", BG_overlap_metric, ignore.case = TRUE) ~ "Mean",
		TRUE ~ "Mode"
	)
	
	metric_cols <- switch(base_metric,
												"Mode" = c("BG_mode", "FG_mode"),
												"Median" = c("BG_median", "FG_median"),
												"Mean" = c("BG_mean", "FG_mean"))
	
	print(base_metric)
	print(metric_cols)
	print(colnames(BGFG_stats))
	
	density_vline_data <- BGFG_stats %>%
		dplyr::select(Labels, Sample, all_of(metric_cols), BG_sd, FG_sd, BG_mad, FG_mad,
									BG_overlap, RFU_threshold,
									matches("SBR_log2|SBR_fold|separation_gap|fg_overlap_pct|bg_overlap_pct|separation_MAD|overall_BGFG_qc|BG_overlap_BG_Percentage")) %>%
		filter(Sample %in% samples) %>%
		rename(BG = !!sym(metric_cols[1]), FG = !!sym(metric_cols[2])) %>%
		mutate(
			metric = base_metric,
			BG_upper_2mad = BG + (2 * BG_mad),
			FG_lower_2mad = FG - (2 * FG_mad),
			overlap_start = pmax(FG_lower_2mad, BG),
			overlap_end = pmin(BG_upper_2mad, FG),
			has_overlap = overlap_start < overlap_end
		)
	
	density_plot <- create_density_plot(
		density_plot_data = density_plot_data,
		density_vline_data = density_vline_data,
		xlim = xlim,
		facet = facet_by,
		scale = scale,
		col_num = col_num
	)
	
	# Add MAD lines
	density_plot <- density_plot +
		geom_vline(aes(xintercept = BG_upper_2mad), data = density_vline_data,
							 color = "darkred", linetype = "dashed", linewidth = 0.8, alpha = 0.7) +
		geom_vline(aes(xintercept = FG_lower_2mad), data = density_vline_data,
							 color = "darkblue", linetype = "dashed", linewidth = 0.8, alpha = 0.7)
	
	# Add overlap region
	if (any(density_vline_data$has_overlap, na.rm = TRUE)) {
		density_plot <- density_plot +
			geom_rect(aes(xmin = overlap_start, xmax = overlap_end, ymin = -Inf, ymax = Inf),
								data = density_vline_data %>% filter(has_overlap),
								fill = "purple", alpha = 0.15, inherit.aes = FALSE)
	}
	

	if (show_new_metrics) {
		
		# Get param object
		
		
		# Read BGFG_QC_metric and BGFG_QC_direction from param
	
		

		
		if (is.null(bgfg_qc_direction) || bgfg_qc_direction == "") {
			bgfg_qc_direction <- "above"
		}
		
		# Map param values to column names and formatting
		metric_mapping <- list(
			"BG_overlap" = list(
				col = "BG_overlap_BG_Percentage",
				param_key = "BG_overlap_BG_Percentage",
				format = "%.1f%%",
				label = "BG Overlap"
			),
			"SBR_log2" = list(
				col = "SBR_log2",
				param_key = "SBR_log2",
				format = "%.2f",
				label = "SBR Log2"
			),
			"SBR_fold" = list(
				col = "SBR_fold",
				param_key = "SBR_fold",
				format = "%.1fx",
				label = "SBR"
			),
			"fg_overlap_pct" = list(
				col = "fg_overlap_pct",
				param_key = "fg_overlap_pct",
				format = "%.1f%%",
				label = "FG Overlap"
			),
			"bg_overlap_pct" = list(
				col = "bg_overlap_pct",
				param_key = "bg_overlap_pct",
				format = "%.1f%%",
				label = "BG Overlap"
			),
			"separation_gap" = list(
				col = "separation_gap",
				param_key = "separation_gap",
				format = "%.2f",
				label = "Gap"
			),
			"separation_MAD" = list(
				col = "separation_MAD",
				param_key = "separation_MAD",
				format = "%.2f",
				label = "Sep MAD"
			)
		)
		
		# Get selected metric details
		selected_metric <- metric_mapping[[bgfg_qc_metric]]
		
		# Fallback if metric not recognized
		if (is.null(selected_metric)) {
			message(sprintf("Metric '%s' not recognized. Falling back to BG_overlap.", bgfg_qc_metric))
			selected_metric <- metric_mapping[["BG_overlap"]]
			bgfg_qc_metric <- "BG_overlap"
		}
		
		# Check if column exists in data
		if (selected_metric$col %in% colnames(density_vline_data)) {
			
			# Get threshold from param
			qc_threshold <- param[[selected_metric$param_key]]
			
			# Use default if not found
			if (is.null(qc_threshold) || is.na(qc_threshold)) {
				qc_threshold <- switch(bgfg_qc_metric,
															 "BG_overlap" = 10,
															 "SBR_log2" = 1,
															 "SBR_fold" = 2,
															 "fg_overlap_pct" = 5,
															 "bg_overlap_pct" = 5,
															 "separation_gap" = 0,
															 "separation_MAD" = 2,
															 10)  # final fallback
				message(sprintf("Threshold for '%s' not found in param. Using default: %s", 
												bgfg_qc_metric, qc_threshold))
			}
			
			# Convert direction to fail logic
			fail_above <- (bgfg_qc_direction == "above")
			
			annotation_data <- density_vline_data %>%
				mutate(
					# Get metric value
					metric_value = .data[[selected_metric$col]],
					
					# Format metric text
					metric_text = sprintf(paste0(selected_metric$label, ": ", selected_metric$format), metric_value),
					
					# Determine pass/fail based on threshold and direction
					qc_status = if (fail_above) {
						ifelse(metric_value > qc_threshold, "flag", "pass")
					} else {
						ifelse(metric_value < qc_threshold, "flag", "pass")
					},
					
					# Set color based on status
					qc_color = ifelse(qc_status == "flag", "red", "darkgreen"),
					qc_text = toupper(qc_status),
					
					# Threshold text with direction indicator
					threshold_text = sprintf("Thr: %s %s", 
																	 ifelse(fail_above, "<", ">"),
																	 qc_threshold),
					
					# Position for annotations
					x_pos = xlim[2] - 0.5
				)
			
			# Add annotations to plot
			density_plot <- density_plot +
				geom_text(aes(x = x_pos, y = Inf, label = metric_text, color = qc_color),
									data = annotation_data, hjust = 1, vjust = 2, size = 3.5, 
									fontface = "bold", inherit.aes = FALSE, show.legend = FALSE) +
				geom_text(aes(x = x_pos, y = Inf, label = threshold_text),
									data = annotation_data, hjust = 1, vjust = 4, size = 3, 
									color = "gray30", inherit.aes = FALSE) +
				geom_text(aes(x = x_pos, y = Inf, label = qc_text, color = qc_color),
									data = annotation_data, hjust = 1, vjust = 6, size = 4, 
									fontface = "bold", inherit.aes = FALSE, show.legend = FALSE) +
				scale_color_identity()
			
		} else {
			warning(sprintf("Column '%s' for metric '%s' not found in BGFG_stats. Skipping annotations.", 
											selected_metric$col, bgfg_qc_metric))
		}
	}
	
	# Optional lines
	if (has_upper_bg) {
		density_plot <- density_plot +
			geom_vline(aes(xintercept = log2(UpperBG)), data = density_plot_data,
								 colour = "orange", linetype = "dotted", alpha = 0.5)
	}
	
	if (has_lower_neti) {
		density_plot <- density_plot +
			geom_vline(aes(xintercept = log2(lowerNetI)), data = density_plot_data,
								 colour = "green", linetype = "dotted", alpha = 0.5)
	}
	
	density_plot +
		labs(
			title = sprintf("BG/FG Density Distribution (Metric: %s)", base_metric),
			subtitle = "Red: BG | Blue: FG | Purple: Overlap | Dashed: ±2 MAD"
		) +
		facet_grid(as.formula(paste(facet_by, "~ .")))
}