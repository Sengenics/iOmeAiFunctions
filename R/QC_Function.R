#' @import rlang
#' @import dplyr
#' @import tidyr
NULL


#' Spot Quality Control
#'
#' Generates a QC report summarizing spot and protein filtering statistics
#' for either per-sample or per-protein analysis based on flags and thresholds.
#'
#'
#' @param datCollate A list object containing all relevant assay data, including RawData, Data, parameters, and counts.
#' @param type Character, either `"PerSample"` (default) or `"PerProtein"` to specify the grouping level for filtering.
#'
#' @return A list with two components:
#' \describe{
#'   \item{filter_outlier}{Summary of spot filtering and outlier removal}
#'   \item{protein_filter_BG}{Summary of protein filtering based on background thresholds}
#' }
#'
#' @note
#' Version 1.0.0 from
#' QC_Function.R
#'
#' @export
QC_Spot_Filtering_Report <- function(datCollate, type = 'PerSample',feature_column = 'Protein') {
	print('QC_Spot_Filtering_Report')
	print(type)

	# Determine number of samples
	sample_number <- datCollate$data$number_list$sample

	# Filter out BOC samples and missing proteins
	df <- datCollate$data$RawData %>%
		filter(!Sample %in% datCollate$param$BOC_samples) %>%
		filter(!is.na(!!sym(feature_column)))
	colnames(df)

	# Get unique (Protein, data) combinations
	feature_df <- datCollate$data$Data %>%
		dplyr::select(!!sym(feature_column), data) %>%
		distinct()

	# Filter by type
	if (type == 'PerProtein') {
		df <- df %>%
			left_join(feature_df) %>%
			filter(data != 'ctrl')
		sample_number <- df %>%
			pull(Sample) %>%
			unique() %>%
			length()
	} else {
		df <- df %>%
			left_join(feature_df) %>%
			filter(data == 'feature' | data == 'analyte')
	}

	# Flag saturation
	df <- df %>%
		rowwise() %>%
		mutate(flag = ifelse(FG > 60000,
												 ifelse(is.na(flag), 'Spot Saturation',
												 			 paste(flag, 'and Saturated')), flag)) %>%
		ungroup()

	# Get unique flag list
	flag_list <- unique(df$flag)
	flag_list <- flag_list[!is.na(flag_list)]

	# Set thresholds
	if (type == 'PerSample') {
		spot_Thr <- datCollate$param$BGFilter_ThrPer
		protein_Thr <- datCollate$param$BGFilter_ThrPer_Protein
	}
	if (type == 'PerProtein') {
		spot_Thr <- datCollate$param$PerProtein_BGFilter_ThrPer
		protein_Thr <- datCollate$param$PerProtein_BGFilter_ThrPer_Protein
	}

	# Run spot filter QC
	if (type == "PerSample") {
		grouping_column <- 'Sample'
		number <- datCollate$data$number_list$spot

		filter_outlier <- multi_spot_filter_count_function(
			df, grouping_column, number, flag_list, spot_Thr,
			'Spot Filter and Outlier Removal',
			'Percentage of Spots Filtered, Outliers Removed and/or Saturated per Sample',
			'probe'
		)

		protein_data <- datCollate$data$Data %>%
			filter(!Sample %in% datCollate$param$BOC_samples) %>%
			filter(data == 'feature')
	}

	if (type == 'PerProtein') {
		grouping_column <- feature_column
		number <- sample_number * datCollate$data$number_list$spot_replicates

		filter_outlier <- multi_spot_filter_count_function(
			df, grouping_column, number, flag_list, spot_Thr,
			'Spot Filter and Outlier Removal',
			'Percentage of Spots Filtered, Outliers Removed and/or Saturated per Protein across all Samples',
			'sample probe'
		)

		protein_data <- datCollate$data$Data %>%
			filter(!Sample %in% datCollate$param$BOC_samples) %>%
			filter(data != 'ctrl')
	}

	# Assign missing values based on num_test
	protein_data <- protein_data %>%
		mutate(flag = ifelse(is.na(num_test), flag, NA))

	# Define valid flag list
	flag_list <- c('Filtered', 'low RFU', 'Saturated')
	df <- protein_data
	df$flag[!df$flag %in% flag_list] <- NA

	# Run protein filter QC
	if (type == 'PerSample') {
		grouping_column <- 'Sample'
		number <- datCollate$data$number_list$protein

		multi_protein <- multi_spot_filter_count_function(
			df, grouping_column, number, flag_list, protein_Thr,
			'Protein Spot Filtering',
			'Percentage of Proteins filtered per Sample',
			'protein'
		)
	}

	if (type == 'PerProtein') {
		grouping_column <- 'Protein'
		number <- sample_number

		multi_protein <- multi_spot_filter_count_function(
			df, grouping_column, number, flag_list, protein_Thr,
			'Protein Spot Filtering',
			'Percentage of Proteins filtered across Sample',
			'sample'
		)
	}

	# Return results
	spot_filtering_qc <- list(
		filter_outlier = filter_outlier,
		protein_filter_BG = multi_protein
	)

	return(spot_filtering_qc)
}


#' RawData Statistics
#'
#' Calculates mean, mode, median, and standard deviation of foreground (FG), background (BG),
#' and net intensity (NetI) values per sample, and computes a BG overlap threshold. Also returns
#' the percentage of background values above the BG_overlap threshold.
#'
#' @param datCollate A list containing:
#'   - \code{data$RawData}: Data frame with columns FG, BG, NetI, feature_column, Sample.
#'   - \code{data$Data}: Data frame with a \code{data} column (e.g., 'ctrl', 'feature').
#'   - \code{param$BG_overlap_metric}: One of "Mean", "Median", or "Mode".
#'   - \code{param$BG_FG_sd_mutiple}: Numeric multiplier for FG standard deviation.
#' @param feature_column Column name used to identify analyte vs control (e.g., "Protein", "Target").
#'
#' @return A data.frame with one row per sample including:
#'   - FG/BG/NetI mean, mode, median, sd
#'   - BG_overlap threshold
#'   - BG_overlap_BG_Percentage: % of BG values exceeding BG_overlap threshold
#'
#' @export
bg_fg_metrics <- function(datCollate, feature_column = "Protein") {
	feature_sym <- rlang::sym(feature_column)
	
	# Join with data annotations
	feature_data <- datCollate$data$Data %>%
		dplyr::select(!!feature_sym, data) %>%
		distinct()
	
	df <- datCollate$data$RawData %>%
		left_join(feature_data) %>%
		filter(data %in% c("feature", "analyte"))
	
	# Log2 transform and clean
	df <- df %>%
		mutate(
			FG = log2(FG),
			BG = log2(BG),
			NetI = log2(NetI)
		) %>%
		mutate(across(c(FG, BG, NetI), ~ ifelse(is.finite(.), ., NA_real_))) %>%
		filter(!is.na(!!feature_sym))
	
	# Calculate metrics
	flag_df <- df %>%
		group_by(Sample) %>%
		summarise(
			FG_mean = mean(FG, na.rm = TRUE),
			FG_mode = mode_function(FG),
			FG_median = median(FG, na.rm = TRUE),
			FG_sd = sd(FG, na.rm = TRUE),
			
			BG_mean = mean(BG, na.rm = TRUE),
			BG_mode = mode_function(BG),
			BG_median = median(BG, na.rm = TRUE),
			BG_sd = sd(BG, na.rm = TRUE),
			
			NetI_mean = mean(NetI, na.rm = TRUE),
			NetI_mode = mode_function(NetI),
			NetI_median = median(NetI, na.rm = TRUE),
			NetI_sd = sd(NetI, na.rm = TRUE),
			.groups = "drop"
		)
	
	# BG overlap calculation
	metric <- datCollate$param$BG_overlap_metric
	multiple <- datCollate$param$BG_FG_sd_mutiple
	
	flag_df <- flag_df %>%
		mutate(
			BG_overlap = dplyr::case_when(
				metric == "Mode"   ~ FG_mode - multiple * FG_sd,
				metric == "Median" ~ FG_median - multiple * FG_sd,
				metric == "Mean"   ~ FG_mean - multiple * FG_sd,
				TRUE               ~ NA_real_
			)
		)
	
	# Count % of BG values above BG_overlap
	plot_data <- df %>%
		select(Sample, BG) %>%
		rename(value = BG) %>%
		mutate(key = "BG")
	
	flag_df <- BG_count_function(
		plot_data = plot_data,
		flag_df = flag_df,
		col_name = "BG_overlap",
		spot_total_number = length(unique(df$spot))
	)
	
	return(flag_df)
}


#' Background QC
#'
#' Calculate the percentage of background (BG) values exceeding a threshold per sample
#'
#' This function compares background intensity values (BG) for each sample
#' against a user-specified threshold column in the summary `flag_df`. It computes
#' the percentage of BG values that exceed this threshold and adds the result as
#' a new column to `flag_df`, named \code{<col_name>_BG_Percentage}.
#'
#' @param plot_data A data.frame in long format with at least columns:
#'   \code{Sample}, \code{value} (BG values), and \code{key = 'BG'}.
#' @param flag_df A data.frame with one row per sample and a column
#'   named according to \code{col_name}, containing the threshold for that sample.
#' @param col_name A string specifying the column in \code{flag_df} that holds
#'   the per-sample BG threshold to compare against (e.g., "BG_overlap").
#' @param spot_total_number The total number of array spots per sample
#'   (used to normalize the count into a percentage).
#'
#' @return The original \code{flag_df}, augmented with a new column
#'   \code{<col_name>_BG_Percentage}, containing the percentage of
#'   BG values above the specified threshold for each sample.
#'
#' @note
#' Version 1.0.0 from
#' QC_Function.R
#'
#' @import dplyr
#' @export
#'
#' @examples
#' BG_count_function(plot_data, flag_df, col_name = "BG_overlap", spot_total_number = 200)
BG_count_function = function(plot_data, flag_df, col_name, spot_total_number) {

  # Extract BG values and merge with thresholds
  BG = plot_data %>%
    filter(key == 'BG') %>%
    dplyr::rename(BG = value) %>%
    left_join(flag_df %>% dplyr::select(Sample, !!sym(col_name)), by = "Sample") %>%
    filter(BG > !!sym(col_name))  # Keep BG values above threshold

  # Define the name of the new percentage column
  per_col = paste0(col_name, '_BG_Percentage')

  # Count and normalize
  BG_count = BG %>%
    group_by(Sample, !!sym(col_name)) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      spot_total = spot_total_number
    ) %>% 
  	mutate(
      Percentage = count / spot_total * 100
    ) %>%
    select(Sample, Percentage)

  # Merge result back into flag_df
  flag_df %>%
    left_join(BG_count, by = "Sample") %>%
    mutate(Percentage = ifelse(is.na(Percentage), 0, Percentage)) %>%
    dplyr::rename(!!per_col := Percentage)
}



#' Merge QC Function
#'
#' Aggregates QC data from a structured QC object across either samples or features.
#'
#' @param QC A nested list of QC results
#' @param type Either "PerSample" or "PerProtein", determines the axis of aggregation
#' @param feature_column Character string for the feature identifier (default = "Protein")
#'
#' @return A list with:
#'   - flagged_df: full QC results with metrics
#'   - flagged: vector of flagged Sample or feature_column values
#'   - Avg_df: thresholds and average values per QC metric
#'
#' @examples
#' Merge_QC_function(QC_list, type = "PerSample")
#'
#' @export
#'
#' @author Shaun Garnett
#'
# Notes:
# Version: 1.1.0
# File: Merge_QC_function.R
# - Updated to support dynamic `feature_column` (e.g., "Protein" or "feature")
# - Replaced hardcoded Protein references with flexible column input
# - Added inline comments for future improvements (bind_rows, verbosity toggle, etc.)

Merge_QC_function = function(QC, type = "PerSample", feature_column = "Protein") {
	print('Merge_QC')
	output_list <- NULL
	
	flagged_df <- if (type == 'PerSample') {
		data.frame(Sample = character(0), value = double(0), QC = character(0), Thr = integer(0), metric = character(0))
	} else {
		stats::setNames(
			data.frame(value = double(0), QC = character(0), Thr = integer(0), metric = character(0)),
			c(feature_column, "value", "QC", "Thr", "metric")
		)
	}
	
	if (type == "PerProtein" && !feature_column %in% colnames(flagged_df)) {
		stop(paste("feature_column", feature_column, "not found in flagged_df"))
	}
	
	Avg_df <- data.frame(metric = character(0), Thr = double(0), Avg = double(0), data_type = character(0))
	
	for (level_1 in names(QC)) {
		if (level_1 != 'QC') {
			print(level_1)
			for (level_2 in names(QC[[level_1]])) {
				print(level_2)
				if ('data' %in% names(QC[[level_1]][[level_2]])) {
					print('hit')
					data <- QC[[level_1]][[level_2]]$data
					if (nrow(data) > 0) {
						df_n <- QC_merge_edit_function(flagged_df, data)
						flagged_df <- rbind(flagged_df, df_n)
						Avg_df <- rbind(
							Avg_df,
							data.frame(
								metric = unique(df_n$metric),
								Thr = unique(df_n$Thr),
								Avg = round(QC[[level_1]][[level_2]]$Avg, 3),
								data_type = QC[[level_1]][[level_2]]$data_type
							)
						)
					}
				}
			}
		}
	}
	
	if ('IgR2' %in% names(QC$ctrl_QC)) {
		igQC <- QC$ctrl_QC$IgR2$ratio
		if (nrow(igQC$data) > 0) {
			data <- igQC$data %>%
				mutate(value = R2 * 100, Thr = R2_Thr * 100, metric = 'Ig R2')
			df_n <- QC_merge_edit_function(flagged_df, data)
			flagged_df <- rbind(flagged_df, df_n)
			Avg_df <- rbind(
				Avg_df,
				data.frame(
					metric = unique(df_n$metric),
					Thr = unique(df_n$Thr),
					Avg = round(igQC$AvgR2, 3),
					data_type = igQC$data_type
				)
			)
		}
	}
	
	flagged_df <- flagged_df %>% filter(!is.na(QC))
	
	flagged <- if (type == 'PerSample') {
		flagged_df %>%
			dplyr::select(Sample, QC) %>%
			filter(QC == 'flag') %>%
			distinct() %>%
			pull(Sample)
	} else {
		flagged_df %>%
			dplyr::select(all_of(feature_column), QC) %>%
			filter(QC == 'flag') %>%
			distinct() %>%
			pull(!!sym(feature_column))
	}
	
	output_list <- list(
		flagged_df = flagged_df,
		flagged = flagged,
		Avg_df = Avg_df
	)
	
	return(output_list)
}



