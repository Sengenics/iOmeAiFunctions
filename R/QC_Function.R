
#' Spot Quality Control
#'
#' Generates a QC report summarizing spot and protein filtering statistics
#' for either per-sample or per-protein analysis based on flags and thresholds.
#'
#'
#' @param datCollate A list object containing all relevant assay data, including RawData, Data, parameters, and counts.
#' @param type Character, either `"PerSample"` (default) or `"PerProtein"` to specify the grouping level for filtering.
#' @param thresholds
#' \describe{
#'  \item{PerSample}
#'   \item{datCollate$param$BGFilter_ThrPer = 10}
#'   \item{datCollate$param$BGFilter_ThrPer_Protein = 1}
#'  \item(PerProtein)
#'    \item{datCollate$param$PerProtein_BGFilter_ThrPer}
#'    \item{datCollate$param$PerProtein_BGFilter_ThrPer_Protein}
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
	unique(feature_df$data)

	# Filter by type
	if (type == 'PerProtein') {
		df <- df %>%
			left_join(feature_df) %>%
		  filter(data == 'feature' | data == 'analyte')
		unique(df$data)
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
			filter(data == 'feature' | data == 'analyte')
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
			filter(data == 'feature' | data == 'analyte')
	}

	# Assign missing values based on num_test
	protein_data <- protein_data %>%
		mutate(flag = ifelse(is.na(num_test), flag, NA))

	# Define valid flag list
	flag_list <- c('Filtered', 'low RFU', 'Saturated')
	df <- protein_data
	#if(!is.na(lowRFU)){
	#  df$flag[df$mean < lowRFU] = 'low RFU'
	#}
	unique(df$flag)
	unique(df$source)
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
		grouping_column <- feature_column
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


#' QC Protein Report Function
#'
#' This function generates quality control (QC) metrics and visualizations for protein-level data,
#' specifically for evaluating coefficient of variation (CV) across samples or across proteins/features.
#' It supports two modes:
#' - `"PerSample"`: Assessing the percentage of proteins with high CV per sample.
#' - `"PerProtein"`: Assessing the percentage of samples with high CV per protein/feature.
#'
#' @param datCollate A list containing data and parameters, typically generated by earlier preprocessing steps.
#' @param type Character string, either `"PerSample"` or `"PerProtein"`, determining the QC strategy. Default is `"PerSample"`.
#' @param feature_column Character string specifying the column name representing features. Default is `"Protein"`. Can also be `"feature"`.
#'
#' @return A list containing QC results:
#' \describe{
#'   \item{HighCV}{A summary of CV filtering (e.g., percentage above threshold).}
#'   \item{ProteinAvgCV}{Average CVs computed per sample or feature, depending on type.}
#' }
#'
#' @note
#' Version 1.0.0 from
#' QC_function.R
#'
#' @export
QC_protein_report = function(datCollate, type = 'PerSample', feature_column = 'Protein') {
	print('QC protein')
	print(type)

	feature_key = tolower(feature_column)

	if (type == 'PerSample') {
		CV_threshold = datCollate$param$CV_threshold
		Thr = datCollate$param$CV_Sample_PerThr
		data = datCollate$data$Data %>%
			filter(!Sample %in% datCollate$param$BOC_samples) %>%
			filter(data == 'feature' | data == 'analyte')
	}

	if (type == 'PerProtein') {
		CV_threshold = datCollate$param$PerProtein_AvgCV
		Thr = datCollate$param$PerProtein_HighCV_PerThr
		data = datCollate$data$Data %>%
			filter(!Sample %in% datCollate$param$BOC_samples) %>%
			filter(data == 'feature' | data == 'analyte')
	}

	protein_QC = list()
	dim(data)

	protein_df = data %>%
		mutate(flag = source)

	# if (datCollate$param$CV_flag == 'GoodCV') {
	# 	protein_df$flag[protein_df$flag == 'GoodCV'] = NA
	# } else {
	# 	protein_df$flag[!protein_df$flag == 'HighCV'] = NA
	# }

	protein_df$flag[!protein_df$flag %in% datCollate$param$CV_flag] = NA

	flag_list = protein_df %>%
		filter(!is.na(flag)) %>%
		pull(flag) %>%
		unique()

	print(flag_list)

	title = paste0('Percentage of ',
								 ifelse(type == 'PerSample', 'Proteins per Sample', paste0('Samples per ', feature_column)),
								 ' with CV above ', CV_threshold, '%')

	protein_QC$HighCV = list()

	if (type == 'PerSample') {
		grouping_column = 'Sample'
		number = datCollate$data$number_list$protein
		metric = 'HighCV Percentage'
		plot_title = title

		protein_QC$HighCV = multi_spot_filter_count_function(
			protein_df,
			grouping_column,
			number,
			flag_list,
			Thr,
			metric,
			plot_title,
			'protein'
		)
	}

	if (type == 'PerProtein') {
		title = paste0('Percentage of Samples with CV above ', CV_threshold, '% per ', feature_column)
		sample_number = datCollate$data$number_list$sample

		protein_QC$HighCV = multi_spot_filter_count_function(
			protein_df,
			feature_column,
			sample_number,
			flag_list,
			Thr,
			'HighCV Percentage',
			title,
			'sample'
		)
	}

	metric = "Protein Average CV %"
	title = 'ProteinAvgCV'
	QC_type = 'Average'

	if (type == 'PerSample') {
		grouping_column = 'Sample'
		protein_QC$ProteinAvgCV = AvgCV_function(
			data,
			CV_threshold,
			metric,
			QC_type,
			grouping_column,
			feature_key
		)
	}

	if (type == 'PerProtein') {
		grouping_column = feature_column
		protein_QC$ProteinAvgCV = AvgCV_function(
			data,
			CV_threshold,
			metric,
			QC_type,
			grouping_column,
			'sample'
		)
	}

	return(protein_QC)
}


#' QC Control Report Plots
#'
#' Generates QC metrics for control probes (e.g., BSA and IgG), including CV and R² checks.
#' Allows flexibility in feature column naming (e.g., "Protein" or "Feature").
#'
#' @param datCollate A list containing input data and parameters.
#' @param data_type Character. Either "PerSample" or "PerProtein". Default is "PerSample".
#' @param feature_column Character. The name of the feature column (e.g., "Protein"). Default is "Protein".
#'
#' @return A list of QC outputs including Cy3 control CVs and IgG dilution R² results.
#'
#' @note
#' Version 1.0.0 from
#' QC_functions.R
#'
#' @export
QC_ctrl_repot_plots <- function(datCollate,
																data_type = 'PerSample',
																feature_column = 'Protein') {
	print('ctrl Report Plots')

	feature_key <- tolower(feature_column)
	ctrl_QC <- list()

	cutoffPercentage <- datCollate$param$Cy3BSA_AvgCV
	Cy3BSA_log2RFU <- datCollate$param$Cy3BSA_log2RFU
	Probe <- datCollate$param$Probe
	ctrl_BSA_probes <- datCollate$param$ctrl_BSA_probes
	ctrl_antibody_probes <- datCollate$param$ctrl_antibody_probes

	# Allow both "ctrl" and "control" labels
	ctrl_Data <- datCollate$data$Data %>%
		filter(!Sample %in% datCollate$param$BOC_samples) %>%
		filter(data == 'ctrl' | data == 'control')

	# ---- Cy3 BSA QC ----
	if (!is.null(ctrl_BSA_probes)) {
		raw_Cy3BSA_data <- datCollate$data$RawData %>%
			filter(!!sym(feature_column) %in% ctrl_BSA_probes) %>%
			group_by(Sample) %>%
			summarise(
				mean = mean(NetI, na.rm = TRUE),
				sd = sd(NetI, na.rm = TRUE),
				.groups = "drop"
			) %>%
			mutate(probe_cv = sd / mean * 100)

		ctrlDat_Cy3BSA <- ctrl_Data %>%
			filter(!!sym(feature_column) %in% ctrl_BSA_probes) %>%
			left_join(raw_Cy3BSA_data %>% select(Sample, probe_cv), by = "Sample")

		metric <- 'Cy3AvgCV'
		grouping_column <- ifelse(data_type == 'PerSample', 'Sample', feature_column)

		ctrl_QC$Cy3AvgCV <- AvgCV_function(
			ctrlDat_Cy3BSA,
			cutoffPercentage,
			metric,
			'Cy3',
			grouping_column,
			'ctrl',
			Cy3BSA_log2RFU
		)
	}

	# ---- IgG Dilution QC ----
	if (data_type == "PerSample" && !is.null(ctrl_antibody_probes)) {
		ctrlDat_Ig <- ctrl_Data %>%
			filter(!!sym(feature_column) %in% ctrl_antibody_probes)

		ctrl_antibody_probes <- sort(ctrl_antibody_probes)
		top_probe <- grep('1', ctrl_antibody_probes, value = TRUE)

		ig_numbers <- readr::parse_number(ctrl_antibody_probes)
		dilution_list <- c(1, 1/2, 1/4, 1/8, 1/16, 1/32)
		dilution_list_edit <- dilution_list[ig_numbers]

		dilution_table <- data.frame(
		  feature = ctrl_antibody_probes,
		  Dilution = dilution_list_edit,
		  stringsAsFactors = FALSE
		)
		names(dilution_table)[1] <- feature_column

		igratio_o <- ctrlDat_Ig %>% select(Sample, !!sym(feature_column), mean)

		factor_probe <- igratio_o %>%
			filter(!!sym(feature_column) == ctrl_antibody_probes[1]) %>%
		  rename('top_ctrl' = !!sym(feature_column) )
		  #dplyr::select(-!!sym(feature_column))

		igratio <- igratio_o %>%
			left_join(dilution_table, by = feature_column) %>%
			left_join(factor_probe %>% rename(i.mean = mean), by = "Sample") %>%
			mutate(Ratio = mean / i.mean) %>%
			rowwise() %>%
			mutate(
				RD_sd = sd(c(Dilution, Ratio)),
				RD_mean = mean(c(Dilution, Ratio)),
				RD_cv = RD_sd / RD_mean * 100,
				RD = abs(Dilution / Ratio)
			) %>%
			ungroup()

		R2_threshold <- datCollate$param$R2_threshold
		slope_threshold <- datCollate$param$ctrl_antribody_slope_threshold
		ctrl_antibody_RFU <- datCollate$param$ctrl_antibody_RFU

		igR2_mean_data <- igratio %>%
			mutate(value = mean)

		top_probe <- grep('1', unique(igR2_mean_data[[feature_column]]), value = TRUE)

		top_probe_values <- igR2_mean_data %>%
			filter(!!sym(feature_column) == top_probe) %>%
			select(Sample, value) %>%
			mutate(value = log2(value)) %>%
			rename(maxDANetI = value)

		igR2_data = igR2_mean_data
		ylab = "Mean Net Intensity"
		log = FALSE
		metric = 'NetI Ig R2'
		R2_threshold
		slope_threshold
		Probe
		top_probe_values
		minNetI = ctrl_antibody_RFU

		igR2_mean <- igR2_function(
			igR2_mean_data, "Mean Net Intensity", FALSE,
			'NetI Ig R2', R2_threshold, slope_threshold,
			Probe, top_probe_values, ctrl_antibody_RFU
		)

		igR2_mean_log2 <- igR2_function(
			igR2_mean_data, "Mean Net Intensity", TRUE,
			'log2 Ig R2', R2_threshold, slope_threshold,
			Probe, top_probe_values, ctrl_antibody_RFU
		)

		igR2_ratio_data <- igratio %>% mutate(value = Ratio)

		igR2_ratio <- igR2_function(
			igR2_ratio_data,
			paste0('Ratio to ', Probe, ' 1'), FALSE,
			'Ratio Ig R2', R2_threshold, slope_threshold,
			Probe, top_probe_values, ctrl_antibody_RFU
		)

		igR2_ratio_log2 <- igR2_function(
			igR2_ratio_data,
			paste0('Ratio to ', Probe, ' 1'), TRUE,
			'log2 Ratio Ig R2', R2_threshold, slope_threshold,
			Probe, top_probe_values, ctrl_antibody_RFU
		)

		ctrl_QC$IgR2 <- list(
			ratio = igR2_ratio,
			mean = igR2_mean,
			log2_mean = igR2_mean_log2,
			log2_ratio = igR2_ratio_log2
		)
	}

	print('ctrl Report Plots ; done')
	return(ctrl_QC)
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
#' @note
#' Version 1.0.0 from
#' QC_Function.R
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
#' @note
#' Version 1.0.0 from
#' QC_Function.R
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
			data.frame(feature = character(0),value = double(0), QC = character(0), Thr = integer(0), metric = character(0)),
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



