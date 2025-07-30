#' Multi-Spot Filter Count Function
#'
#' Summarizes and flags samples or features based on counts and percentages of flagged observations.
#' Used as a helper in QC reporting to assess the impact of CV or filtering criteria across groups.
#'
#' @param df A data frame containing CV flag data.
#' @param grouping_column Character. Column to group by (e.g., 'Sample' or 'Protein').
#' @param number Numeric or scalar. Total count used for calculating percentages.
#' @param flag_list Character vector. List of flags to include.
#' @param Thr Numeric. Threshold percentage for flagging QC status.
#' @param metric Character. Label describing the metric being assessed.
#' @param plot_title Character. Title for use in potential plots (not plotted here).
#' @param data_type Character. Optional string to tag the type of input data (e.g., 'protein').
#'
#' @return A list with:
#' \describe{
#'   \item{flag_count}{Data frame with flag counts and percentages.}
#'   \item{flag_sample_order}{Ordering and QC status per group.}
#'   \item{data}{Wide-format summary of flag counts per group.}
#'   \item{Avg}{Mean of the calculated percentages.}
#'   \item{data_type}{Passed-through label of data type.}
#' }
#'
#' @note
#' Version 1.0.0 from
#' QC_sub_functions.R
#'
#' @export
multi_spot_filter_count_function <- function(df,
																						 grouping_column,
																						 number,
																						 flag_list,
																						 Thr,
																						 metric,
																						 plot_title,
																						 data_type = 'protein') {
	print('multi_spot_filter_count_function')
	print(metric)
	print(plot_title)

	# Count flagged entries and calculate percentage
	flag_count <- df %>%
		mutate(flag = ifelse(is.na(flag), NA, flag)) %>%
		group_by(!!sym(grouping_column), flag) %>%
		summarise(count = n(), .groups = "drop") %>%
		mutate(
			total = number,
			count = ifelse(is.na(flag), 0, count),
			Percentage = count / total * 100,
			PerThr = Thr,
			metric = metric
		)

	# Restrict to defined flag list
	flags <- unique(flag_count$flag)
	flags <- flags[!is.na(flags)]
	flag_list <- flag_list[flag_list %in% flags]
	print(flag_list)
	flag_count$flag <- factor(flag_count$flag, levels = flag_list)

	# Create QC status (pass or flag) based on total percentage per group
	flag_sample_order <- flag_count %>%
		group_by(!!sym(grouping_column), total) %>%
		summarise(sum = sum(count), .groups = "drop") %>%
		mutate(
			Percentage = sum / total * 100,
			PerThr = Thr,
			QC = ifelse(Percentage > PerThr, 'flag', 'pass')
		) %>%
		arrange(sum)

	# Join QC status back to flag_count
	flag_count <- flag_count %>%
		left_join(flag_sample_order %>% select(!!sym(grouping_column), QC), by = grouping_column)

	# Handle ordering of grouping factor
	if (grouping_column == "Sample") {
		flag_count$Sample <- factor(flag_count$Sample, levels = flag_sample_order$Sample)
	}
	if (grouping_column == "Protein") {
		flag_count$Protein <- factor(flag_count$Protein, levels = flag_sample_order$Protein)
	}

	# Create wide format summary if flags are present
	if (length(flag_list) > 0) {
		flag_count_wide <- flag_count %>%
			filter(!is.na(flag)) %>%
			select(-Percentage) %>%
			tidyr::spread(key = flag, value = count) %>%
			left_join(
				flag_count %>%
					group_by(!!sym(grouping_column)) %>%
					summarise(Percentage = sum(Percentage, na.rm = TRUE), .groups = "drop"),
				by = grouping_column
			) %>%
			mutate(metric = metric)
	} else {
		flag_count_wide <- flag_count
	}

	Avg <- round(mean(flag_count_wide$Percentage, na.rm = TRUE), 2)

	# Return results
	list(
		flag_count = flag_count,
		flag_sample_order = flag_sample_order,
		data = flag_count_wide,
		Avg = Avg,
		data_type = data_type
	)
}


#' Average CV Function
#'
#' Computes the average coefficient of variation (CV) per sample or feature group,
#' and applies QC classification based on a threshold. Includes optional checks
#' for Cy3-specific metrics such as signal intensity and completeness.
#'
#' @param data A data frame with columns: `cv`, `mean`, and a grouping column (e.g., `Sample`).
#' @param cutoffPercentage Numeric. Threshold CV percentage for flagging.
#' @param metric Character. Label for the metric being evaluated (e.g., 'Protein Average CV %').
#' @param type Character. Type of metric (default 'Average').
#' @param grouping_column Character. Name of the column used to group (default 'Sample').
#' @param data_type Character. Tag for data type, such as 'protein' or 'feature' (default 'protein').
#' @param min_NetI Numeric. Minimum log2 mean Net Intensity to consider valid (default 12).
#'
#' @return A list containing:
#' \describe{
#'   \item{data}{Data frame with CV values, flags, and QC decisions.}
#'   \item{Avg}{Overall average CV value.}
#'   \item{data_type}{Tag passed through from input.}
#' }
#'
#' @note
#' Version 1.0.0 from
#' QC_sub_functions.R
#'
#' @export
AvgCV_function <- function(data,
													 cutoffPercentage,
													 metric,
													 type = 'Average',
													 grouping_column = 'Sample',
													 data_type = 'protein',
													 min_NetI = 12) {

	data_list <- list()

	# Calculate average CV, log2 mean intensity, and count per group
	AvgCV_persmpl <- data %>%
		group_by(!!sym(grouping_column)) %>%
		summarise(
			AvgCV = round(mean(cv, na.rm = TRUE), 3),
			log2meanNetI = round(log2(mean(mean, na.rm = TRUE)), 3),
			count = n(),
			.groups = "drop"
		) %>%
		mutate(
			PerThr = cutoffPercentage,
			flag_type = ifelse(AvgCV < PerThr, 'pass', 'HighCV'),
			metric = metric
		)

	# Additional logic for Cy3 metrics if grouping is Sample
	if (grouping_column == 'Sample' && metric == 'Cy3AvgCV') {
		mean_NetI <- mean(AvgCV_persmpl$log2meanNetI, na.rm = TRUE)
		sd_NetI <- sd(AvgCV_persmpl$log2meanNetI, na.rm = TRUE)
		max_NetI <- mean_NetI + (3 * sd_NetI)
		max_number <- max(AvgCV_persmpl$count, na.rm = TRUE)

		AvgCV_persmpl <- AvgCV_persmpl %>%
			mutate(flag_type = ifelse(
				log2meanNetI < min_NetI, 'low RFU',
				ifelse(log2meanNetI > max_NetI, 'high RFU',
							 ifelse(count < max_number, 'incomplete', flag_type)
				)
			))
	}

	# Assign final QC status
	AvgCV_persmpl <- AvgCV_persmpl %>%
		mutate(QC = ifelse(flag_type == 'pass', 'pass', 'flag'))

	AvgCV_persmpl$QC <- factor(AvgCV_persmpl$QC, levels = c('pass', 'flag'))

	# Calculate global average CV
	AvgCV <- round(mean(AvgCV_persmpl$AvgCV, na.rm = TRUE), 2)

	# Output list
	data_list$data <- AvgCV_persmpl
	data_list$Avg <- AvgCV
	data_list$data_type <- data_type

	return(data_list)
}



#' IgG R² QC Function
#'
#' Evaluates R², slope, and signal quality across a dilution series for control antibody probes.
#' Flags non-linear relationships or low signal intensity in each sample's response curve.
#'
#' @param igR2_data A data frame containing Sample, Dilution, value (NetI or Ratio), and Protein info.
#' @param ylab Character. Label for the y-axis (used to infer Ratio vs NetI behavior).
#' @param log Logical. Whether values were log-transformed (used in naming).
#' @param metric Character. Name of the metric being evaluated.
#' @param R2_threshold Numeric. Minimum R² value to pass QC.
#' @param slope_threshold Numeric. Allowed deviation from slope = 1.
#' @param Probe Character. Prefix for control antibody probes.
#' @param top_probe_values A data frame with maxDANetI (log2 RFU) per Sample.
#' @param minNetI Numeric. Minimum acceptable NetI threshold.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{QC summary per sample, including R², slope, and QC flag.}
#'   \item{Data}{Input igR2_data joined with QC info.}
#'   \item{AvgR2}{Overall mean R².}
#'   \item{AvgSlope}{Overall mean slope.}
#'   \item{data_type}{Fixed label: "ctrl".}
#' }
#'
#' @note
#' Version 1.0.0 from
#' QC_sub_functions.R
#'
#' @export
igR2_function <- function(igR2_data,
													ylab,
													log,
													metric,
													R2_threshold,
													slope_threshold,
													Probe,
													top_probe_values,
													minNetI) {

	ig_R2_count <- igR2_data %>%
		group_by(Sample) %>%
		summarise(count_total = n(), .groups = "drop") %>%
		mutate(R2_Thr = R2_threshold)

	ig_R2 <- igR2_data %>%
		filter(!is.na(value)) %>%
		group_by(Sample) %>%
		summarise(
			cor = round(cor(Dilution, value), 3),
			R2 = round(summary(lm(Dilution ~ value))$adj.r.squared, 3),
			slope = round(coef(lm(Dilution ~ value))[2], 3),
			count = n(),
			.groups = "drop"
		) %>%
		full_join(ig_R2_count, by = "Sample") %>%
		left_join(top_probe_values, by = "Sample") %>%
		mutate(
			flag_type = dplyr::case_when(
				is.na(R2) ~ 'missing',
				R2 < R2_Thr ~ 'non linear',
				count < max(count, na.rm = TRUE) ~ 'incomplete',
				maxDANetI < minNetI ~ 'low RFU',
				TRUE ~ 'pass'
			),
			QC = ifelse(flag_type == 'pass', 'pass', 'flag'),
			metric = metric
		)

	if (grepl('Ratio', ylab)) {
		ig_R2 <- ig_R2 %>%
			mutate(flag_type = ifelse(
				flag_type == 'flag',
				flag_type,
				ifelse(slope > 1 + slope_threshold | slope < 1 - slope_threshold,
							 'slope', flag_type)
			)) %>%
			mutate(QC = ifelse(flag_type == 'pass', 'pass', 'flag'))
	}

	ig_R2$QC <- factor(ig_R2$QC, levels = c('pass', 'flag'))

	igratio_plot_data <- igR2_data %>%
		left_join(ig_R2, by = "Sample")

	AvgR2 <- round(mean(ig_R2$R2, na.rm = TRUE), 2)
	AvgSlope <- signif(mean(ig_R2$slope, na.rm = TRUE), 3)

	list(
		data = ig_R2,
		Data = igR2_data,
		AvgR2 = AvgR2,
		AvgSlope = AvgSlope,
		data_type = 'ctrl'
	)
}

