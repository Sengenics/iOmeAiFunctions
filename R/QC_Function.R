
#' QC Spot Filtering Report
#' @note File:QC_Function.R
#'
#' Generates a QC report summarizing spot and protein filtering statistics 
#' for either per-sample or per-protein analysis based on flags and thresholds.
#'
#' @param datCollate A list object containing all relevant assay data, including RawData, Data, parameters, and counts.
#' @param type Character, either `"PerSample"` (default) or `"PerProtein"` to specify the grouping level for filtering.
#'
#' @return A list with two components:
#' \describe{
#'   \item{filter_outlier}{Summary of spot filtering and outlier removal}
#'   \item{protein_filter_BG}{Summary of protein filtering based on background thresholds}
#' }
#' @note Version 1.1.1
#' @export
QC_Spot_Filtering_Report <- function(datCollate, type = 'PerSample') {
	print('QC_Spot_Filtering_Report')
	print(type)
	
	# Determine number of samples
	sample_number <- datCollate$data$number_list$sample
	
	# Filter out BOC samples and missing proteins
	df <- datCollate$data$RawData %>%
		filter(!Sample %in% datCollate$param$BOC_samples) %>%
		filter(!is.na(Protein))
	
	# Get unique (Protein, data) combinations
	feature_df <- datCollate$data$Data %>%
		dplyr::select(Protein, data) %>%
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
			filter(data == 'feature')
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
		grouping_column <- 'Protein'
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


