#' iOmeAIFunctions: Penetrance Fold Change (pFC) Analysis
#'
#' @description
#' The penetrance fold change (pFC) method detects true autoantibodies that differ
#' between two groups of interest and was specifically designed for small sample size analyses.
#'
#' @details
#' The detailed steps involved are:
#' \enumerate{
#'   \item Generate a per-antigen baseline based on a reference group (e.g., for disease vs
#'         healthy, "healthy" is set as baseline/reference group) using appropriately
#'         normalized input data
#'   \item For each antigen in each sample calculate fold changes based on the per-antigen
#'         baseline value calculated in step 1
#'   \item For each group being compared calculate per-antigen frequencies (penetrance
#'         frequency) of samples that exceed an FC threshold relative to the per-antigen
#'         baseline value calculated in step 1
#'   \item For samples that exceed the pFC threshold defined in step 3, calculate – on a
#'         per-antigen basis – the mean log2 intensity and the mean FC for each group of
#'         interest (include in the final results table)
#'   \item Using the frequencies calculated in step 3, perform Fisher's exact testing on a
#'         per-antigen basis, comparing groups of interest
#'   \item Optionally determine antigens that are associated with PSA status by linear
#'         modeling and report PSA-associated antigens in the final results table
#' }
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{pFC_process}}: Core processing function using ExpressionSet
#'   \item \code{\link{pFC_plot}}: Generate plots from pFC results
#'   \item \code{\link{pFC_save}}: Save results and plots to disk
#'   \item \code{\link{pFC_analysis}}: Pipeline wrapper maintaining backward compatibility
#'   \item \code{\link{pFC_UI}}: Shiny module UI component
#'   \item \code{\link{pFC_Server}}: Shiny module server component
#' }
#' @keywords internal
"_PACKAGE"
#' Process Penetrance Fold Change (pFC) Analysis
#'
#' Core processing function for pFC analysis using ExpressionSet objects.
#' This function performs the main pFC calculations and returns all data tables.
#'
#' @param eset ExpressionSet object containing expression data and phenotype data
#' @param var Character string; variable of interest (column name in pData(eset))
#' @param groupPos Character string; category name for positive group (e.g., 'case')
#' @param groupNeg Character string; category name for negative group (e.g., 'control')
#' @param fold_change Numeric; FC threshold for penetrance calculations (default: 2)
#' @param p_val Numeric; p-value threshold for Fisher's exact test (default: 0.2)
#' @param PSA_flag Logical; flag pFC hits based on PSA status (default: TRUE)
#' @param PSA_colname Character string; column name for PSA typing in pData(eset)
#' @param cores Integer; number of cores for parallel processing (default: 1)
#'
#' @return List containing:
#' \itemize{
#'   \item normalized_data: Normalized RFU data (wide format)
#'   \item fc_data: Per-feature, per-sample fold changes
#'   \item pfc_stats: Complete pFC statistics table
#'   \item pfc_significant: Significant hits (p < p_val threshold)
#'   \item fisher_results: Fisher's exact test results
#'   \item metadata: Filtered metadata used in analysis
#'   \item parameters: List of parameters used
#' }
#'
#' @export
pFC_process <- function(eset, 
												assay_name,
												var, 
												groupPos, 
												groupNeg,
												fold_change = 2,
												p_val = 0.2,
												PSA_flag = TRUE,
												PSA_colname = "PSA_class",
												cores = 1) {
	
	if (missing(assay_name) || is.null(assay_name) ||
			!is.character(assay_name) || length(assay_name) != 1 || !nzchar(assay_name)) {
		stop(
			"`assay_name` is required and must be a single non-empty character string naming an assay in the ExpressionSet, (clinical_loess_normalised)",
			call. = FALSE
		)
	}
	
	# Check if assay exists
	available_assays <- Biobase::assayDataElementNames(eset)
	
	if (!assay_name %in% available_assays) {
		stop("Assay '", assay_name, "' not found in ExpressionSet.\n",
				 "Available assays: ", paste(available_assays, collapse = ", "))
	}
	
	# Extract specified assay
	if (assay_name == "exprs") {
		input_data <- Biobase::exprs(eset)
	} else {
		input_data <- Biobase::assayDataElement(eset, assay_name)
	}
	
	# # Extract data from ExpressionSet
	# if(!is.null(assay_name){
	# 	input_date = eset@assayData[]
	# input_data <- exprs(eset)  # log2-transformed expression data
	metadata <- pData(eset)
	
	# Subset metadata to include only relevant groups
	metadata <- metadata %>% 
		dplyr::filter((!!rlang::sym(var)) %in% c(groupPos, groupNeg))
	metadata <- droplevels(metadata)
	
	# Ensure input data matches metadata samples
	input_data <- input_data[, rownames(metadata), drop = FALSE]
	
	# Format data to long format with RFU (back-transform from log2)
	df <- input_data %>%
		as.data.frame(check.names = FALSE) %>%
		tibble::rownames_to_column("Protein") %>%
		tidyr::pivot_longer(cols = -Protein, 
												names_to = "Sample_ID", 
												values_to = "value") %>%
		dplyr::mutate(RFU = 2^value)
	
	# Add metadata
	metadata$Sample_ID <- rownames(metadata)
	merge.df <- merge(metadata, df, by = "Sample_ID") %>%
		dplyr::filter(!(!!rlang::sym(var)) %in% c("", "PN"))
	
	# Create normalized data output (wide format, non-log2)
	normalized_data <- merge.df %>%
		reshape2::dcast(Protein ~ Sample_ID, value.var = "RFU")
	
	# Calculate groupNeg baseline (median RFU per feature)
	control_median <- merge.df %>%
		dplyr::filter(!!rlang::sym(var) == groupNeg) %>%
		dplyr::group_by(Protein) %>%
		dplyr::summarise(baseline_median = median(RFU, na.rm = TRUE))
	
	# Calculate per-feature, per-sample fold changes
	FC.merge.dat <- merge(merge.df, control_median, by = "Protein") %>%
		dplyr::mutate(FC = RFU / baseline_median)
	
	# Handle NA and Inf values
	FC.merge.dat$FC[is.na(FC.merge.dat$FC)] <- 0
	
	# FC output table
	fc_output <- FC.merge.dat %>%
		reshape2::dcast(Protein ~ Sample_ID, value.var = "FC")
	
	# Calculate penetrance statistics for groupPos
	n_groupPos <- sum(metadata[[var]] == groupPos)
	summary_penPos <- FC.merge.dat %>%
		dplyr::filter(!!rlang::sym(var) == groupPos) %>%
		dplyr::filter(FC >= fold_change, FC != Inf) %>%
		dplyr::group_by(Protein) %>%
		dplyr::summarise(
			freq_groupPos = n(),
			median_RFU_groupPos = median(RFU, na.rm = TRUE)
		) %>%
		dplyr::mutate(per_groupPos = (freq_groupPos / n_groupPos) * 100)
	
	pFC.pos <- merge(summary_penPos, control_median, by = "Protein") %>%
		dplyr::mutate(pFC_groupPos = median_RFU_groupPos / baseline_median) %>%
		dplyr::select(-baseline_median)
	
	# Calculate penetrance statistics for groupNeg
	n_groupNeg <- sum(metadata[[var]] == groupNeg)
	summary_penNeg <- FC.merge.dat %>%
		dplyr::filter(!!rlang::sym(var) == groupNeg) %>%
		dplyr::filter(FC >= fold_change, FC != Inf) %>%
		dplyr::group_by(Protein) %>%
		dplyr::summarise(
			freq_groupNeg = n(),
			median_RFU_groupNeg = median(RFU, na.rm = TRUE)
		) %>%
		dplyr::mutate(per_groupNeg = (freq_groupNeg / n_groupNeg) * 100)
	
	pFC.neg <- merge(summary_penNeg, control_median, by = "Protein") %>%
		dplyr::mutate(pFC_groupNeg = median_RFU_groupNeg / baseline_median) %>%
		dplyr::select(-baseline_median)
	
	# Combine all statistics
	all_proteins <- data.frame(Protein = unique(FC.merge.dat$Protein))
	final_stats <- all_proteins %>%
		merge(pFC.pos, by = "Protein", all.x = TRUE) %>%
		merge(pFC.neg, by = "Protein", all.x = TRUE) %>%
		merge(control_median, by = "Protein", all.x = TRUE)
	
	# Replace NA with 0
	final_stats[is.na(final_stats)] <- 0
	
	# Reorder and rename columns
	final_stats <- final_stats %>%
		dplyr::select(
			Protein,
			freq_groupPos, per_groupPos, median_RFU_groupPos, pFC_groupPos,
			freq_groupNeg, per_groupNeg, median_RFU_groupNeg, pFC_groupNeg,
			baseline_median
		)
	
	# Round numeric columns
	final_stats[, c(3:5, 7:10)] <- round(final_stats[, c(3:5, 7:10)], 0)
	
	# Rename columns with group names
	colnames(final_stats) <- c(
		"Protein",
		paste0("Penetrance frequency (", groupPos, ")"),
		paste0("Penetrance frequency% (", groupPos, ")"),
		paste0("Mean penetrance (", groupPos, ")"),
		paste0("Penetrance fold change (", groupPos, ")"),
		paste0("Penetrance frequency (", groupNeg, ")"),
		paste0("Penetrance frequency% (", groupNeg, ")"),
		paste0("Mean penetrance (", groupNeg, ")"),
		paste0("Penetrance fold change (", groupNeg, ")"),
		paste0("Mean (", groupNeg, ")")
	)
	
	# Fisher's exact testing
	fisher_bin <- fc_output
	rownames(fisher_bin) <- fisher_bin$Protein
	fisher_bin <- fisher_bin %>% dplyr::select(-Protein)
	fisher_bin <- ifelse(fisher_bin >= fold_change, 1, 0)
	
	meta_fish <- metadata[colnames(fisher_bin), ]
	meta_fish[[var]] <- as.factor(meta_fish[[var]])
	
	fisher_results <- fitPA_StandAlone(
		obj = as.matrix(fisher_bin),
		cl = meta_fish[[var]],
		cores = cores
	)
	
	fisher_results <- apply(fisher_results, 2, function(x) signif(x, digits = 2))
	fisher_results <- as.data.frame(fisher_results)
	fisher_results$Protein <- rownames(fisher_results)
	
	# Merge pFC stats with Fisher's results
	all_stats <- merge(final_stats, fisher_results, by = "Protein")
	
	# Filter significant results
	sig_stats <- all_stats %>%
		dplyr::filter(pvalues < p_val) %>%
		dplyr::arrange(pvalues)
	
	# Add PSA flags if requested
	if (PSA_flag && nrow(sig_stats) > 0 && PSA_colname %in% colnames(metadata)) {
		metadata[[PSA_colname]] <- as.factor(metadata[[PSA_colname]])
		PSA_tab <- table(metadata[[PSA_colname]])
		
		if (length(PSA_tab) > 1 && all(PSA_tab > 2)) {
			# Perform limma analysis for PSA association
			# Note: This requires limma_func from your package
			tryCatch({
				PSA_input <- input_data[, rownames(metadata), drop = FALSE]
				d_TT <- limma_func(
					metadata = metadata,
					input = PSA_input,
					feature_select = rownames(PSA_input),
					variable = PSA_colname,
					plot_violins = FALSE,
					EB_trend = TRUE,
					EB_robust = TRUE,
					plot_height = 20,
					file_descriptor = "temp_PSA",
					add_anno = var
				)
				
				PSA_sig <- d_TT[[2]] %>%
					dplyr::filter(abs(logFC) > log2(1.1) & P.Value < 0.05)
				
				sig_stats$PSA_flag <- ifelse(sig_stats$Protein %in% rownames(PSA_sig), "yes", "no")
			}, error = function(e) {
				warning("PSA flagging failed: ", e$message)
				sig_stats$PSA_flag <- NA
			})
		} else {
			sig_stats$PSA_flag <- NA
		}
	}
	
	# Return list of results
	results <- list(
		normalized_data = normalized_data,
		fc_data = fc_output,
		pfc_stats = all_stats,
		pfc_significant = sig_stats,
		fisher_results = fisher_results,
		metadata = metadata,
		merge_data = merge.df,  # For plotting
		input_data = input_data,  # For plotting
		parameters = list(
			var = var,
			groupPos = groupPos,
			groupNeg = groupNeg,
			fold_change = fold_change,
			p_val = p_val,
			PSA_flag = PSA_flag,
			PSA_colname = PSA_colname
		)
	)
	
	return(results)
}

#' Calculate pFC v2 baseline statistics
#'
#' Computes per-protein baseline statistics on the log2 scale, with optional
#' robust outlier trimming using an initial median + k*MAD cutoff.
#'
#' @param eset ExpressionSet object containing assay data and phenotype data.
#' @param assay_name Character string. Required assayData element name to analyze.
#' @param var Character string or NULL. Grouping variable in `pData(eset)`.
#'   Required when `baseline_source = "group"`.
#' @param baseline_source Character string. One of `"all"` or `"group"`.
#' @param baseline_group Character string or NULL. Reference group to use when
#'   `baseline_source = "group"`.
#' @param fold_change Numeric. Fold-change threshold used to derive
#'   `fc_threshold_log2` and `fc_threshold_rfu`.
#' @param trim_outliers Logical. If TRUE, remove provisional outliers using an
#'   initial `median + outlier_mad_multiplier * MAD` cutoff and recalculate.
#' @param outlier_mad_multiplier Numeric. Multiplier used for provisional outlier
#'   trimming. Default is 4.
#' @param min_baseline_n Integer. Minimum number of non-missing samples required
#'   to compute baseline statistics.
#'
#' @return Data frame with one row per protein.
#' @export
pFC_baseline_stats_v2 <- function(eset,
																	assay_name,
																	var = NULL,
																	baseline_source = c("all", "group"),
																	baseline_group = NULL,
																	fold_change = 2,
																	trim_outliers = TRUE,
																	outlier_mad_multiplier = 4,
																	min_baseline_n = 3) {
	
	baseline_source <- match.arg(baseline_source, choices = c("all", "group"))
	
	if (missing(assay_name) || is.null(assay_name) ||
			!is.character(assay_name) || length(assay_name) != 1 || !nzchar(assay_name)) {
		stop("`assay_name` is required and must be a single non-empty character string.", call. = FALSE)
	}
	
	if (!is.numeric(fold_change) || length(fold_change) != 1 || is.na(fold_change) || fold_change <= 0) {
		stop("`fold_change` must be a single positive numeric value.", call. = FALSE)
	}
	
	if (!is.numeric(outlier_mad_multiplier) || length(outlier_mad_multiplier) != 1 ||
			is.na(outlier_mad_multiplier) || outlier_mad_multiplier <= 0) {
		stop("`outlier_mad_multiplier` must be a single positive numeric value.", call. = FALSE)
	}
	
	available_assays <- Biobase::assayDataElementNames(eset)
	if (!assay_name %in% available_assays) {
		stop(
			"Assay '", assay_name, "' not found in ExpressionSet.\n",
			"Available assays: ", paste(available_assays, collapse = ", "),
			call. = FALSE
		)
	}
	
	metadata <- Biobase::pData(eset)
	input_data <- Biobase::assayDataElement(eset, assay_name)
	
	if (baseline_source == "group") {
		if (is.null(var) || !is.character(var) || length(var) != 1 || !var %in% colnames(metadata)) {
			stop("`var` must name a metadata column when `baseline_source = \"group\"`.", call. = FALSE)
		}
		if (is.null(baseline_group) || !is.character(baseline_group) ||
				length(baseline_group) != 1 || !nzchar(baseline_group)) {
			stop("`baseline_group` must be provided when `baseline_source = \"group\"`.", call. = FALSE)
		}
		
		metadata <- metadata %>%
			dplyr::filter((!!rlang::sym(var)) == baseline_group)
		
		if (nrow(metadata) == 0) {
			stop("No samples found for `baseline_group = \"", baseline_group, "\"`.", call. = FALSE)
		}
		
		input_data <- input_data[, rownames(metadata), drop = FALSE]
	}
	
	baseline_long <- input_data %>%
		as.data.frame(check.names = FALSE) %>%
		tibble::rownames_to_column("Protein") %>%
		tidyr::pivot_longer(
			cols = -Protein,
			names_to = "Sample_ID",
			values_to = "log2_value"
		) %>%
		dplyr::mutate(RFU = 2^log2_value)
	
	baseline_stats <- baseline_long %>%
		dplyr::group_by(Protein) %>%
		dplyr::group_modify(~{
			x <- .x$log2_value
			x <- x[!is.na(x)]
			
			if (length(x) < min_baseline_n) {
				return(tibble::tibble(
					baseline_n = length(x),
					baseline_n_trimmed = NA_integer_,
					baseline_n_removed = NA_integer_,
					baseline_log2_median_initial = NA_real_,
					baseline_log2_mad_initial = NA_real_,
					baseline_log2_outlier_cutoff = NA_real_,
					baseline_log2_median = NA_real_,
					baseline_log2_mad = NA_real_,
					baseline_log2_plus_2mad = NA_real_,
					baseline_log2_plus_3mad = NA_real_,
					fc_threshold_log2 = NA_real_
				))
			}
			
			median_initial <- stats::median(x, na.rm = TRUE)
			mad_initial <- stats::mad(x, center = median_initial, constant = 1, na.rm = TRUE)
			
			x_trim <- x
			cutoff <- NA_real_
			
			if (isTRUE(trim_outliers) && !is.na(mad_initial) && mad_initial > 0) {
				cutoff <- median_initial + (outlier_mad_multiplier * mad_initial)
				x_trim_candidate <- x[x <= cutoff]
				
				if (length(x_trim_candidate) >= min_baseline_n) {
					x_trim <- x_trim_candidate
				}
			}
			
			median_final <- stats::median(x_trim, na.rm = TRUE)
			mad_final <- stats::mad(x_trim, center = median_final, constant = 1, na.rm = TRUE)
			
			tibble::tibble(
				baseline_n = length(x),
				baseline_n_trimmed = length(x_trim),
				baseline_n_removed = length(x) - length(x_trim),
				baseline_log2_median_initial = median_initial,
				baseline_log2_mad_initial = mad_initial,
				baseline_log2_outlier_cutoff = cutoff,
				baseline_log2_median = median_final,
				baseline_log2_mad = mad_final,
				baseline_log2_plus_2mad = median_final + (2 * mad_final),
				baseline_log2_plus_3mad = median_final + (3 * mad_final),
				fc_threshold_log2 = median_final + log2(fold_change)
			)
		}) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(
			baseline_rfu_median = 2^baseline_log2_median,
			baseline_rfu_plus_2mad = 2^baseline_log2_plus_2mad,
			baseline_rfu_plus_3mad = 2^baseline_log2_plus_3mad,
			fc_threshold_rfu = 2^fc_threshold_log2
		)
	
	baseline_stats
}


pFC_sample_flags_v2 <- function(eset,
																assay_name,
																baseline_stats,
																var = NULL) {
	
	metadata <- Biobase::pData(eset)
	input_data <- Biobase::assayDataElement(eset, assay_name)
	
	metadata$Sample_ID <- rownames(metadata)
	
	sample_flags <- input_data %>%
		as.data.frame(check.names = FALSE) %>%
		tibble::rownames_to_column("Protein") %>%
		tidyr::pivot_longer(
			cols = -Protein,
			names_to = "Sample_ID",
			values_to = "log2_value"
		) %>%
		dplyr::mutate(RFU = 2^log2_value) %>%
		dplyr::left_join(metadata, by = "Sample_ID") %>%
		dplyr::left_join(baseline_stats, by = "Protein") %>%
		dplyr::mutate(
			log2_fc = log2_value - baseline_log2_median,
			FC = RFU / baseline_rfu_median,
			pass_fc_threshold = log2_value >= fc_threshold_log2,
			pass_2mad_threshold = log2_value >= baseline_log2_plus_2mad,
			pass_3mad_threshold = log2_value >= baseline_log2_plus_3mad
		)
	
	sample_flags
}

pFC_penetrance_by_group_v2 <- function(sample_flags,
																			 var,
																			 threshold_method = c("fc", "2mad", "3mad")) {
	
	threshold_method <- match.arg(threshold_method)
	
	pass_col <- switch(
		threshold_method,
		fc = "pass_fc_threshold",
		`2mad` = "pass_2mad_threshold",
		`3mad` = "pass_3mad_threshold"
	)
	
	sample_flags %>%
		dplyr::filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
		dplyr::group_by(Protein, .data[[var]]) %>%
		dplyr::summarise(
			n_samples = dplyr::n(),
			n_positive = sum(.data[[pass_col]], na.rm = TRUE),
			penetrance_percent = 100 * n_positive / n_samples,
			baseline_rfu_median = dplyr::first(baseline_rfu_median),
			fc_threshold_rfu = dplyr::first(fc_threshold_rfu),
			baseline_rfu_plus_2mad = dplyr::first(baseline_rfu_plus_2mad),
			baseline_rfu_plus_3mad = dplyr::first(baseline_rfu_plus_3mad),
			.groups = "drop"
		) %>%
		dplyr::rename(Group = !!rlang::sym(var)) %>%
		dplyr::arrange(Protein, Group)
}

.pfc_v2_pass_col <- function(threshold_method) {
	threshold_method <- match.arg(
		threshold_method,
		choices = c("fc", "2mad", "3mad")
	)
	
	switch(
		threshold_method,
		fc = "pass_fc_threshold",
		`2mad` = "pass_2mad_threshold",
		`3mad` = "pass_3mad_threshold"
	)
}

.pfc_v2_validate_group_var <- function(sample_flags, var) {
	if (is.null(var) || !is.character(var) || length(var) != 1 || !nzchar(var)) {
		stop("`var` must be a single non-empty character string.", call. = FALSE)
	}
	
	if (!var %in% colnames(sample_flags)) {
		stop("`var` not found in `sample_flags`: ", var, call. = FALSE)
	}
	
	invisible(TRUE)
}


pFC_fisher_one_vs_rest_v2 <- function(sample_flags,
																			var,
																			threshold_method = c("fc", "2mad", "3mad"),
																			p_adjust_method = "BH") {
	
	.pfc_v2_validate_group_var(sample_flags, var)
	pass_col <- .pfc_v2_pass_col(threshold_method)
	
	test_data <- sample_flags %>%
		dplyr::filter(!is.na(.data[[var]]), .data[[var]] != "")
	
	groups <- sort(unique(as.character(test_data[[var]])))
	proteins <- unique(test_data$Protein)
	
	results <- lapply(proteins, function(protein_i) {
		prot_df <- test_data[test_data$Protein == protein_i, , drop = FALSE]
		
		lapply(groups, function(group_i) {
			in_group <- as.character(prot_df[[var]]) == group_i
			positive <- as.logical(prot_df[[pass_col]])
			positive[is.na(positive)] <- FALSE
			
			tbl <- matrix(
				c(
					sum(in_group & positive),
					sum(in_group & !positive),
					sum(!in_group & positive),
					sum(!in_group & !positive)
				),
				nrow = 2,
				byrow = TRUE,
				dimnames = list(
					c("group", "rest"),
					c("positive", "negative")
				)
			)
			
			ft <- stats::fisher.test(tbl)
			
			tibble::tibble(
				Protein = protein_i,
				Group = group_i,
				test = "fisher_one_vs_rest",
				comparison = paste0(group_i, "_vs_rest"),
				group_positive = tbl["group", "positive"],
				group_negative = tbl["group", "negative"],
				rest_positive = tbl["rest", "positive"],
				rest_negative = tbl["rest", "negative"],
				odds_ratio = unname(ft$estimate),
				conf_low = ft$conf.int[1],
				conf_high = ft$conf.int[2],
				p_value = ft$p.value
			)
		}) %>% dplyr::bind_rows()
	}) %>% dplyr::bind_rows()
	
	results %>%
		dplyr::group_by(Group) %>%
		dplyr::mutate(p_adj = stats::p.adjust(p_value, method = p_adjust_method)) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(threshold_method = threshold_method[1])
}

pFC_chisq_global_v2 <- function(sample_flags,
																var,
																threshold_method = c("fc", "2mad", "3mad"),
																p_adjust_method = "BH") {
	
	.pfc_v2_validate_group_var(sample_flags, var)
	pass_col <- .pfc_v2_pass_col(threshold_method)
	
	test_data <- sample_flags %>%
		dplyr::filter(!is.na(.data[[var]]), .data[[var]] != "")
	
	proteins <- unique(test_data$Protein)
	
	results <- lapply(proteins, function(protein_i) {
		prot_df <- test_data[test_data$Protein == protein_i, , drop = FALSE]
		positive <- as.logical(prot_df[[pass_col]])
		positive[is.na(positive)] <- FALSE
		
		tbl <- table(as.character(prot_df[[var]]), positive)
		chisq_res <- tryCatch(stats::chisq.test(tbl), error = function(e) NULL)
		
		tibble::tibble(
			Protein = protein_i,
			test = "chisq_all_groups",
			comparison = "all_groups",
			chi_sq_statistic = if (is.null(chisq_res)) NA_real_ else unname(chisq_res$statistic),
			chi_sq_df = if (is.null(chisq_res)) NA_real_ else unname(chisq_res$parameter),
			p_value = if (is.null(chisq_res)) NA_real_ else chisq_res$p.value
		)
	}) %>% dplyr::bind_rows()
	
	results %>%
		dplyr::mutate(
			p_adj = stats::p.adjust(p_value, method = p_adjust_method),
			threshold_method = threshold_method[1]
		)
}

pFC_logistic_tests_v2 <- function(sample_flags,
																	var,
																	threshold_method = c("fc", "2mad", "3mad"),
																	p_adjust_method = "BH") {
	
	.pfc_v2_validate_group_var(sample_flags, var)
	pass_col <- .pfc_v2_pass_col(threshold_method)
	
	test_data <- sample_flags %>%
		dplyr::filter(!is.na(.data[[var]]), .data[[var]] != "")
	
	groups <- sort(unique(as.character(test_data[[var]])))
	proteins <- unique(test_data$Protein)
	
	group_results <- list()
	global_results <- list()
	
	for (protein_i in proteins) {
		prot_df <- test_data[test_data$Protein == protein_i, , drop = FALSE]
		prot_df$positive <- as.integer(as.logical(prot_df[[pass_col]]))
		prot_df$positive[is.na(prot_df$positive)] <- 0L
		prot_df$group_var <- as.factor(as.character(prot_df[[var]]))
		
		global_fit <- tryCatch(
			stats::glm(positive ~ group_var, family = stats::binomial(), data = prot_df),
			error = function(e) NULL
		)
		null_fit <- tryCatch(
			stats::glm(positive ~ 1, family = stats::binomial(), data = prot_df),
			error = function(e) NULL
		)
		
		global_p <- NA_real_
		if (!is.null(global_fit) && !is.null(null_fit)) {
			lrt <- tryCatch(stats::anova(null_fit, global_fit, test = "Chisq"), error = function(e) NULL)
			if (!is.null(lrt) && nrow(lrt) >= 2) {
				global_p <- lrt$`Pr(>Chi)`[2]
			}
		}
		
		global_results[[protein_i]] <- tibble::tibble(
			Protein = protein_i,
			test = "logistic_all_groups",
			comparison = "all_groups",
			p_value = global_p
		)
		
		group_results[[protein_i]] <- lapply(groups, function(group_i) {
			prot_df$target_group <- factor(
				ifelse(as.character(prot_df[[var]]) == group_i, group_i, "rest"),
				levels = c("rest", group_i)
			)
			
			fit <- tryCatch(
				stats::glm(positive ~ target_group, family = stats::binomial(), data = prot_df),
				error = function(e) NULL
			)
			
			if (is.null(fit)) {
				return(tibble::tibble(
					Protein = protein_i,
					Group = group_i,
					test = "logistic_one_vs_rest",
					comparison = paste0(group_i, "_vs_rest"),
					estimate = NA_real_,
					std_error = NA_real_,
					statistic = NA_real_,
					p_value = NA_real_,
					odds_ratio = NA_real_
				))
			}
			
			coef_tab <- summary(fit)$coefficients
			row_i <- paste0("target_group", group_i)
			
			if (!row_i %in% rownames(coef_tab)) {
				return(tibble::tibble(
					Protein = protein_i,
					Group = group_i,
					test = "logistic_one_vs_rest",
					comparison = paste0(group_i, "_vs_rest"),
					estimate = NA_real_,
					std_error = NA_real_,
					statistic = NA_real_,
					p_value = NA_real_,
					odds_ratio = NA_real_
				))
			}
			
			tibble::tibble(
				Protein = protein_i,
				Group = group_i,
				test = "logistic_one_vs_rest",
				comparison = paste0(group_i, "_vs_rest"),
				estimate = coef_tab[row_i, "Estimate"],
				std_error = coef_tab[row_i, "Std. Error"],
				statistic = coef_tab[row_i, "z value"],
				p_value = coef_tab[row_i, "Pr(>|z|)"],
				odds_ratio = unname(exp(coef_tab[row_i, "Estimate"]))
			)
		}) %>% dplyr::bind_rows()
	}
	
	group_stats <- dplyr::bind_rows(group_results) %>%
		dplyr::group_by(Group) %>%
		dplyr::mutate(p_adj = stats::p.adjust(p_value, method = p_adjust_method)) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(threshold_method = threshold_method[1])
	
	global_stats <- dplyr::bind_rows(global_results) %>%
		dplyr::mutate(
			p_adj = stats::p.adjust(p_value, method = p_adjust_method),
			threshold_method = threshold_method[1]
		)
	
	list(
		group_stats = group_stats,
		global_stats = global_stats
	)
}


pFC_firth_tests_v2 <- function(sample_flags,
															 var,
															 threshold_method = c("fc", "2mad", "3mad"),
															 p_adjust_method = "BH") {
	
	.pfc_v2_validate_group_var(sample_flags, var)
	pass_col <- .pfc_v2_pass_col(threshold_method)
	
	test_data <- sample_flags %>%
		dplyr::filter(!is.na(.data[[var]]), .data[[var]] != "")
	
	groups <- sort(unique(as.character(test_data[[var]])))
	proteins <- unique(test_data$Protein)
	
	if (!requireNamespace("logistf", quietly = TRUE)) {
		results <- expand.grid(
			Protein = proteins,
			Group = groups,
			stringsAsFactors = FALSE
		)
		
		return(tibble::as_tibble(results) %>%
					 	dplyr::mutate(
					 		test = "firth_one_vs_rest",
					 		comparison = paste0(Group, "_vs_rest"),
					 		estimate = NA_real_,
					 		std_error = NA_real_,
					 		statistic = NA_real_,
					 		p_value = NA_real_,
					 		odds_ratio = NA_real_,
					 		p_adj = NA_real_,
					 		threshold_method = threshold_method[1],
					 		note = "Package `logistf` not installed"
					 	))
	}
	
	results <- lapply(proteins, function(protein_i) {
		prot_df <- test_data[test_data$Protein == protein_i, , drop = FALSE]
		prot_df$positive <- as.integer(as.logical(prot_df[[pass_col]]))
		prot_df$positive[is.na(prot_df$positive)] <- 0L
		
		lapply(groups, function(group_i) {
			prot_df$target_group <- factor(
				ifelse(as.character(prot_df[[var]]) == group_i, group_i, "rest"),
				levels = c("rest", group_i)
			)
			
			fit <- tryCatch(
				logistf::logistf(positive ~ target_group, data = prot_df),
				error = function(e) NULL
			)
			
			if (is.null(fit)) {
				return(tibble::tibble(
					Protein = protein_i,
					Group = group_i,
					test = "firth_one_vs_rest",
					comparison = paste0(group_i, "_vs_rest"),
					estimate = NA_real_,
					std_error = NA_real_,
					statistic = NA_real_,
					p_value = NA_real_,
					odds_ratio = NA_real_,
					note = "Model fit failed"
				))
			}
			
			row_i <- grep("^target_group", names(fit$coefficients), value = TRUE)
			if (length(row_i) != 1) {
				return(tibble::tibble(
					Protein = protein_i,
					Group = group_i,
					test = "firth_one_vs_rest",
					comparison = paste0(group_i, "_vs_rest"),
					estimate = NA_real_,
					std_error = NA_real_,
					statistic = NA_real_,
					p_value = NA_real_,
					odds_ratio = NA_real_,
					note = "Coefficient not found"
				))
			}
			
			row_idx <- match(row_i, names(fit$coefficients))
			
			tibble::tibble(
				Protein = protein_i,
				Group = group_i,
				test = "firth_one_vs_rest",
				comparison = paste0(group_i, "_vs_rest"),
				estimate = fit$coefficients[row_idx],
				std_error = fit$se[row_idx],
				statistic = fit$coefficients[row_idx] / fit$se[row_idx],
				p_value = fit$prob[row_idx],
				odds_ratio = unname(exp(fit$coefficients[row_idx])),
				note = NA_character_
			)
		}) %>% dplyr::bind_rows()
	}) %>% dplyr::bind_rows()
	
	results %>%
		dplyr::group_by(Group) %>%
		dplyr::mutate(p_adj = stats::p.adjust(p_value, method = p_adjust_method)) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(threshold_method = threshold_method[1])
}

pFC_penetrance_test_suite_v2 <- function(sample_flags,
																				 var,
																				 threshold_method = c("fc", "2mad", "3mad"),
																				 p_adjust_method = "BH") {
	
	threshold_method <- match.arg(threshold_method, choices = c("fc", "2mad", "3mad"))
	
	master_penetrance <- pFC_penetrance_by_group_v2(
		sample_flags = sample_flags,
		var = var,
		threshold_method = threshold_method
	)
	
	fisher_stats <- pFC_fisher_one_vs_rest_v2(
		sample_flags = sample_flags,
		var = var,
		threshold_method = threshold_method,
		p_adjust_method = p_adjust_method
	)
	
	chisq_stats <- pFC_chisq_global_v2(
		sample_flags = sample_flags,
		var = var,
		threshold_method = threshold_method,
		p_adjust_method = p_adjust_method
	)
	
	logistic_stats <- pFC_logistic_tests_v2(
		sample_flags = sample_flags,
		var = var,
		threshold_method = threshold_method,
		p_adjust_method = p_adjust_method
	)
	
	firth_stats <- pFC_firth_tests_v2(
		sample_flags = sample_flags,
		var = var,
		threshold_method = threshold_method,
		p_adjust_method = p_adjust_method
	)
	
	master_group_stats <- master_penetrance %>%
		dplyr::left_join(
			fisher_stats %>%
				dplyr::select(Protein, Group, fisher_p_value = p_value, fisher_p_adj = p_adj, fisher_odds_ratio = odds_ratio),
			by = c("Protein", "Group")
		) %>%
		dplyr::left_join(
			logistic_stats$group_stats %>%
				dplyr::select(Protein, Group, logistic_p_value = p_value, logistic_p_adj = p_adj, logistic_odds_ratio = odds_ratio),
			by = c("Protein", "Group")
		) %>%
		dplyr::left_join(
			firth_stats %>%
				dplyr::select(Protein, Group, firth_p_value = p_value, firth_p_adj = p_adj, firth_odds_ratio = odds_ratio),
			by = c("Protein", "Group")
		)
	
	master_global_stats <- master_penetrance %>%
		dplyr::group_by(Protein) %>%
		dplyr::summarise(
			max_penetrance_percent = max(penetrance_percent, na.rm = TRUE),
			n_groups = dplyr::n_distinct(Group),
			threshold_method = dplyr::first(threshold_method),
			.groups = "drop"
		) %>%
		dplyr::left_join(
			chisq_stats %>%
				dplyr::select(Protein, chisq_p_value = p_value, chisq_p_adj = p_adj, chi_sq_statistic, chi_sq_df),
			by = "Protein"
		) %>%
		dplyr::left_join(
			logistic_stats$global_stats %>%
				dplyr::select(Protein, logistic_global_p_value = p_value, logistic_global_p_adj = p_adj),
			by = "Protein"
		)
	
	list(
		master_penetrance = master_penetrance,
		master_group_stats = master_group_stats,
		master_global_stats = master_global_stats,
		fisher_stats = fisher_stats,
		chisq_stats = chisq_stats,
		logistic_group_stats = logistic_stats$group_stats,
		logistic_global_stats = logistic_stats$global_stats,
		firth_group_stats = firth_stats
	)
}

pFC_v2_results_guide_ui <- function() {
	shiny::tagList(
		shiny::tags$div(
			class = "alert alert-info",
			shiny::tags$strong("How to read the pFC v2 result tables"),
			shiny::tags$ul(
				shiny::tags$li(
					shiny::tags$strong("master_penetrance: "),
					"Per protein and per group summary of threshold-positive samples. `penetrance_percent` is the percent of samples above the selected threshold."
				),
				shiny::tags$li(
					shiny::tags$strong("master_group_stats: "),
					"Adds one-vs-rest Fisher, logistic, and Firth p-values to each Protein x Group row. Use these to identify which specific group is enriched."
				),
				shiny::tags$li(
					shiny::tags$strong("master_global_stats: "),
					"Protein-level overview with maximum penetrance plus global chi-squared and logistic p-values. Use this to screen proteins that differ anywhere across groups."
				),
				shiny::tags$li(
					shiny::tags$strong("fisher_stats: "),
					"Exact one-vs-rest enrichment test. Best for small sample sizes and sparse positives."
				),
				shiny::tags$li(
					shiny::tags$strong("chisq_stats: "),
					"Global test across all groups. Good for overall heterogeneity, but less reliable with low counts."
				),
				shiny::tags$li(
					shiny::tags$strong("logistic_group/global_stats: "),
					"Model-based tests for one-vs-rest and all-groups association. These are easiest to extend later with covariates."
				),
				shiny::tags$li(
					shiny::tags$strong("firth_group_stats: "),
					"Bias-reduced logistic regression for sparse or separated data. Prefer this when ordinary logistic regression is unstable."
				),
				shiny::tags$li(
					shiny::tags$strong("p_adj columns: "),
					"Benjamini-Hochberg adjusted p-values within each test family. Use these for ranking and filtering rather than raw p-values alone."
				)
			)
		)
	)
}





#' Generate Plots for pFC Analysis
#'
#' Creates violin plots and heatmaps from pFC analysis results
#'
#' @param pfc_results List output from pFC_process()
#' @param plot_width Numeric; width for heatmap plots (default: 15)
#' @param plot_height Numeric; height for heatmap plots (default: 10)
#' @param add_anno Character vector; additional annotation columns from metadata
#' @param violin_ncol Integer; number of columns for violin plot facets (default: 3)
#' @param violin_nrow Integer; number of rows per page for violin plots (default: 3)
#'
#' @return List containing:
#' \itemize{
#'   \item violin_plots: List of ggplot violin plot objects
#'   \item heatmap_manual: Pheatmap object (manual sort)
#'   \item heatmap_manual_RC: Pheatmap object (manual sort, row-centered)
#'   \item heatmap_clustered_RC: Pheatmap object (clustered, row-centered)
#'   \item heatmap_data: List with data and metadata for heatmaps
#' }
#'
#' @export
pFC_plot <- function(pfc_results,
										 plot_width = 15,
										 plot_height = 10,
										 add_anno = NULL,
										 violin_ncol = 3,
										 violin_nrow = 3) {
	
	# Extract necessary data from results
	sig_stats <- pfc_results$pfc_significant
	metadata <- pfc_results$metadata
	input_data <- pfc_results$input_data
	merge.df <- pfc_results$merge_data
	params <- pfc_results$parameters
	
	var <- params$var
	groupPos <- params$groupPos
	groupNeg <- params$groupNeg
	
	plot_list <- list()
	
	# ---- Violin Plots ----
	if (nrow(sig_stats) > 0) {
		sig_features <- sig_stats$Protein
		
		# Prepare data for violin plots
		violin_df <- merge.df %>%
			dplyr::filter(Protein %in% sig_features) %>%
			dplyr::select(Sample_ID, Protein, !!rlang::sym(var), value)
		
		# Determine color scheme
		n_levels <- nlevels(as.factor(metadata[[var]]))
		if (n_levels == 2) {
			violin_cols <- c("#009E73", "#BEAED4")
		} else {
			violin_cols <- c("#009E73", "#BEAED4", "#80B1D3")
		}
		
		# Create base violin plot
		violin_base <- ggplot(violin_df, aes(x = .data[[var]], y = value, color = .data[[var]])) +
			geom_violin(alpha = 0.5) +
			scale_colour_manual(values = violin_cols) +
			scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
			theme_minimal() +
			geom_point(position = position_jitter(seed = 1, width = 0.2)) +
			theme(legend.position = "none") +
			labs(y = "Log2 Intensity") +
			facet_wrap_paginate(~ Protein, ncol = violin_ncol, nrow = violin_nrow, scales = "free")
		
		# Generate paginated plots
		n_pages <- ggforce::n_pages(violin_base)
		violin_plots <- lapply(1:n_pages, function(i) {
			violin_base + facet_wrap_paginate(~ Protein, ncol = violin_ncol, nrow = violin_nrow, 
																				page = i, scales = "free")
		})
		
		plot_list$violin_plots <- violin_plots
	} else {
		plot_list$violin_plots <- NULL
	}
	
	# ---- Heatmaps ----
	if (nrow(sig_stats) > 2) {
		sig_features <- sig_stats$Protein
		
		# Prepare metadata for annotation
		if (is.null(add_anno)) {
			meta_anno <- metadata[, var, drop = FALSE]
		} else {
			cols_anno <- unique(c(var, add_anno))
			meta_anno <- metadata[, cols_anno, drop = FALSE]
		}
		
		# Set factor levels for proper ordering
		get_groups <- c(groupPos, groupNeg)
		meta_anno[[var]] <- factor(meta_anno[[var]], levels = get_groups)
		
		# Subset data to significant features
		data_sig <- input_data[sig_features, rownames(meta_anno), drop = FALSE]
		
		# Row-center data
		data_RC <- t(scale(t(input_data), scale = FALSE, center = TRUE))
		data_sig_RC <- data_RC[sig_features, rownames(meta_anno), drop = FALSE]
		
		# Manual sort by group
		toplot <- NULL
		toplot_RC <- NULL
		
		for (j in 1:length(get_groups)) {
			# For non-row-centered data
			ns <- data_sig[, which(meta_anno[[var]] == get_groups[j]), drop = FALSE]
			if (ncol(ns) > 1) {
				ns_clust <- hclust(vegan::vegdist(t(ns), method = "euclidean"))
				order_samples <- ns_clust$labels[ns_clust$order]
			} else {
				order_samples <- colnames(ns)
			}
			
			if (j == 1) {
				toplot <- ns[, order_samples, drop = FALSE]
			} else {
				toplot <- cbind(toplot, ns[, order_samples, drop = FALSE])
			}
			
			# For row-centered data
			ns_RC <- data_sig_RC[, which(meta_anno[[var]] == get_groups[j]), drop = FALSE]
			if (ncol(ns_RC) > 1) {
				ns_RC_clust <- hclust(vegan::vegdist(t(ns_RC), method = "euclidean"))
				order_samples_RC <- ns_RC_clust$labels[ns_RC_clust$order]
			} else {
				order_samples_RC <- colnames(ns_RC)
			}
			
			if (j == 1) {
				toplot_RC <- ns_RC[, order_samples_RC, drop = FALSE]
			} else {
				toplot_RC <- cbind(toplot_RC, ns_RC[, order_samples_RC, drop = FALSE])
			}
		}
		
		# Prepare metadata for heatmap annotation
		meta_plot <- meta_anno[colnames(toplot), , drop = FALSE]
		meta_plot_RC <- meta_anno[colnames(toplot_RC), , drop = FALSE]
		
		# Calculate gap position
		gap_tab <- table(meta_anno[[var]])
		gap_finder <- gap_tab[1]
		
		# Get annotation colors
		if (is.null(add_anno)) {
			anno_col <- color_distinct(meta_colors = meta_plot, variables = 1)
		} else {
			anno_col <- color_distinct(meta_colors = meta_plot, variables = 1:ncol(meta_plot))
		}
		
		# Generate heatmaps (return as objects, not save)
		heatmap_manual <- pheatmap::pheatmap(
			toplot,
			gaps_col = gap_finder,
			annotation_col = meta_plot,
			annotation_colors = anno_col,
			cluster_cols = FALSE,
			show_rownames = TRUE,
			show_colnames = FALSE,
			silent = TRUE
		)
		
		heatmap_manual_RC <- pheatmap::pheatmap(
			toplot_RC,
			gaps_col = gap_finder,
			annotation_col = meta_plot_RC,
			annotation_colors = anno_col,
			cluster_cols = FALSE,
			show_rownames = TRUE,
			show_colnames = FALSE,
			silent = TRUE
		)
		
		heatmap_clustered_RC <- pheatmap::pheatmap(
			toplot_RC,
			annotation_col = meta_plot_RC,
			annotation_colors = anno_col,
			cluster_cols = TRUE,
			show_rownames = TRUE,
			show_colnames = FALSE,
			silent = TRUE
		)
		
		plot_list$heatmap_manual <- heatmap_manual
		plot_list$heatmap_manual_RC <- heatmap_manual_RC
		plot_list$heatmap_clustered_RC <- heatmap_clustered_RC
		
		# Store heatmap data for custom rendering if needed
		plot_list$heatmap_data <- list(
			toplot = toplot,
			toplot_RC = toplot_RC,
			meta_plot = meta_plot,
			meta_plot_RC = meta_plot_RC,
			anno_col = anno_col,
			gap_finder = gap_finder
		)
	} else {
		plot_list$heatmap_manual <- NULL
		plot_list$heatmap_manual_RC <- NULL
		plot_list$heatmap_clustered_RC <- NULL
		plot_list$heatmap_data <- NULL
	}
	
	return(plot_list)
}

#' Save pFC Analysis Results and Plots
#'
#' Saves all tables and plots from pFC analysis to disk
#'
#' @param pfc_results List output from pFC_process()
#' @param pfc_plots List output from pFC_plot()
#' @param descriptor Character string; output directory path OR base filename descriptor
#' @param plot_width Numeric; width for saved plots (default: 15)
#' @param plot_height Numeric; height for saved plots (default: 10)
#'
#' @return Invisible NULL (files are saved to disk)
#'
#' @export
pFC_save <- function(pfc_results,
										 pfc_plots,
										 output_dir,
										 file_prefix,
										 plot_width = 15,
										 plot_height = 10) {
	
	if (missing(output_dir) || is.null(output_dir) || !nzchar(output_dir)) {
		stop("`output_dir` is required.", call. = FALSE)
	}
	
	if (missing(file_prefix) || is.null(file_prefix) || !nzchar(file_prefix)) {
		stop("`file_prefix` is required.", call. = FALSE)
	}
	
	file_prefix <- trimws(file_prefix)
	file_prefix <- gsub("[^A-Za-z0-9_-]", "_", file_prefix)
	
	if (!dir.exists(output_dir)) {
		dir.create(output_dir, recursive = TRUE)
	}
	
	p_val <- pfc_results$parameters$p_val
	
	write.csv(
		pfc_results$normalized_data,
		file.path(output_dir, paste0("pFC_Normalised_NetI_nonlog2_", file_prefix, ".csv")),
		row.names = FALSE
	)
	
	write.csv(
		pfc_results$fc_data,
		file.path(output_dir, paste0("pFC_per_feature_per_sample_FC_vs_groupNeg_mean_", file_prefix, ".csv")),
		row.names = FALSE
	)
	
	write.csv(
		pfc_results$pfc_stats,
		file.path(output_dir, paste0("pFC_all_stats_", file_prefix, ".csv")),
		row.names = FALSE
	)
	
	write.csv(
		pfc_results$pfc_significant,
		file.path(output_dir, paste0("pFC_Fishers_exact_p_", p_val, file_prefix, ".csv")),
		row.names = FALSE
	)
	
	...
}

# pFC_save <- function(pfc_results,
# 										 pfc_plots,
# 										 descriptor,
# 										 plot_width = 15,
# 										 plot_height = 10) {
# 	
# 	# Separate directory and base name ----
# 	# If descriptor is a path, extract directory and basename
# 	if (dirname(descriptor) != ".") {
# 		output_dir <- descriptor
# 		base_name <- basename(descriptor)
# 	} else {
# 		# descriptor is just a name, not a path
# 		output_dir <- descriptor
# 		base_name <- descriptor
# 	}
# 	
# 	# Create output directory
# 	if (!dir.exists(output_dir)) {
# 		dir.create(output_dir, recursive = TRUE)
# 	}
# 	
# 	params <- pfc_results$parameters
# 	p_val <- params$p_val
# 	
# 	message("Saving results to: ", output_dir)
# 	
# 	# ---- Save Tables ----
# 	
# 	# Normalized data (non-log2)
# 	write.csv(
# 		pfc_results$normalized_data,
# 		file.path(output_dir, paste0("pFC_Normalised_NetI_nonlog2_", base_name, ".csv")),
# 		row.names = FALSE
# 	)
# 	
# 	# Fold change data
# 	write.csv(
# 		pfc_results$fc_data,
# 		file.path(output_dir, paste0("pFC_per_feature_per_sample_FC_vs_groupNeg_mean_", base_name, ".csv")),
# 		row.names = FALSE
# 	)
# 	
# 	# All statistics
# 	write.csv(
# 		pfc_results$pfc_stats,
# 		file.path(output_dir, paste0("pFC_all_stats_", base_name, ".csv")),
# 		row.names = FALSE
# 	)
# 	
# 	# Significant results
# 	write.csv(
# 		pfc_results$pfc_significant,
# 		file.path(output_dir, paste0("pFC_Fishers_exact_p_", p_val, "_", base_name, ".csv")),
# 		row.names = FALSE
# 	)
# 	
# 	message("✓ Saved ", nrow(pfc_results$pfc_stats), " features to CSV files")
# 	
# 	# ---- Save Plots ----
# 	
# 	# Violin plots
# 	if (!is.null(pfc_plots$violin_plots) && length(pfc_plots$violin_plots) > 0) {
# 		pdf(
# 			file.path(output_dir, paste0("violin_pFC_", p_val, "_", base_name, ".pdf")),
# 			width = plot_width,
# 			height = plot_height
# 		)
# 		for (p in pfc_plots$violin_plots) {
# 			print(p)
# 		}
# 		dev.off()
# 		message("✓ Saved violin plots")
# 	}
# 	
# 	# Heatmaps
# 	if (!is.null(pfc_plots$heatmap_manual)) {
# 		# Manual sort
# 		pdf(
# 			file.path(output_dir, paste0("heatmap_", p_val, "_", base_name, "_manualsort.pdf")),
# 			width = plot_width,
# 			height = plot_height
# 		)
# 		print(pfc_plots$heatmap_manual)
# 		dev.off()
# 		
# 		# Manual sort, row-centered
# 		pdf(
# 			file.path(output_dir, paste0("heatmap_", p_val, "_", base_name, "_manualsort_RC.pdf")),
# 			width = plot_width,
# 			height = plot_height
# 		)
# 		print(pfc_plots$heatmap_manual_RC)
# 		dev.off()
# 		
# 		# Clustered, row-centered
# 		pdf(
# 			file.path(output_dir, paste0("heatmap_", p_val, "_", base_name, "_RC.pdf")),
# 			width = plot_width,
# 			height = plot_height
# 		)
# 		print(pfc_plots$heatmap_clustered_RC)
# 		dev.off()
# 		
# 		message("✓ Saved heatmaps")
# 	}
# 	
# 	message("\n✓ All pFC results saved successfully!")
# 	message("  Output directory: ", normalizePath(output_dir))
# 	
# 	invisible(NULL)
# }

#' Penetrance Fold Change (pFC) Analysis - Pipeline Wrapper
#'
#' Wrapper function for pipeline compatibility. Maintains exact functionality
#' of the original pFC.func() while using the new modular structure.
#'
#' @param input Matrix or data frame; loess normalized, log2-transformed data
#' @param metadata Data frame; metadata with rownames matching colnames of input
#' @param var Character string; variable of interest
#' @param groupPos Character string; positive group name (e.g., 'case')
#' @param groupNeg Character string; negative group name (e.g., 'control')
#' @param fold_change Numeric; FC threshold (default: 2)
#' @param descriptor Character string; output directory/file descriptor
#' @param p_val Numeric; p-value threshold for Fisher's test (default: 0.2)
#' @param plot_width Numeric; plot width (default: 15)
#' @param plot_height Numeric; plot height (default: 10)
#' @param add_anno Character vector; additional annotation variables
#' @param PSA_flag Logical; flag PSA-associated antigens (default: TRUE)
#' @param PSA_colname Character string; PSA column name (default: "PSA_class")
#' @param cores Integer; number of cores for parallel processing (default: 1)
#'
#' @return Invisible list containing pfc_results and pfc_plots
#'
#' @export
pFC_analysis <- function(input,
												 metadata = NULL,
												 var = NULL,
												 groupPos = NULL,
												 groupNeg = NULL,
												 fold_change = 2,
												 descriptor,
												 p_val = 0.2,
												 plot_width = 15,
												 plot_height = 10,
												 add_anno = NULL,
												 PSA_flag = TRUE,
												 PSA_colname = "PSA_class",
												 cores = 1) {
	
	# Build ExpressionSet from input matrix and metadata
	# Ensure input is a matrix
	if (!is.matrix(input)) {
		input <- as.matrix(input)
	}
	
	# Ensure metadata rownames match input colnames
	if (!all(colnames(input) %in% rownames(metadata))) {
		stop("Metadata rownames must match input colnames")
	}
	
	# Match order
	metadata <- metadata[colnames(input), , drop = FALSE]
	
	# Create AnnotatedDataFrame for phenotype data
	pheno_data <- Biobase::AnnotatedDataFrame(data = metadata)
	
	# Create ExpressionSet
	eset <- Biobase::ExpressionSet(
		assayData = input,
		phenoData = pheno_data
	)
	
	# Run processing
	pfc_results <- pFC_process(
		eset = eset,
		var = var,
		groupPos = groupPos,
		groupNeg = groupNeg,
		fold_change = fold_change,
		p_val = p_val,
		PSA_flag = PSA_flag,
		PSA_colname = PSA_colname,
		cores = cores
	)
	
	# Generate plots
	pfc_plots <- pFC_plot(
		pfc_results = pfc_results,
		plot_width = plot_width,
		plot_height = plot_height,
		add_anno = add_anno
	)
	
	# Save results
	pFC_save(
		pfc_results = pfc_results,
		pfc_plots = pfc_plots,
		descriptor = descriptor,
		plot_width = plot_width,
		plot_height = plot_height
	)
	
	# Return invisibly for programmatic access if needed
	invisible(list(
		results = pfc_results,
		plots = pfc_plots
	))
}

#' Penetrance Association Analysis via Fisher's Exact Test
#'
#' Performs parallel Fisher's exact test for each feature to calculate odds ratios,
#' confidence intervals, and p-values for association with class membership.
#' This is the core statistical function for penetrance fold-change (pFC) analysis.
#'
#' @param obj Matrix, data.frame, or ExpressionSet containing expression data.
#'   Features as rows, samples as columns. Will be converted to binary based on threshold.
#' @param cl Factor with exactly 2 levels defining sample classes (e.g., case vs control).
#'   Length must match number of columns in obj.
#' @param thres Numeric threshold for binarization. Values > thres become 1 (present),
#'   values <= thres become 0 (absent). Default: 0.
#' @param adjust.method Character string specifying p-value adjustment method.
#'   Passed to \code{\link[stats]{p.adjust}}. Default: "fdr" (Benjamini-Hochberg).
#'   Options include "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".
#' @param cores Integer specifying number of CPU cores to use for parallel processing.
#'   Default: 1. Passed to \code{\link[parallel]{makeCluster}}.
#' @param ... Additional arguments passed to \code{\link[parallel]{makeCluster}}.
#'
#' @return Data frame with one row per feature containing:
#'   \describe{
#'     \item{oddsRatio}{Odds ratio from Fisher's exact test}
#'     \item{lower}{Lower bound of 95% confidence interval for odds ratio}
#'     \item{upper}{Upper bound of 95% confidence interval for odds ratio}
#'     \item{pvalues}{Raw p-values from Fisher's exact test}
#'     \item{adjPvalues}{Adjusted p-values using specified method}
#'   }
#'   Rownames match input feature names (or indices if none provided).
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Converts expression data to binary (present/absent) based on threshold
#'   \item For each feature, constructs 2x2 contingency table (present/absent × class1/class2)
#'   \item Performs two-sided Fisher's exact test in parallel
#'   \item Adjusts p-values for multiple testing
#' }
#'
#' The odds ratio represents the odds of a feature being present in class 1 relative to class 2.
#' Values > 1 indicate enrichment in class 1, < 1 indicate enrichment in class 2.
#'
#' @note Original location: pFC_script.R (Sengenics Analysis-Pipeline)
#' @note This function requires the helper function \code{returnAppropriateObj} to be available
#'
#' @importFrom parallel makeCluster stopCluster parRapply
#' @importFrom stats fisher.test p.adjust
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic example with matrix input
#' data_matrix <- matrix(rnorm(100 * 20), nrow = 100, ncol = 20)
#' rownames(data_matrix) <- paste0("Feature_", 1:100)
#' sample_class <- factor(rep(c("Case", "Control"), each = 10))
#'
#' # Run penetrance analysis with default threshold
#' results <- fitPA_StandAlone(
#'   obj = data_matrix,
#'   cl = sample_class,
#'   thres = 0,
#'   adjust.method = "fdr",
#'   cores = 2
#' )
#'
#' # View top results by adjusted p-value
#' head(results[order(results$adjPvalues), ])
#'
#' # Filter significant results (adj p < 0.05, OR > 2)
#' significant <- results[results$adjPvalues < 0.05 & results$oddsRatio > 2, ]
#'
#' # Example with ExpressionSet
#' library(Biobase)
#' eset <- ExpressionSet(assayData = data_matrix)
#' pData(eset)$Group <- sample_class
#'
#' results_eset <- fitPA_StandAlone(
#'   obj = eset,
#'   cl = pData(eset)$Group,
#'   thres = median(exprs(eset)),  # Use median as threshold
#'   cores = 4
#' )
#' }
#'
#' @seealso
#' \code{\link[stats]{fisher.test}} for Fisher's exact test
#' \code{\link[stats]{p.adjust}} for multiple testing correction
#' \code{\link[parallel]{makeCluster}} for parallel processing setup
#' @export
fitPA_StandAlone <- function(obj, cl, thres = 0, adjust.method = "fdr", cores = 1, ...) {
	
	# Convert object to appropriate format and binarize
	x <- returnAppropriateObj(obj, norm = FALSE, log = FALSE) > thres
	nrows <- nrow(x)
	
	# Add rownames if missing
	if (is.null(rownames(x))) {
		rownames(x) <- 1:nrows
	}
	
	# Count samples per class
	nClass1 <- sum(cl == levels(cl)[1])
	nClass2 <- sum(cl == levels(cl)[2])
	
	# Set up parallel cluster
	cores <- makeCluster(getOption("cl.cores", cores), ...)
	
	# Perform Fisher's exact test in parallel for each feature
	res <- parRapply(cl = cores, x, function(i) {
		# Construct 2x2 contingency table
		tbl <- table(1 - i, cl)
		
		# Handle edge case where table doesn't have full dimensions
		if (sum(dim(tbl)) != 4) {
			tbl <- array(0, dim = c(2, 2))
			tbl[1, 1] <- sum(i[cl == levels(cl)[1]])
			tbl[1, 2] <- sum(i[cl == levels(cl)[2]])
			tbl[2, 1] <- nClass1 - tbl[1, 1]
			tbl[2, 2] <- nClass2 - tbl[1, 2]
		}
		
		# Perform Fisher's exact test
		ft <- fisher.test(tbl, 
											workspace = 8e+06, 
											alternative = "two.sided", 
											conf.int = TRUE)
		
		# Return results as vector
		cbind(o = ft$estimate, 
					cl = ft$conf.int[1], 
					cu = ft$conf.int[2], 
					p = ft$p.value)
	})
	
	# Clean up parallel cluster
	stopCluster(cores)
	
	# Extract results from vectorized output
	nres <- nrows * 4
	seqs <- seq(1, nres, by = 4)
	p <- res[seqs + 3]
	adjp <- p.adjust(p, method = adjust.method)
	o <- res[seqs]
	cl <- res[seqs + 1]
	cu <- res[seqs + 2]
	
	# Construct results data frame
	res <- data.frame(cbind(o, cl, cu, p, adjp))
	colnames(res) <- c("oddsRatio", "lower", "upper", "pvalues", "adjPvalues")
	rownames(res) <- rownames(x)
	
	return(res)
}

#' Extract Expression Data in Appropriate Format
#'
#' Helper function to extract expression data from various object types
#' and optionally normalize and/or log-transform.
#'
#' @param obj Matrix, data.frame, ExpressionSet, or MRexperiment containing expression data.
#' @param norm Logical; should data be normalized? Default: FALSE.
#'   For MRexperiment objects, passed to MRcounts().
#' @param log Logical; should data be log-transformed? Default: FALSE.
#'   For MRexperiment objects, passed to MRcounts().
#' @param sl Numeric; scaling factor for MRexperiment objects. Default: 1000.
#'   Only used when obj is an MRexperiment.
#'
#' @return Numeric matrix with features as rows, samples as columns.
#'
#' @note Original location: pFC_script.R (Sengenics Analysis-Pipeline)
#' @note Supports both metagenomics (MRexperiment) and microarray (ExpressionSet) data
#'
#' @importFrom Biobase exprs
#' @keywords internal
#' @export
returnAppropriateObj <- function(obj, norm = FALSE, log = FALSE, sl = 1000) {
	
	# Handle different object types
	if (inherits(obj, "MRexperiment")) {
		# Metagenomics data (legacy support)
		if (!requireNamespace("metagenomeSeq", quietly = TRUE)) {
			stop("metagenomeSeq package required for MRexperiment objects")
		}
		mat <- metagenomeSeq::MRcounts(obj, norm = norm, log = log, sl = sl)
		
	} else if (inherits(obj, "ExpressionSet")) {
		# Microarray data (primary use case)
		mat <- Biobase::exprs(obj)
		
		# Apply log transformation if requested
		if (log) {
			mat <- log2(mat + 1)  # Add 1 to avoid log(0)
		}
		
		# Normalization would typically be done upstream for ExpressionSet
		if (norm) {
			warning("Normalization for ExpressionSet should be done upstream. Skipping.")
		}
		
	} else if (inherits(obj, "matrix")) {
		# Raw matrix
		mat <- obj
		
	} else if (inherits(obj, "data.frame")) {
		# Data frame
		mat <- as.matrix(obj)
		
	} else {
		stop("Object must be ExpressionSet, matrix, data.frame, or MRexperiment")
	}
	
	return(mat)
}


