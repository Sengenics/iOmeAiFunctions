#' Low-intensity filter (nMAD = 2) for feature RFU
#'
#' Computes per-sample log2 NetI summaries on feature rows, defines a low-intensity
#' RFU cutoff as median − 2 × MAD (on the log2 scale), converts that cutoff back
#' to RFU, and flags rows with `mean` below the cutoff by setting `source = "lowNetI"`.
#'
#' @param datCollate A list with `datCollate$data$Data` (data.frame) containing
#'   at least columns: `Sample`, `data`, `mean`, `source`. `datCollate$data$feature_df`
#'   may be present but is not required for this step.
#' @param feature_label Character. The label in column `data` that designates
#'   feature rows. Default is `"feature"`.
#' @param test Logical. If `TRUE`, builds a set of diagnostic ggplot objects
#'   (returned invisibly via an attribute). Default `FALSE`.
#' @param sample_select Optional character. Sample to highlight in diagnostics
#'   when `test = TRUE`. Defaults to the first sample in the data.
#'
#' @return The input `datCollate` with:
#' \itemize{
#'   \item `datCollate$data$Data` — updated with threshold columns and `source` flagged as `"lowNetI"` where applicable
#'   \item `datCollate$data$lowIntensityThresholds` — per-sample summary and thresholds
#' }
#'
#' @examples
#' \dontrun{
#'   datCollate <- lowIntensityFilterFunction(datCollate)
#' }
#'
#' @export
#' @importFrom dplyr %>% filter mutate group_by summarise ungroup left_join count
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_density geom_vline geom_jitter geom_col geom_text labs expand_limits
#' @importFrom forcats fct_reorder
#'
#' @note
#' Version 1.0.0 from
#' Pre-Processing functions.R
lowIntensityFilterFunction <- function(datCollate,
																			 feature_label = "feature",
																			 test = FALSE,
																			 sample_select = NULL) {
	
	# Pull Data and do some quick checks
	Data <- datCollate$data$Data
	if (!is.data.frame(Data)) {
		stop("`datCollate$data$Data` must be a data.frame.")
	}
	req_cols <- c("Sample", "data", "mean", "source")
	missing <- setdiff(req_cols, names(Data))
	if (length(missing)) {
		stop("`Data` is missing required columns: ", paste(missing, collapse = ", "))
	}
	
	# Per-sample summaries on feature rows (avoid log2 of non-positives)
	Data_sum <- Data %>%
		dplyr::filter(.data$data == feature_label) %>%
		dplyr::filter(is.finite(.data$mean), .data$mean > 0) %>%
		dplyr::mutate(log2NetI = log2(.data$mean)) %>%
		dplyr::group_by(.data$Sample) %>%
		dplyr::summarise(
			mean_log2NetI   = mean(.data$log2NetI, na.rm = TRUE),
			median_log2NetI = stats::median(.data$log2NetI, na.rm = TRUE),
			sd_log2NetI     = stats::sd(.data$log2NetI, na.rm = TRUE),
			mad_log2NetI    = stats::mad(.data$log2NetI, na.rm = TRUE),
			.groups = "drop"
		) %>%
		dplyr::mutate(
			nMAD2NetI     = .data$median_log2NetI - (2 * .data$mad_log2NetI),
			RFU_nMAD2NetI = 2 ^ .data$nMAD2NetI
		)
	
	# Join thresholds back and flag low intensity
	Data_edit <- Data %>%
		dplyr::left_join(Data_sum, by = "Sample") %>%
		dplyr::mutate(
			source = ifelse(.data$mean < .data$RFU_nMAD2NetI, "low NetI", .data$source)
		)
	
	# Optional diagnostics
	if (isTRUE(test)) {
		if (is.null(sample_select)) {
			sample_select <- unique(Data_edit$Sample)[1]
		}
		
		p_density <- ggplot2::ggplot(
			Data_edit %>% dplyr::filter(.data$Sample %in% sample_select)
		) +
			ggplot2::geom_density(ggplot2::aes(x = log2(.data$mean))) +
			ggplot2::geom_vline(ggplot2::aes(xintercept = .data$nMAD2NetI))
		
		p_jitter_all <- ggplot2::ggplot(Data_edit) +
			ggplot2::geom_jitter(ggplot2::aes(x = log2(.data$mean), y = .data$cv, col = .data$source))
		
		p_jitter_feat <- ggplot2::ggplot(
			Data_edit %>% dplyr::filter(.data$source != "GoodCV", .data$data == feature_label)
		) +
			ggplot2::geom_jitter(ggplot2::aes(x = log2(.data$mean), y = .data$cv, col = .data$source))
		
		counts_all <- Data_edit %>%
			dplyr::mutate(source = trimws(tolower(.data$source))) %>%
			dplyr::filter(!is.na(.data$source), .data$source != "") %>%
			dplyr::count(.data$source, name = "n", sort = TRUE) %>%
			dplyr::mutate(pct = .data$n / sum(.data$n))
		
		p_counts_all <- ggplot2::ggplot(
			counts_all, ggplot2::aes(x = forcats::fct_reorder(.data$source, .data$n), y = .data$n)
		) +
			ggplot2::geom_col() +
			ggplot2::geom_text(ggplot2::aes(label = paste0(round(.data$pct * 100, 1), "%")), hjust = -0.1) +
			ggplot2::expand_limits(y = max(counts_all$n) * 1.1) +
			ggplot2::labs(x = "Source", y = "Count", title = "Counts of `source` in Data_edit")
		
		counts_by_sample <- Data_edit %>%
			dplyr::group_by(.data$Sample) %>%
			dplyr::mutate(source = trimws(tolower(.data$source))) %>%
			dplyr::filter(!is.na(.data$source), .data$source != "") %>%
			dplyr::count(.data$source, name = "n", sort = TRUE) %>%
			dplyr::mutate(pct = .data$n / sum(.data$n)) %>%
			dplyr::ungroup()
		
		p_counts_one <- ggplot2::ggplot(
			counts_by_sample %>% dplyr::filter(.data$Sample == sample_select),
			ggplot2::aes(x = forcats::fct_reorder(.data$source, .data$n), y = .data$n)
		) +
			ggplot2::geom_col() +
			ggplot2::geom_text(ggplot2::aes(label = paste0(round(.data$pct * 100, 1), "%")), hjust = -0.1) +
			ggplot2::expand_limits(y = max(counts_by_sample$n) * 1.1) +
			ggplot2::labs(x = "Source", y = "Count",
										title = paste("Counts of `source` in Data_edit for", sample_select))
		
		attr(datCollate, "lowIntensity_plots") <- list(
			density = p_density,
			jitter_all = p_jitter_all,
			jitter_feature = p_jitter_feat,
			counts_all = p_counts_all,
			counts_sample = p_counts_one
		)
	}
	
	# Write results back and return
	datCollate$data$Data <- Data_edit
	datCollate$data$lowIntensityThresholds <- Data_sum
	datCollate
}
