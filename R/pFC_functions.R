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
#'
#' @name iOmeAIFunctions-pFC
#' @docType package
NULL



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
												var, 
												groupPos, 
												groupNeg,
												fold_change = 2,
												p_val = 0.2,
												PSA_flag = TRUE,
												PSA_colname = "PSA_class",
												cores = 1) {
	
	# Extract data from ExpressionSet
	input_data <- exprs(eset)  # log2-transformed expression data
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
	final_stats[, c(3:5, 7:10)] <- round(final_stats[, c(3:5, 7:10)], 2)
	
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
#' @param descriptor Character string; output directory and file descriptor
#' @param plot_width Numeric; width for saved plots (default: 15)
#' @param plot_height Numeric; height for saved plots (default: 10)
#'
#' @return Invisible NULL (files are saved to disk)
#'
#' @export
pFC_save <- function(pfc_results,
										 pfc_plots,
										 descriptor,
										 plot_width = 15,
										 plot_height = 10) {
	
	# Create output directory
	if (!dir.exists(descriptor)) {
		dir.create(descriptor, recursive = TRUE)
	}
	
	params <- pfc_results$parameters
	p_val <- params$p_val
	
	# ---- Save Tables ----
	
	# Normalized data (non-log2)
	write.csv(
		pfc_results$normalized_data,
		file.path(descriptor, paste0("pFC_Normalised_NetI_nonlog2_", descriptor, ".csv")),
		row.names = FALSE
	)
	
	# Fold change data
	write.csv(
		pfc_results$fc_data,
		file.path(descriptor, paste0("pFC_per_feature_per_sample_FC_vs_groupNeg_mean_", descriptor, ".csv")),
		row.names = FALSE
	)
	
	# All statistics
	write.csv(
		pfc_results$pfc_stats,
		file.path(descriptor, paste0("pFC_all_stats_", descriptor, ".csv")),
		row.names = FALSE
	)
	
	# Significant results
	write.csv(
		pfc_results$pfc_significant,
		file.path(descriptor, paste0("pFC_Fishers_exact_p_", p_val, "_", descriptor, ".csv")),
		row.names = FALSE
	)
	
	# ---- Save Plots ----
	
	# Violin plots
	if (!is.null(pfc_plots$violin_plots)) {
		pdf(
			file.path(descriptor, paste0("violin_pFC_", p_val, "_", descriptor, ".pdf")),
			width = plot_width,
			height = plot_height
		)
		for (p in pfc_plots$violin_plots) {
			print(p)
		}
		dev.off()
	}
	
	# Heatmaps
	if (!is.null(pfc_plots$heatmap_manual)) {
		# Manual sort
		pdf(
			file.path(descriptor, paste0("heatmap_", p_val, "_", descriptor, "_manualsort.pdf")),
			width = plot_width,
			height = plot_height
		)
		print(pfc_plots$heatmap_manual)
		dev.off()
		
		# Manual sort, row-centered
		pdf(
			file.path(descriptor, paste0("heatmap_", p_val, "_", descriptor, "_manualsort_RC.pdf")),
			width = plot_width,
			height = plot_height
		)
		print(pfc_plots$heatmap_manual_RC)
		dev.off()
		
		# Clustered, row-centered
		pdf(
			file.path(descriptor, paste0("heatmap_", p_val, "_", descriptor, "_RC.pdf")),
			width = plot_width,
			height = plot_height
		)
		print(pfc_plots$heatmap_clustered_RC)
		dev.off()
	}
	
	invisible(NULL)
}

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


