#' Run Limma Differential Expression Analysis
#'
#' @param metadata Data frame with sample metadata
#' @param input Expression matrix (features × samples)
#' @param variable Character; column name for variable of interest
#' @param covariates Character vector; covariate column names
#' @param class1 Character; first class (if categorical)
#' @param class2 Character; second class (if categorical)
#' @param continuous Logical; is variable continuous?
#' @param feature_select Character vector; features to analyze
#' @param EB_trend Logical; empirical Bayes trend
#' @param EB_robust Logical; empirical Bayes robust
#'
#' @return List containing:
#'   \item{fit}{Fitted limma model}
#'   \item{design}{Design matrix}
#'   \item{topTable}{Complete results table}
#'   \item{metadata_filtered}{Filtered metadata}
#'   \item{input_filtered}{Filtered input matrix}
#'   \item{params}{Analysis parameters}
#'
#' @export
run_limma_analysis <- function(metadata, input, variable, 
															 covariates = NULL,
															 class1 = NULL, class2 = NULL,
															 continuous = FALSE,
															 feature_select = NULL,
															 EB_trend = TRUE, EB_robust = TRUE) {
	
	# Validate inputs
	if (is.null(feature_select)) {
		stop("please specify filtered feature list as feature_select")
	}
	
	# Make syntactically valid names
	if (!is.numeric(metadata[, variable])) {
		metadata[, variable][!is.na(metadata[, variable])] <- 
			make.names(metadata[, variable][!is.na(metadata[, variable])])
		metadata[, variable] <- as.factor(metadata[, variable])
	}
	
	if (!is.null(covariates)) {
		for (covariate in covariates) {
			if (!is.numeric(metadata[, covariate])) {
				metadata[, covariate] <- make.names(metadata[, covariate])
				metadata[, covariate][!is.na(metadata[, covariate])] <- 
					make.names(metadata[, covariate][!is.na(metadata[, covariate])])
				metadata[, covariate] <- as.factor(metadata[, covariate])
			}
		}
	}
	
	# Remove NA rows
	covariate_cols <- c(variable, covariates)
	if (length(covariate_cols) > 1) {
		na_rows <- which(rowSums(is.na(metadata[, covariate_cols])) > 0)
	} else {
		na_rows <- which(is.na(metadata[, covariate_cols]))
	}
	
	if (length(na_rows) > 0) {
		warning(paste("Removed", length(na_rows), "rows with NA values"))
		metadata <- metadata[-na_rows, ]
		input <- input[, row.names(metadata)]
	}
	
	# Create ExpressionSet
	meta.var <- data.frame(
		labelDescription = colnames(metadata),
		row.names = colnames(metadata)
	)
	meta.PD <- new("AnnotatedDataFrame", 
								 data = metadata, 
								 varMetadata = meta.var)
	eSet <- new("ExpressionSet", 
							exprs = as.matrix(input),
							annotation = rownames(input),
							phenoData = meta.PD)
	
	# Build model
	if (!continuous) {
		design_formula <- as.formula(
			paste0("~ 0 + ", paste(c(variable, covariates), collapse = " + "))
		)
		design <- model.matrix(design_formula, data = metadata)
		contrast_string <- paste0(variable, class1, "-", variable, class2)
		user_contrast <- makeContrasts(contrasts = contrast_string, levels = design)
		contrast_used <- user_contrast
	} else {
		design_formula <- as.formula(
			paste0("~ ", paste(c(variable, covariates), collapse = " + "))
		)
		design <- model.matrix(design_formula, data = metadata)
		contrast_used <- NULL
	}
	
	# Fit model
	fit <- lmFit(eSet, design)
	
	if (!continuous) {
		fit2 <- contrasts.fit(fit, user_contrast)
	} else {
		fit2 <- contrasts.fit(fit, coeff = 2)
	}
	
	fit2 <- eBayes(fit2, trend = EB_trend, robust = EB_robust)
	
	# Get topTable
	TT <- topTable(fit2, number = Inf)
	TT <- TT[feature_select, ]
	TT <- TT %>% arrange(P.Value)
	
	# Return results
	list(
		fit = fit2,
		design = design,
		topTable = TT,
		metadata_filtered = metadata,
		input_filtered = input,
		params = list(
			variable = variable,
			covariates = covariates,
			class1 = class1,
			class2 = class2,
			continuous = continuous,
			contrast = contrast_used
		)
	)
}

#' Filter Significant Limma Results and Add ROC Metrics
#'
#' @param limma_results Output from run_limma_analysis()
#' @param p_val Numeric; p-value threshold
#' @param FC_cut Numeric; fold change threshold
#' @param add_ROC Logical; calculate ROC metrics?
#'
#' @return List containing:
#'   \item{sig_results}{Significant results with ROC}
#'   \item{data_sig}{Significant expression matrix}
#'   \item{data_sig_RC}{Row-centered significant data}
#'   \item{metadata_sig}{Metadata for significant samples}
#'
#' @export
filter_significant_results <- function(limma_results, 
																			 p_val = 0.05,
																			 FC_cut = 1.1,
																			 add_ROC = TRUE) {
	
	TT <- limma_results$topTable
	metadata <- limma_results$metadata_filtered
	input <- limma_results$input_filtered
	params <- limma_results$params
	
	# Filter by p-value and FC
	sig_idx <- which(TT$P.Value < p_val & abs(TT$logFC) > log2(FC_cut))
	
	if (length(sig_idx) == 0) {
		message("No significant results found")
		return(NULL)
	}
	
	sig_features <- rownames(TT)[sig_idx]
	
	# Filter metadata
	if (!params$continuous) {
		meta_sig <- metadata[metadata[, params$variable] %in% 
												 	c(params$class1, params$class2), ]
	} else {
		meta_sig <- metadata
	}
	
	# Filter data
	data_sig <- input[sig_features, rownames(meta_sig), drop = FALSE]
	data_RC <- data.frame(
		t(scale(t(input), scale = FALSE, center = TRUE)),
		check.names = FALSE
	)
	data_sig_RC <- data_RC[sig_features, rownames(meta_sig), drop = FALSE]
	
	# Add ROC metrics (if categorical)
	if (add_ROC && !params$continuous) {
		
		ROC_results_up <- ROC_mini(
			input = input[sig_features, rownames(meta_sig), drop = FALSE],
			metadata = meta_sig,
			variable = params$variable,
			groupPos = params$class1,
			groupNeg = params$class2,
			descriptor = "up",
			folder = NULL
		)
		ROC_results_up <- data.frame(ROC_results_up)
		
		ROC_results_down <- ROC_mini(
			input = input[sig_features, rownames(meta_sig), drop = FALSE],
			metadata = meta_sig,
			variable = params$variable,
			groupPos = params$class2,
			groupNeg = params$class1,
			descriptor = "down",
			folder = NULL
		)
		ROC_results_down <- data.frame(ROC_results_down)
		
		# Merge
		limma_sig <- TT[sig_features, ]
		limma_sig$Protein <- rownames(limma_sig)
		
		limma_up <- limma_sig[limma_sig$logFC > 0, ]
		limma_down <- limma_sig[limma_sig$logFC < 0, ]
		
		merged_up <- left_join(limma_up, ROC_results_up, by = "Protein")
		merged_down <- left_join(limma_down, ROC_results_down, by = "Protein")
		
		merged_results <- rbind(merged_up, merged_down)
		rownames(merged_results) <- merged_results$Protein
		
		# Format
		merged_results <- merged_results %>% 
			dplyr::select(-c(AveExpr, B, t, Protein))
		merged_results$P.Value <- format(merged_results$P.Value, 
																		 scientific = TRUE, digits = 3)
		merged_results$adj.P.Val <- format(merged_results$adj.P.Val, 
																			 scientific = TRUE, digits = 3)
		merged_results$FC <- logratio2foldchange(merged_results$logFC)
		merged_results[, c("logFC", "FC", "AUC", "Sensitivity", 
											 "Specificity", "Optimal.Cutoff")] <- 
			round(merged_results[, c("logFC", "FC", "AUC", "Sensitivity", 
															 "Specificity", "Optimal.Cutoff")], digits = 3)
		
		merged_results <- merged_results %>% arrange(P.Value)
		merged_results <- merged_results[, c(1, 8, 2:7)]
		merged_results <- merged_results %>% arrange(desc(abs(FC)))
		
		sig_results <- merged_results
		
	} else {
		sig_results <- TT[sig_features, ]
	}
	
	list(
		sig_results = sig_results,
		data_sig = data_sig,
		data_sig_RC = data_sig_RC,
		metadata_sig = meta_sig
	)
}

#' Generate All Limma Plots
#'
#' @param limma_results Output from run_limma_analysis()
#' @param sig_results Output from filter_significant_results()
#' @param p_val Numeric; p-value threshold
#' @param FC_cut Numeric; fold change threshold
#' @param plot_width Numeric; plot width
#' @param plot_height Numeric; plot height
#' @param add_anno Character vector; additional annotation columns
#' @param plot_violins Logical; generate violin plots?
#'
#' @return List of plot objects and heatmap data
#' @export
generate_limma_plots <- function(limma_results, 
																 sig_results,
																 p_val = 0.05,
																 FC_cut = 1.1,
																 plot_width = 20,
																 plot_height = 10,
																 add_anno = NULL,
																 plot_violins = TRUE) {
	
	plots <- list()
	
	if (is.null(sig_results)) {
		return(plots)
	}
	
	params <- limma_results$params
	TT <- limma_results$topTable
	data_sig <- sig_results$data_sig
	data_sig_RC <- sig_results$data_sig_RC
	meta_sig <- sig_results$metadata_sig
	
	# Prepare metadata for plotting
	if (is.null(add_anno)) {
		meta_plot <- meta_sig[, params$variable, drop = FALSE]
	} else {
		cols_anno <- unique(c(params$variable, add_anno))
		meta_plot <- data.frame(meta_sig[, cols_anno, drop = FALSE])
	}
	
	# Generate heatmaps (if enough features)
	if (nrow(data_sig) > 2) {
		plots$heatmaps <- generate_heatmap_data(
			data_sig = data_sig,
			data_sig_RC = data_sig_RC,
			meta_plot = meta_plot,
			params = params,
			add_anno = add_anno
		)
	}
	
	# Volcano plot
	plots$volcano <- generate_volcano_plot(
		topTable = TT,
		sig_features = rownames(data_sig),
		p_val = p_val,
		FC_cut = FC_cut
	)
	
	# Violin plots
	if (plot_violins && !params$continuous && nrow(data_sig) > 0) {
		plots$violin <- generate_violin_plots(
			data_sig = data_sig,
			metadata = meta_plot,
			variable = params$variable
		)
	}
	
	plots
}

#' Generate Heatmap Data
#' @keywords internal
generate_heatmap_data <- function(data_sig, data_sig_RC, meta_plot, 
																	params, add_anno) {
	
	heatmaps <- list()
	
	if (!params$continuous) {
		# Categorical: cluster by group
		get_groups <- c(params$class1, params$class2)
		meta_plot[, params$variable] <- factor(meta_plot[, params$variable], 
																					 levels = get_groups)
		
		data_sig <- data_sig[, rownames(meta_plot)]
		data_sig_RC <- data_sig_RC[, rownames(meta_plot)]
		
		# Cluster within groups
		for (j in 1:length(get_groups)) {
			ns <- data_sig[, which(meta_plot[, params$variable] == get_groups[j])]
			ns_clust <- hclust(vegdist(t(ns), distance = "correlation"))
			order <- ns_clust$labels[ns_clust$order]
			if (j == 1) {
				toplot <- ns[, order]
			} else {
				toplot <- cbind(toplot, ns[, order])
			}
		}
		meta_plot_sorted <- meta_plot[colnames(toplot), , drop = FALSE]
		
		# Same for RC
		for (j in 1:length(get_groups)) {
			ns <- data_sig_RC[, which(meta_plot[, params$variable] == get_groups[j])]
			ns_clust <- hclust(vegdist(t(ns), distance = "correlation"))
			order <- ns_clust$labels[ns_clust$order]
			if (j == 1) {
				toplot_RC <- ns[, order]
			} else {
				toplot_RC <- cbind(toplot_RC, ns[, order])
			}
		}
		meta_plot_RC_sorted <- meta_plot[colnames(toplot_RC), , drop = FALSE]
		
		# Calculate gap
		gap_tab <- table(meta_plot[, params$variable])
		gap_finder <- gap_tab[1]
		
	} else {
		# Continuous: sort by variable
		meta_plot_sorted <- meta_plot %>% arrange(.data[[params$variable]])
		toplot <- data_sig[, rownames(meta_plot_sorted)]
		toplot_RC <- data_sig_RC[, rownames(meta_plot_sorted)]
		meta_plot_RC_sorted <- meta_plot_sorted
		gap_finder <- NULL
	}
	
	# Get colors
	if (is.null(add_anno)) {
		anno_col <- color_distinct(meta_colors = meta_plot_sorted, variables = 1)
	} else {
		anno_col <- color_distinct(meta_colors = meta_plot_sorted, 
															 variables = 1:ncol(meta_plot_sorted))
	}
	
	# Store heatmap data
	heatmaps$manual_sort <- list(
		data = toplot,
		annotation_col = meta_plot_sorted,
		annotation_colors = anno_col,
		gaps_col = gap_finder,
		cluster_cols = FALSE
	)
	
	heatmaps$manual_sort_RC <- list(
		data = toplot_RC,
		annotation_col = meta_plot_RC_sorted,
		annotation_colors = anno_col,
		gaps_col = gap_finder,
		cluster_cols = FALSE
	)
	
	heatmaps$clustered_RC <- list(
		data = toplot_RC,
		annotation_col = meta_plot_RC_sorted,
		annotation_colors = anno_col,
		gaps_col = gap_finder,
		cluster_cols = TRUE
	)
	
	heatmaps
}

#' Generate Volcano Plot
#' @keywords internal
generate_volcano_plot <- function(topTable, sig_features, p_val, FC_cut) {
	
	sigs_ordered <- topTable[order(topTable$P.Value), ]
	sigs_ordered$genelabels <- ifelse(
		sigs_ordered$P.Value < p_val & (rownames(sigs_ordered) %in% sig_features),
		TRUE, FALSE
	)
	threshold_OE <- sigs_ordered$P.Value < p_val & 
		abs(sigs_ordered$logFC) > log2(FC_cut)
	sigs_ordered$threshold <- threshold_OE
	sigs_ordered$symbol <- rownames(sigs_ordered)
	
	ggplot(sigs_ordered) +
		geom_point(aes(x = logFC, y = -log10(P.Value), colour = threshold)) +
		scale_color_brewer(palette = "Dark2") +
		geom_text_repel(
			aes(x = logFC, y = -log10(P.Value),
					label = ifelse(genelabels == TRUE, symbol, "")),
			max.overlaps = 50
		) +
		ggtitle("Volcano plot hits") +
		xlab("log2 fold change") +
		ylab("-log10 p-value") +
		theme(
			legend.position = "none",
			plot.title = element_text(size = rel(1.5), hjust = 0.5),
			axis.title = element_text(size = rel(1.25))
		)
}

#' Generate Violin Plots
#' @keywords internal
generate_violin_plots <- function(data_sig, metadata, variable) {
	
	sig_f <- rownames(data_sig)
	df <- as.matrix(data_sig) %>% melt()
	
	colnames(df)[1:2] <- c("feature", "Sample")
	metadata$Sample <- rownames(metadata)
	
	merge.df <- merge(metadata, df, by = "Sample")
	
	if (length(sig_f) <= 6) {
		n_col <- 3
		n_row <- 2
	} else {
		n_col <- 3
		n_row <- 3
	}
	
	violin_cols <- c("#009E73", "#BEAED4", "#80B1D3", 
									 "goldenrod2", "coral2", "palevioletred2")
	color_select <- violin_cols[1:nlevels(as.factor(metadata[, variable]))]
	
	p <- ggplot(merge.df, aes(x = .data[[variable]], y = value, 
														color = .data[[variable]])) +
		geom_violin(alpha = 0.5) +
		scale_colour_manual(values = color_select) +
		scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
		theme_minimal() +
		geom_point(position = position_jitter(seed = 1, width = 0.2)) +
		theme(legend.position = "none") +
		facet_wrap_paginate(~ feature, ncol = n_col, nrow = n_row, scales = "free")
	
	list(
		plot = p,
		n_pages = n_pages(p)
	)
}

#' Save Limma Results to Files
#'
#' @param limma_results Output from run_limma_analysis()
#' @param sig_results Output from filter_significant_results()
#' @param plots Output from generate_limma_plots()
#' @param file_descriptor Character; output folder name
#' @param p_val Numeric; p-value threshold
#' @param plot_width Numeric; plot width
#' @param plot_height Numeric; plot height
#'
#' @export
save_limma_results <- function(limma_results, 
															 sig_results = NULL,
															 plots = NULL,
															 file_descriptor,
															 p_val = 0.05,
															 plot_width = 20,
															 plot_height = 10) {
	
	# Create directory
	dir.create(file_descriptor, showWarnings = FALSE)
	
	# Save topTable
	write.csv(
		limma_results$topTable,
		file = paste0(file_descriptor, "/limma_full_table_", file_descriptor, ".csv")
	)
	
	# Save significant results
	if (!is.null(sig_results) && !is.null(sig_results$sig_results)) {
		write.csv(
			sig_results$sig_results,
			paste0(file_descriptor, "/linear_modeling_ROC_results_table_", 
						 file_descriptor, ".csv")
		)
	}
	
	# Save plots
	if (!is.null(plots)) {
		
		# Heatmaps
		if (!is.null(plots$heatmaps)) {
			for (hm_name in names(plots$heatmaps)) {
				hm <- plots$heatmaps[[hm_name]]
				
				pdf(
					paste0(file_descriptor, "/heatmap_", p_val, "_", 
								 file_descriptor, "_", hm_name, ".pdf"),
					width = plot_width,
					height = plot_height
				)
				pheatmap(
					hm$data,
					gaps_col = hm$gaps_col,
					annotation_col = hm$annotation_col,
					annotation_colors = hm$annotation_colors,
					cluster_cols = hm$cluster_cols,
					show_rownames = TRUE,
					show_colnames = FALSE
				)
				dev.off()
			}
		}
		
		# Volcano plot
		if (!is.null(plots$volcano)) {
			pdf(
				paste0(file_descriptor, "/volcano_plot_", file_descriptor, ".pdf"),
				10, 7
			)
			print(plots$volcano)
			dev.off()
		}
		
		# Violin plots
		if (!is.null(plots$violin)) {
			pdf(
				paste0(file_descriptor, "/violin_limma_p_0_05_", 
							 file_descriptor, ".pdf")
			)
			for (i in 1:plots$violin$n_pages) {
				print(plots$violin$plot +
								facet_wrap_paginate(~ feature, ncol = 3, nrow = 3, 
																		page = i, scales = "free"))
			}
			dev.off()
		}
	}
	
	message("✓ All results saved to: ", file_descriptor)
}

#' Limma Analysis Pipeline (Complete Workflow)
#'
#' @inheritParams run_limma_analysis
#' @inheritParams filter_significant_results
#' @inheritParams generate_limma_plots
#' @param file_descriptor Character; output folder name (optional, for saving)
#' @param save_results Logical; save results to files?
#'
#' @return List containing all analysis results
#' @export
limma_func_dev <- function(metadata, input, variable, 
													 covariates = NULL,
													 class1 = NULL, class2 = NULL,
													 continuous = FALSE,
													 feature_select = NULL,
													 file_descriptor = NULL,
													 EB_trend = TRUE, EB_robust = TRUE,
													 FC_cut = 1.1, p_val = 0.05,
													 plot_height = 10, plot_width = 20,
													 add_anno = NULL, plot_violins = TRUE,
													 save_results = TRUE) {
	
	# Step 1: Run limma analysis
	message("Step 1: Running limma analysis...")
	limma_results <- run_limma_analysis(
		metadata = metadata,
		input = input,
		variable = variable,
		covariates = covariates,
		class1 = class1,
		class2 = class2,
		continuous = continuous,
		feature_select = feature_select,
		EB_trend = EB_trend,
		EB_robust = EB_robust
	)
	
	# Step 2: Filter significant results
	message("Step 2: Filtering significant results...")
	sig_results <- filter_significant_results(
		limma_results = limma_results,
		p_val = p_val,
		FC_cut = FC_cut,
		add_ROC = !continuous
	)
	
	# Step 3: Generate plots
	message("Step 3: Generating plots...")
	plots <- generate_limma_plots(
		limma_results = limma_results,
		sig_results = sig_results,
		p_val = p_val,
		FC_cut = FC_cut,
		plot_width = plot_width,
		plot_height = plot_height,
		add_anno = add_anno,
		plot_violins = plot_violins
	)
	
	# Step 4: Save results (if requested)
	if (save_results && !is.null(file_descriptor)) {
		message("Step 4: Saving results...")
		save_limma_results(
			limma_results = limma_results,
			sig_results = sig_results,
			plots = plots,
			file_descriptor = file_descriptor,
			p_val = p_val,
			plot_width = plot_width,
			plot_height = plot_height
		)
	}
	
	# Return combined results
	list(
		design = limma_results$design,
		topTable = limma_results$topTable,
		sig_results = if (!is.null(sig_results)) sig_results$sig_results else NULL,
		data_sig = if (!is.null(sig_results)) sig_results$data_sig else NULL,
		data_sig_RC = if (!is.null(sig_results)) sig_results$data_sig_RC else NULL,
		metadata_filtered = limma_results$metadata_filtered,
		plots = plots
	)
}
