#' Plot Denoised Data Heatmap
#'
#' Creates a heatmap of denoised data with sample annotations
#'
#' @param denoised_data Data frame; denoised expression data
#' @param eset ExpressionSet; ExpressionSet with metadata for annotations
#' @param annotation_cols Character vector; column names from pData(eset) to use for annotation
#' @param title Character; plot title
#' @param cluster_cols Logical; whether to cluster columns (default = TRUE)
#' @param cluster_rows Logical; whether to cluster rows (default = TRUE)
#' @param show_rownames Logical; whether to show row names (default = FALSE)
#' @param show_colnames Logical; whether to show column names (default = FALSE)
#' @param ... Additional arguments passed to pheatmap
#'
#' @return pheatmap object
#' @import pheatmap
#' @import Biobase
#' @export
#'
#' @examples
#' plot_denoise_heatmap(
#'   denoised_data, 
#'   eset, 
#'   annotation_cols = c("Sample_Group", "Batch_ID")
#' )
#'
plot_denoise_heatmap <- function(denoised_data, eset, 
																 annotation_cols = NULL,
																 title = "Denoised Data",
																 cluster_cols = TRUE,
																 cluster_rows = TRUE,
																 show_rownames = FALSE,
																 show_colnames = FALSE,
																 ...) {
	
	# Extract metadata
	metadata <- Biobase::pData(eset)
	
	# Ensure metadata matches data
	metadata <- metadata[colnames(denoised_data), , drop = FALSE]
	
	# Prepare annotation
	if (!is.null(annotation_cols)) {
		annotation <- metadata[, annotation_cols, drop = FALSE]
	} else {
		annotation <- NULL
	}
	
	# Create color breaks
	max_val <- max(denoised_data, na.rm = TRUE)
	if (max_val <= 15) {
		breaks <- seq(0, max_val, by = 0.1)
	} else {
		breaks <- seq(0, max_val, by = 0.2)
	}
	
	# Create heatmap
	p <- pheatmap::pheatmap(
		denoised_data,
		annotation_col = annotation,
		cluster_cols = cluster_cols,
		cluster_rows = cluster_rows,
		show_rownames = show_rownames,
		show_colnames = show_colnames,
		breaks = breaks,
		main = title,
		...
	)
	
	return(p)
}


#' Plot AAb Borders on Heatmap
#'
#' Creates a heatmap with black borders around AAb-positive cells
#'
#' @param background_data Data frame; background data to plot (e.g., NetI or normalized)
#' @param aab_called_data Data frame; AAb-called data (0/positive values)
#' @param eset ExpressionSet; ExpressionSet with metadata
#' @param annotation_cols Character vector; metadata columns for annotation
#' @param variable Character; variable name for manual group sorting
#' @param title Character; plot title
#' @param ... Additional arguments for pheatmap
#'
#' @return pheatmap object with bordered cells
#' @import pheatmap
#' @import Biobase
#' @importFrom dplyr filter
#' @importFrom purrr map map_lgl
#' @export
#'
#' @examples
#' plot_denoise_borders(
#'   NetI_data, 
#'   aab_called_data, 
#'   eset, 
#'   annotation_cols = c("Sample_Group"),
#'   variable = "Sample_Group"
#' )
#'
plot_denoise_borders <- function(background_data, aab_called_data, eset,
																 annotation_cols = NULL,
																 variable = NULL,
																 title = "AAb Borders",
																 ...) {
	
	require(pheatmap)
	require(dplyr)
	require(purrr)
	
	# Extract metadata
	metadata <- Biobase::pData(eset)
	
	# Ensure matching samples
	common_samples <- intersect(colnames(background_data), colnames(aab_called_data))
	background_data <- background_data[, common_samples, drop = FALSE]
	aab_called_data <- aab_called_data[, common_samples, drop = FALSE]
	metadata <- metadata[common_samples, , drop = FALSE]
	
	# Manual sort by variable if specified
	if (!is.null(variable) && variable %in% colnames(metadata)) {
		metadata[[variable]] <- factor(metadata[[variable]])
		
		# Cluster within each group
		sorted_samples <- c()
		for (level in levels(metadata[[variable]])) {
			group_samples <- rownames(metadata)[metadata[[variable]] == level]
			group_data <- background_data[, group_samples, drop = FALSE]
			
			# Remove zero rows/cols for clustering
			if (ncol(group_data) >= 3 && nrow(group_data) >= 3) {
				non_zero_rows <- rowSums(group_data) > 0
				non_zero_cols <- colSums(group_data) > 0
				
				if (sum(non_zero_rows) >= 3 && sum(non_zero_cols) >= 3) {
					cluster_data <- group_data[non_zero_rows, non_zero_cols, drop = FALSE]
					hc <- hclust(dist(t(cluster_data)))
					sorted_samples <- c(sorted_samples, colnames(cluster_data)[hc$order])
					
					# Add back zero samples
					zero_samples <- setdiff(group_samples, colnames(cluster_data))
					sorted_samples <- c(sorted_samples, zero_samples)
				} else {
					sorted_samples <- c(sorted_samples, group_samples)
				}
			} else {
				sorted_samples <- c(sorted_samples, group_samples)
			}
		}
		
		background_data <- background_data[, sorted_samples, drop = FALSE]
		aab_called_data <- aab_called_data[, sorted_samples, drop = FALSE]
		metadata <- metadata[sorted_samples, , drop = FALSE]
	}
	
	# Prepare annotations
	if (!is.null(annotation_cols)) {
		annotation <- metadata[, annotation_cols, drop = FALSE]
	} else {
		annotation <- NULL
	}
	
	# Create initial heatmap
	ph <- pheatmap::pheatmap(
		background_data,
		annotation_col = annotation,
		cluster_cols = FALSE,
		cluster_rows = TRUE,
		show_rownames = TRUE,
		show_colnames = FALSE,
		main = title,
		silent = TRUE,
		...
	)
	
	# Extract the heatmap grob
	grob_classes <- purrr::map(ph$gtable$grobs, class)
	idx_grob <- which(purrr::map_lgl(grob_classes, function(cl) 'gTree' %in% cl))[1]
	grob_names <- names(ph$gtable$grobs[[idx_grob]]$children)
	idx_rect <- grob_names[grep('rect', grob_names)][1]
	
	border_mat <- ph$gtable$grobs[[idx_grob]]$children[[idx_rect]]$gp$fill
	
	# Match AAb-called data to heatmap order
	row_order <- rownames(background_data)[ph$tree_row$order]
	aab_called_data <- aab_called_data[row_order, colnames(background_data), drop = FALSE]
	
	# Set borders to black where AAb-called data > 0
	for (i in 1:ncol(border_mat)) {
		pos_idx <- which(aab_called_data[, i] > 0)
		border_mat[pos_idx, i] <- "#000000"
	}
	
	# Set non-AAb borders to white
	border_mat[border_mat != "#000000"] <- "#FFFFFF"
	ph$gtable$grobs[[idx_grob]]$children[[idx_rect]]$gp$col <- border_mat
	
	return(ph)
}


#' Plot Cutpoint Summary
#'
#' Creates summary plots for cutpoint selection including:
#' - TP:FP ratio vs cutpoint
#' - PN AAb count vs cutpoint
#' - ZZ control rates vs cutpoint
#'
#' @param cutpoint_results Data frame; output from denoise_find_cutpoints()
#' @param optimal_cutpoint Numeric; optimal cutpoint to highlight (optional)
#'
#' @return Combined plot (grid.arrange object)
#' @import ggplot2
#' @import gridExtra
#' @export
#'
#' @examples
#' plot_cutpoint_summary(cutpoint_results, optimal_cutpoint = 1.4)
#'
plot_cutpoint_summary <- function(cutpoint_results, optimal_cutpoint = NULL) {
	
	require(ggplot2)
	require(gridExtra)
	
	# TP:FP ratio plot
	p1 <- ggplot(cutpoint_results, aes(x = cutpoint, y = TP_FP_ratio)) +
		geom_line(color = "blue", size = 1) +
		geom_point(size = 2) +
		labs(title = "TP:FP Ratio vs Cutpoint", x = "Cutpoint", y = "TP:FP Ratio") +
		theme_minimal()
	
	if (!is.null(optimal_cutpoint)) {
		p1 <- p1 + geom_vline(xintercept = optimal_cutpoint, linetype = "dashed", color = "red")
	}
	
	# PN AAb count plot
	p2 <- ggplot(cutpoint_results, aes(x = cutpoint, y = PN_Aab_count_67_perc)) +
		geom_line(color = "darkgreen", size = 1) +
		geom_point(size = 2) +
		labs(title = "PN AAb Count vs Cutpoint", x = "Cutpoint", y = "PN AAb Count (≥67%)") +
		theme_minimal()
	
	if (!is.null(optimal_cutpoint)) {
		p2 <- p2 + geom_vline(xintercept = optimal_cutpoint, linetype = "dashed", color = "red")
	}
	
	# ZZ control rates plot
	p3 <- ggplot(cutpoint_results) +
		geom_line(aes(x = cutpoint, y = zz_2_frac, color = "ZZ_con2"), size = 1) +
		geom_line(aes(x = cutpoint, y = zz_4_frac, color = "ZZ_con4"), size = 1) +
		geom_point(aes(x = cutpoint, y = zz_2_frac), size = 2) +
		geom_point(aes(x = cutpoint, y = zz_4_frac), size = 2) +
		labs(title = "ZZ Control Positivity vs Cutpoint", x = "Cutpoint", y = "Fraction Positive") +
		scale_color_manual(values = c("ZZ_con2" = "purple", "ZZ_con4" = "orange")) +
		theme_minimal() +
		theme(legend.title = element_blank())
	
	if (!is.null(optimal_cutpoint)) {
		p3 <- p3 + geom_vline(xintercept = optimal_cutpoint, linetype = "dashed", color = "red")
	}
	
	# Combine plots
	combined_plot <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
	
	return(combined_plot)
}


#' Plot t-SNE of AAb-Called Data
#'
#' Creates a t-SNE visualization of AAb-called data
#'
#' @param aab_called_data Data frame; AAb-called expression data
#' @param eset ExpressionSet; ExpressionSet with metadata
#' @param color_by Character; metadata column for point colors
#' @param shape_by Character; metadata column for point shapes (optional)
#' @param perplexity Numeric; t-SNE perplexity parameter (default = 30)
#' @param seed Numeric; random seed for reproducibility
#'
#' @return ggplot object
#' @import ggplot2
#' @import Rtsne
#' @import Biobase
#' @export
#'
#' @examples
#' plot_denoise_tsne(
#'   aab_called_data, 
#'   eset, 
#'   color_by = "Sample_Group",
#'   shape_by = "PSA_class"
#' )
#'
plot_denoise_tsne <- function(aab_called_data, eset, 
															color_by, shape_by = NULL,
															perplexity = 30, seed = 42) {
	
	require(Rtsne)
	require(ggplot2)
	
	# Extract metadata
	metadata <- Biobase::pData(eset)
	
	# Remove zero-only samples
	non_zero_samples <- colSums(aab_called_data) > 0
	aab_called_data <- aab_called_data[, non_zero_samples, drop = FALSE]
	metadata <- metadata[colnames(aab_called_data), , drop = FALSE]
	
	# Run t-SNE
	set.seed(seed)
	tsne_result <- Rtsne(
		t(aab_called_data),
		dims = 2,
		perplexity = min(perplexity, floor((ncol(aab_called_data) - 1) / 3)),
		verbose = FALSE,
		max_iter = 1000
	)
	
	# Create plot data
	plot_data <- data.frame(
		tSNE1 = tsne_result$Y[, 1],
		tSNE2 = tsne_result$Y[, 2],
		metadata[, c(color_by, shape_by), drop = FALSE]
	)
	
	# Create plot
	p <- ggplot(plot_data, aes(x = tSNE1, y = tSNE2, color = .data[[color_by]])) +
		geom_point(size = 3, alpha = 0.7) +
		labs(title = "t-SNE of AAb-Called Data", x = "t-SNE 1", y = "t-SNE 2") +
		theme_minimal() +
		theme(legend.position = "right")
	
	if (!is.null(shape_by)) {
		p <- p + aes(shape = .data[[shape_by]])
	}
	
	return(p)
}