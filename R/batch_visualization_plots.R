#' Plot Dendrogram with Technical Replicates Highlighted
#' @importFrom dendextend color_labels color_branches
plot_dendrogram <- function(hclust_obj, meta, color_by, replicate_col = NULL, title = "") {
	library(dendextend)
	
	dend <- as.dendrogram(hclust_obj)
	
	# Color labels by factor
	factor_vals <- meta[[color_by]]
	label_colors <- rainbow(length(unique(factor_vals)))[as.numeric(as.factor(factor_vals))]
	names(label_colors) <- rownames(meta)
	
	dend <- color_labels(dend, col = label_colors)
	
	# Highlight technical replicates
	if (!is.null(replicate_col) && replicate_col != "") {
		replicate_ids <- meta[[replicate_col]]
		
		# Find replicates (duplicated IDs)
		dup_ids <- unique(replicate_ids[duplicated(replicate_ids)])
		
		if (length(dup_ids) > 0) {
			# Draw dendrogram
			par(mar = c(10, 4, 4, 2))
			plot(dend, main = title)
			
			# Add rectangles around replicate groups
			replicate_colors <- rainbow(length(dup_ids), alpha = 0.3)
			
			for (i in seq_along(dup_ids)) {
				rep_id <- dup_ids[i]
				rep_samples <- rownames(meta)[meta[[replicate_col]] == rep_id]
				
				# Find positions in dendrogram
				positions <- which(labels(dend) %in% rep_samples)
				
				if (length(positions) > 1) {
					rect(
						xleft = min(positions) - 0.5,
						xright = max(positions) + 0.5,
						ybottom = par("usr")[3],
						ytop = par("usr")[4],
						col = replicate_colors[i],
						border = NA
					)
				}
			}
			
			# Redraw dendrogram on top
			par(new = TRUE)
			plot(dend, main = title)
			
			legend("topright",
						 legend = paste("Replicate", dup_ids),
						 fill = replicate_colors,
						 border = NA,
						 bty = "n")
		} else {
			plot(dend, main = paste(title, "\n(No technical replicates found)"))
		}
	} else {
		plot(dend, main = title)
	}
	
	legend("topleft",
				 legend = paste(color_by, "groups"),
				 bty = "n")
}

#' Plot t-SNE
plot_tsne <- function(tsne_obj, meta, color_by, shape_by = NULL, title = "") {
	df <- data.frame(
		tSNE1 = tsne_obj$Y[, 1],
		tSNE2 = tsne_obj$Y[, 2],
		color_var = meta[[color_by]],
		row.names = rownames(meta)
	)
	
	if (!is.null(shape_by) && shape_by != "") {
		df$shape_var <- meta[[shape_by]]
		
		p <- ggplot(df, aes(x = tSNE1, y = tSNE2, color = color_var, shape = shape_var)) +
			geom_point(size = 4, alpha = 0.7) +
			scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 9, 10, 11, 12)[1:length(unique(df$shape_var))]) +
			labs(color = color_by, shape = shape_by)
	} else {
		p <- ggplot(df, aes(x = tSNE1, y = tSNE2, color = color_var)) +
			geom_point(size = 4, alpha = 0.7) +
			labs(color = color_by)
	}
	
	p + 
		labs(title = title, x = "t-SNE 1", y = "t-SNE 2") +
		theme_minimal(base_size = 14) +
		theme(
			legend.position = "right",
			plot.title = element_text(face = "bold", hjust = 0.5)
		)
}

#' Plot t-SNE Comparison (side-by-side)
plot_tsne_comparison <- function(tsne_orig, tsne_corr, meta, color_by, shape_by = NULL) {
	df_orig <- data.frame(
		tSNE1 = tsne_orig$Y[, 1],
		tSNE2 = tsne_orig$Y[, 2],
		color_var = meta[[color_by]],
		Dataset = "Original"
	)
	
	df_corr <- data.frame(
		tSNE1 = tsne_corr$Y[, 1],
		tSNE2 = tsne_corr$Y[, 2],
		color_var = meta[[color_by]],
		Dataset = "Corrected"
	)
	
	df <- rbind(df_orig, df_corr)
	
	if (!is.null(shape_by) && shape_by != "") {
		df$shape_var <- rep(meta[[shape_by]], 2)
		
		p <- ggplot(df, aes(x = tSNE1, y = tSNE2, color = color_var, shape = shape_var)) +
			geom_point(size = 3, alpha = 0.7) +
			scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 9, 10, 11, 12)[1:length(unique(df$shape_var))]) +
			labs(color = color_by, shape = shape_by)
	} else {
		p <- ggplot(df, aes(x = tSNE1, y = tSNE2, color = color_var)) +
			geom_point(size = 3, alpha = 0.7) +
			labs(color = color_by)
	}
	
	p +
		facet_wrap(~ Dataset) +
		labs(title = "t-SNE: Original vs ComBat Corrected", x = "t-SNE 1", y = "t-SNE 2") +
		theme_minimal(base_size = 14) +
		theme(
			legend.position = "bottom",
			plot.title = element_text(face = "bold", hjust = 0.5),
			strip.text = element_text(face = "bold", size = 12)
		)
}

#' Plot PCA
plot_pca <- function(pca_obj, meta, color_by, shape_by = NULL, title = "") {
	var_explained <- pca_obj$sdev^2 / sum(pca_obj$sdev^2) * 100
	
	df <- data.frame(
		PC1 = pca_obj$x[, 1],
		PC2 = pca_obj$x[, 2],
		color_var = meta[[color_by]]
	)
	
	if (!is.null(shape_by) && shape_by != "") {
		df$shape_var <- meta[[shape_by]]
		
		p <- ggplot(df, aes(x = PC1, y = PC2, color = color_var, shape = shape_var)) +
			geom_point(size = 4, alpha = 0.7) +
			scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 9, 10, 11, 12)[1:length(unique(df$shape_var))]) +
			labs(color = color_by, shape = shape_by)
	} else {
		p <- ggplot(df, aes(x = PC1, y = PC2, color = color_var)) +
			geom_point(size = 4, alpha = 0.7) +
			labs(color = color_by)
	}
	
	p +
		labs(
			title = title,
			x = sprintf("PC1 (%.1f%%)", var_explained[1]),
			y = sprintf("PC2 (%.1f%%)", var_explained[2])
		) +
		theme_minimal(base_size = 14) +
		theme(
			legend.position = "right",
			plot.title = element_text(face = "bold", hjust = 0.5)
		)
}

#' Plot PCA Variance (single dataset)
plot_pca_variance_single <- function(variance) {
	df <- data.frame(
		PC = paste0("PC", 1:min(20, length(variance))),
		Variance = variance[1:min(20, length(variance))] * 100
	)
	df$PC <- factor(df$PC, levels = df$PC)
	
	ggplot(df, aes(x = PC, y = Variance)) +
		geom_col(fill = "steelblue") +
		labs(
			title = "PCA Variance Explained",
			x = "Principal Component",
			y = "Variance Explained (%)"
		) +
		theme_minimal(base_size = 14) +
		theme(
			axis.text.x = element_text(angle = 45, hjust = 1),
			plot.title = element_text(face = "bold", hjust = 0.5)
		)
}

#' Plot PCA Variance Comparison
plot_pca_variance_comparison <- function(var_orig, var_corr) {
	n_pcs <- min(20, length(var_orig), length(var_corr))
	
	df <- data.frame(
		PC = rep(paste0("PC", 1:n_pcs), 2),
		Variance = c(var_orig[1:n_pcs] * 100, var_corr[1:n_pcs] * 100),
		Dataset = rep(c("Original", "Corrected"), each = n_pcs)
	)
	df$PC <- factor(df$PC, levels = paste0("PC", 1:n_pcs))
	
	ggplot(df, aes(x = PC, y = Variance, fill = Dataset)) +
		geom_col(position = "dodge") +
		scale_fill_manual(values = c("Original" = "coral", "Corrected" = "steelblue")) +
		labs(
			title = "PCA Variance Explained: Original vs Corrected",
			x = "Principal Component",
			y = "Variance Explained (%)"
		) +
		theme_minimal(base_size = 14) +
		theme(
			axis.text.x = element_text(angle = 45, hjust = 1),
			plot.title = element_text(face = "bold", hjust = 0.5),
			legend.position = "top"
		)
}

#' Plot Distance Heatmap
#' @importFrom pheatmap pheatmap
plot_distance_heatmap <- function(dist_mat, meta, color_by, title = "") {
	library(pheatmap)
	
	# Convert distance to matrix
	dist_matrix <- as.matrix(dist_mat)
	
	# Annotation
	annotation_df <- data.frame(
		Group = meta[[color_by]],
		row.names = rownames(meta)
	)
	
	pheatmap(
		dist_matrix,
		annotation_row = annotation_df,
		annotation_col = annotation_df,
		show_rownames = FALSE,
		show_colnames = FALSE,
		main = title,
		clustering_distance_rows = dist_mat,
		clustering_distance_cols = dist_mat
	)
}