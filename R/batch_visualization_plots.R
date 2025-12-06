#' Plot Dendrogram with Technical Replicates Highlighted
#'
#' Colors only technical replicates (TR in QC column) by their batch factor
#' Labels show Sample IDs
#'
#' @param hclust_obj Hierarchical clustering object
#' @param meta Sample metadata data frame (must have rownames as Sample IDs)
#' @param color_by Column to color TR samples by (e.g., batch factor)
#' @param qc_column Column name containing QC info (default "QC")
#' @param title Plot title
#' @importFrom dendextend color_labels set labels<-
plot_dendrogram <- function(hclust_obj, 
														meta, 
														color_by,
														qc_column = "QC",
														title = "") {
	library(dendextend)
	
	dend <- as.dendrogram(hclust_obj)
	
	# Ensure Sample column exists (use rownames if not)
	if (!"Sample" %in% colnames(meta)) {
		meta$Sample <- rownames(meta)
	}
	
	# Set dendrogram labels to Sample IDs
	labels(dend) <- meta$Sample[order.dendrogram(dend)]
	
	# Check if QC column exists
	has_qc <- qc_column %in% colnames(meta)
	has_color <- color_by %in% colnames(meta)
	
	if (! has_qc) {
		warning(paste("QC column", qc_column, "not found in metadata"))
		plot(dend, main = paste(title, "\n(QC column not found)"))
		return(invisible(NULL))
	}
	
	if (!has_color) {
		warning(paste("Color column", color_by, "not found in metadata"))
		plot(dend, main = paste(title, "\n(Color column not found)"))
		return(invisible(NULL))
	}
	
	# Identify technical replicates
	is_tr <- grepl("TR", meta[[qc_column]], ignore.case = TRUE)
	
	if (sum(is_tr) == 0) {
		# No TR found - plot with standard coloring
		plot(dend, main = paste(title, "\n(No technical replicates found)"))
		return(invisible(NULL))
	}
	
	# Get unique values in the color_by column for TR samples
	tr_groups <- unique(meta[[color_by]][is_tr])
	n_tr_groups <- length(tr_groups)
	
	# Create color palette for TR groups
	tr_colors <- rainbow(n_tr_groups, s = 0.8, v = 0.9)
	names(tr_colors) <- tr_groups
	
	# Create color vector for all samples
	label_colors <- rep("gray30", nrow(meta))
	names(label_colors) <- meta$Sample
	
	# Color only TR samples by their batch factor
	for (i in which(is_tr)) {
		sample_id <- meta$Sample[i]
		group_value <- meta[[color_by]][i]
		label_colors[sample_id] <- tr_colors[as.character(group_value)]
	}
	
	# Apply colors to dendrogram labels
	dend <- color_labels(dend, col = label_colors[labels(dend)])
	
	# Plot
	par(mar = c(10, 4, 4, 2))
	plot(dend, main = title)
	
	# Calculate optimal label size based on number of samples
	n_samples <- nrow(meta)
	label_cex <- calculate_label_cex(n_samples)
	
	# Adjust bottom margin based on label size
	bottom_margin <- max(10, 10 + (1 - label_cex) * 5)
	
	# Plot
	par(mar = c(bottom_margin, 4, 4, 2))
	plot(dend, main = title, cex = label_cex)
	
	# Add legend (adjust size too)
	legend_cex <- min(0.9, label_cex + 0.1)
	
	
	# Add legend
	# legend("topright",
	# 			 legend = c(
	# 			 	paste0("Technical Replicates (", color_by, "):"),
	# 			 	as.character(tr_groups),
	# 			 	"",
	# 			 	"Other samples"
	# 			 ),
	# 			 fill = c(NA, tr_colors, NA, "gray30"),
	# 			 border = c(NA, rep("black", n_tr_groups), NA, "black"),
	# 			 bty = "n",
	# 			 cex = legend_cex)
	
	legend("topright",
				 legend = c(
				 	paste0("Technical Replicates (", color_by, "):"),
				 	as.character(tr_groups),
				 	"",
				 	"Other samples"
				 ),
				 fill = c(NA, tr_colors, NA, "gray30"),
				 col = c(NA, rep("black", n_tr_groups), NA, "black"),  # Use 'col' instead of 'border'
				 bty = "n",
				 cex = legend_cex)
	
	# Add informative note
	mtext(
		sprintf("Technical replicates (colored by %s) should cluster together", color_by),
		side = 1,
		line = bottom_margin - 1,
		cex = 0.8,
		col = "darkblue",
		font = 3
	)
	
	invisible(NULL)
}


#' Calculate optimal label cex for dendrogram
#'
#' @param n_samples Number of samples in the dendrogram
#' @return Numeric cex value between 0. 3 and 1.0
calculate_label_cex <- function(n_samples) {
	if (n_samples <= 20) {
		return(1.0)
	} else if (n_samples <= 40) {
		return(0.8)
	} else if (n_samples <= 60) {
		return(0.6)
	} else if (n_samples <= 100) {
		return(0.5)
	} else if (n_samples <= 150) {
		return(0.4)
	} else {
		# For very large datasets, use minimum readable size
		return(max(0.3, 60 / n_samples))
	}
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


#' Plot Correlation Matrix
#' @importFrom corrplot corrplot
plot_correlation_matrix <- function(cor_matrix, title = "") {
	library(corrplot)
	library(PerformanceAnalytics)
	
	# Use PerformanceAnalytics chart. Correlation style
	chart.Correlation(cor_matrix, histogram = FALSE, method = "pearson")
	title(main = title, line = 3)
}

#' Plot Correlation Histogram
plot_correlation_histogram <- function(correlation_results) {
	library(ggplot2)
	
	# Extract upper triangle correlations
	cor_orig <- correlation_results$original$cor_matrix
	orig_values <- cor_orig[upper.tri(cor_orig)]
	
	df <- data.frame(
		Correlation = orig_values,
		Dataset = "Original"
	)
	
	if (! is.null(correlation_results$corrected)) {
		cor_corr <- correlation_results$corrected$cor_matrix
		corr_values <- cor_corr[upper.tri(cor_corr)]
		
		df <- rbind(
			df,
			data.frame(
				Correlation = corr_values,
				Dataset = "Corrected"
			)
		)
	}
	
	ggplot(df, aes(x = Correlation, fill = Dataset)) +
		geom_histogram(bins = 30, alpha = 0.6, position = "identity", color = "white") +
		geom_vline(xintercept = 0.9, linetype = "dashed", color = "red", size = 1) +
		scale_fill_manual(values = c("Original" = "coral", "Corrected" = "steelblue")) +
		labs(
			title = "Distribution of Technical Replicate Correlations",
			subtitle = "Red line indicates 0.9 threshold",
			x = "Pearson Correlation",
			y = "Count"
		) +
		theme_minimal(base_size = 14) +
		theme(legend.position = "top")
}

#' Plot Correlation by Batch
plot_correlation_by_batch <- function(correlation_results, meta, batch_column, tr_samples) {
	library(ggplot2)
	
	# Calculate mean correlation for each TR sample
	cor_orig <- correlation_results$original$cor_matrix
	
	mean_cors <- data.frame(
		Sample = rownames(cor_orig),
		Mean_Correlation = colMeans(cor_orig, na.rm = TRUE),
		Dataset = "Original"
	)
	
	# Add batch info
	meta_tr <- meta[tr_samples, ]
	mean_cors <- mean_cors %>%
		left_join(
			meta_tr %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, !!sym(batch_column)),
			by = "Sample"
		)
	
	if (! is.null(correlation_results$corrected)) {
		cor_corr <- correlation_results$corrected$cor_matrix
		
		mean_cors_corr <- data.frame(
			Sample = rownames(cor_corr),
			Mean_Correlation = colMeans(cor_corr, na.rm = TRUE),
			Dataset = "Corrected"
		)
		
		mean_cors_corr <- mean_cors_corr %>%
			left_join(
				meta_tr %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, !!sym(batch_column)),
				by = "Sample"
			)
		
		mean_cors <- rbind(mean_cors, mean_cors_corr)
	}
	
	# Order batches by mean correlation
	batch_order <- mean_cors %>%
		filter(Dataset == "Original") %>%
		group_by(!!sym(batch_column)) %>%
		summarise(batch_mean = mean(Mean_Correlation, na.rm = TRUE)) %>%
		arrange(batch_mean) %>%
		pull(!! sym(batch_column))
	
	mean_cors[[batch_column]] <- factor(mean_cors[[batch_column]], levels = batch_order)
	
	ggplot(mean_cors, aes(x = !!sym(batch_column), y = Mean_Correlation, color = Dataset)) +
		geom_boxplot(alpha = 0.3) +
		geom_jitter(size = 3, width = 0.2, alpha = 0.7) +
		geom_hline(yintercept = 0.9, linetype = "dashed", color = "red") +
		scale_color_manual(values = c("Original" = "coral", "Corrected" = "steelblue")) +
		labs(
			title = "Mean Correlation by Batch",
			subtitle = "Each point represents a technical replicate sample",
			x = batch_column,
			y = "Mean Correlation"
		) +
		theme_minimal(base_size = 14) +
		theme(
			axis.text.x = element_text(angle = 45, hjust = 1),
			legend.position = "top"
		)
}

#' Plot Intra vs Inter Batch Correlation
plot_correlation_intra_inter <- function(correlation_results, meta, batch_column, tr_samples) {
	library(ggplot2)
	library(tidyr)
	
	meta_tr <- meta[tr_samples, ]
	
	# Create dataframe of all pairwise correlations
	cor_orig <- correlation_results$original$cor_matrix
	
	cor_df <- as.data.frame(cor_orig) %>%
		rownames_to_column("Sample") %>%
		pivot_longer(-Sample, names_to = "Sample_1", values_to = "Correlation") %>%
		filter(Sample != Sample_1)
	
	# Add batch information
	cor_df <- cor_df %>%
		left_join(
			meta_tr %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, Batch = !!sym(batch_column)),
			by = "Sample"
		) %>%
		left_join(
			meta_tr %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, Batch_1 = !!sym(batch_column)),
			by = c("Sample_1" = "Sample")
		) %>%
		mutate(
			Type = ifelse(Batch == Batch_1, "Intra-Batch", "Inter-Batch"),
			Dataset = "Original"
		)
	
	# Add corrected data if available
	if (!is.null(correlation_results$corrected)) {
		cor_corr <- correlation_results$corrected$cor_matrix
		
		cor_df_corr <- as.data.frame(cor_corr) %>%
			rownames_to_column("Sample") %>%
			pivot_longer(-Sample, names_to = "Sample_1", values_to = "Correlation") %>%
			filter(Sample != Sample_1) %>%
			left_join(
				meta_tr %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, Batch = !!sym(batch_column)),
				by = "Sample"
			) %>%
			left_join(
				meta_tr %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, Batch_1 = !!sym(batch_column)),
				by = c("Sample_1" = "Sample")
			) %>%
			mutate(
				Type = ifelse(Batch == Batch_1, "Intra-Batch", "Inter-Batch"),
				Dataset = "Corrected"
			)
		
		cor_df <- rbind(cor_df, cor_df_corr)
	}
	
	# Flag low correlations
	cor_df <- cor_df %>%
		mutate(QC = ifelse(Correlation < 0.9, "flag", "pass"))
	
	ggplot(cor_df, aes(x = Type, y = Correlation, color = QC)) +
		geom_boxplot(aes(color = NULL), alpha = 0.3) +
		geom_jitter(size = 2, width = 0.2, alpha = 0.6) +
		geom_hline(yintercept = 0.9, linetype = "dashed", color = "red") +
		scale_color_manual(values = c("pass" = "black", "flag" = "red")) +
		facet_wrap(~ Dataset) +
		labs(
			title = "Intra-Batch vs Inter-Batch Correlations",
			subtitle = "Intra-batch correlations should be higher",
			x = "",
			y = "Pearson Correlation"
		) +
		theme_minimal(base_size = 14) +
		theme(legend.position = "top")
}

#' Create Correlation Summary Table
create_correlation_summary_table <- function(correlation_results, meta, batch_column, tr_samples) {
	library(dplyr)
	library(tidyr)
	
	meta_tr <- meta[tr_samples, ]
	cor_orig <- correlation_results$original$cor_matrix
	
	# Calculate intra and inter batch stats
	cor_df <- as.data.frame(cor_orig) %>%
		rownames_to_column("Sample") %>%
		pivot_longer(-Sample, names_to = "Sample_1", values_to = "Correlation") %>%
		filter(Sample != Sample_1) %>%
		left_join(
			meta_tr %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, Batch = !!sym(batch_column)),
			by = "Sample"
		) %>%
		left_join(
			meta_tr %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, Batch_1 = !!sym(batch_column)),
			by = c("Sample_1" = "Sample")
		) %>%
		mutate(Type = ifelse(Batch == Batch_1, "Intra", "Inter"))
	
	summary_orig <- cor_df %>%
		group_by(Type) %>%
		summarise(
			Mean = round(mean(Correlation, na.rm = TRUE), 3),
			Median = round(median(Correlation, na.rm = TRUE), 3),
			Minimum = round(min(Correlation, na.rm = TRUE), 3),
			Maximum = round(max(Correlation, na.rm = TRUE), 3)
		) %>%
		mutate(Dataset = "Original")
	
	if (!is.null(correlation_results$corrected)) {
		cor_corr <- correlation_results$corrected$cor_matrix
		
		cor_df_corr <- as.data.frame(cor_corr) %>%
			rownames_to_column("Sample") %>%
			pivot_longer(-Sample, names_to = "Sample_1", values_to = "Correlation") %>%
			filter(Sample != Sample_1) %>%
			left_join(
				meta_tr %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, Batch = !! sym(batch_column)),
				by = "Sample"
			) %>%
			left_join(
				meta_tr %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, Batch_1 = !!sym(batch_column)),
				by = c("Sample_1" = "Sample")
			) %>%
			mutate(Type = ifelse(Batch == Batch_1, "Intra", "Inter"))
		
		summary_corr <- cor_df_corr %>%
			group_by(Type) %>%
			summarise(
				Mean = round(mean(Correlation, na.rm = TRUE), 3),
				Median = round(median(Correlation, na.rm = TRUE), 3),
				Minimum = round(min(Correlation, na.rm = TRUE), 3),
				Maximum = round(max(Correlation, na.rm = TRUE), 3)
			) %>%
			mutate(Dataset = "Corrected")
		
		summary_orig <- rbind(summary_orig, summary_corr)
	}
	
	DT::datatable(
		summary_orig,
		options = list(
			pageLength = 10,
			dom = 't'
		),
		rownames = FALSE
	) %>%
		DT::formatStyle(
			'Mean',
			backgroundColor = DT::styleInterval(0.9, c('lightcoral', 'lightgreen'))
		) %>%
		DT::formatStyle(
			'Minimum',
			backgroundColor = DT::styleInterval(0.9, c('lightcoral', 'lightgreen'))
		)
}


#' Plot Sample Correlation by Batch
plot_sample_correlation_by_batch <- function(correlation_results, meta, batch_column, samples) {
	library(ggplot2)
	library(dplyr)
	
	# Calculate mean correlation for each sample
	cor_orig <- correlation_results$original$cor_matrix
	
	mean_cors <- data.frame(
		Sample = rownames(cor_orig),
		Mean_Correlation = colMeans(cor_orig, na.rm = TRUE),
		Dataset = "Original"
	)
	
	# Add batch info
	meta_samples <- meta[samples, ]
	mean_cors <- mean_cors %>%
		left_join(
			meta_samples %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, !!sym(batch_column)),
			by = "Sample"
		)
	
	if (! is.null(correlation_results$corrected)) {
		cor_corr <- correlation_results$corrected$cor_matrix
		
		mean_cors_corr <- data.frame(
			Sample = rownames(cor_corr),
			Mean_Correlation = colMeans(cor_corr, na.rm = TRUE),
			Dataset = "Corrected"
		)
		
		mean_cors_corr <- mean_cors_corr %>%
			left_join(
				meta_samples %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, !!sym(batch_column)),
				by = "Sample"
			)
		
		mean_cors <- rbind(mean_cors, mean_cors_corr)
	}
	
	# Order batches by mean correlation in original data
	batch_order <- mean_cors %>%
		filter(Dataset == "Original") %>%
		group_by(!!sym(batch_column)) %>%
		summarise(batch_mean = mean(Mean_Correlation, na.rm = TRUE), .groups = "drop") %>%
		arrange(batch_mean) %>%
		pull(!! sym(batch_column))
	
	mean_cors[[batch_column]] <- factor(mean_cors[[batch_column]], levels = batch_order)
	
	# Calculate batch statistics for annotation
	batch_stats <- mean_cors %>%
		group_by(!!sym(batch_column), Dataset) %>%
		summarise(
			batch_mean = mean(Mean_Correlation, na.rm = TRUE),
			n = n(),
			.groups = "drop"
		)
	
	p <- ggplot(mean_cors, aes(x = !! sym(batch_column), y = Mean_Correlation, color = Dataset)) +
		geom_boxplot(alpha = 0.3, outlier.shape = NA) +
		geom_jitter(size = 2, width = 0.2, alpha = 0.6) +
		scale_color_manual(values = c("Original" = "coral", "Corrected" = "steelblue")) +
		labs(
			title = "Mean Sample Correlation by Batch",
			subtitle = "Each point represents a non-TR sample's mean correlation with all other samples",
			x = batch_column,
			y = "Mean Correlation"
		) +
		theme_minimal(base_size = 14) +
		theme(
			axis.text.x = element_text(angle = 45, hjust = 1),
			legend.position = "top"
		)
	
	# Add batch sample counts
	if (!is.null(correlation_results$corrected)) {
		p <- p + 
			geom_text(
				data = batch_stats %>% filter(Dataset == "Original"),
				aes(label = paste0("n=", n), y = min(mean_cors$Mean_Correlation) - 0.02),
				color = "black",
				size = 3,
				angle = 45,
				hjust = 1
			)
	}
	
	p
}

#' Plot Sample Correlation Intra vs Inter Batch
plot_sample_correlation_intra_inter <- function(correlation_results, meta, batch_column, samples) {
	library(ggplot2)
	library(dplyr)
	library(tidyr)
	
	meta_samples <- meta[samples, ]
	
	# Create dataframe of all pairwise correlations
	cor_orig <- correlation_results$original$cor_matrix
	
	cor_df <- as.data.frame(cor_orig) %>%
		rownames_to_column("Sample") %>%
		pivot_longer(-Sample, names_to = "Sample_1", values_to = "Correlation") %>%
		filter(Sample != Sample_1)
	
	# Add batch information
	cor_df <- cor_df %>%
		left_join(
			meta_samples %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, Batch = !!sym(batch_column)),
			by = "Sample"
		) %>%
		left_join(
			meta_samples %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, Batch_1 = !!sym(batch_column)),
			by = c("Sample_1" = "Sample")
		) %>%
		mutate(
			Type = ifelse(Batch == Batch_1, "Intra-Batch", "Inter-Batch"),
			Dataset = "Original"
		)
	
	# Add corrected data if available
	if (!is.null(correlation_results$corrected)) {
		cor_corr <- correlation_results$corrected$cor_matrix
		
		cor_df_corr <- as.data.frame(cor_corr) %>%
			rownames_to_column("Sample") %>%
			pivot_longer(-Sample, names_to = "Sample_1", values_to = "Correlation") %>%
			filter(Sample != Sample_1) %>%
			left_join(
				meta_samples %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, Batch = !!sym(batch_column)),
				by = "Sample"
			) %>%
			left_join(
				meta_samples %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, Batch_1 = !!sym(batch_column)),
				by = c("Sample_1" = "Sample")
			) %>%
			mutate(
				Type = ifelse(Batch == Batch_1, "Intra-Batch", "Inter-Batch"),
				Dataset = "Corrected"
			)
		
		cor_df <- rbind(cor_df, cor_df_corr)
	}
	
	# Calculate statistics for text annotation
	summary_stats <- cor_df %>%
		group_by(Dataset, Type) %>%
		summarise(
			median = median(Correlation, na.rm = TRUE),
			mean = mean(Correlation, na.rm = TRUE),
			.groups = "drop"
		)
	
	ggplot(cor_df, aes(x = Type, y = Correlation, fill = Dataset)) +
		geom_violin(alpha = 0.5, position = position_dodge(width = 0.9)) +
		geom_boxplot(width = 0.2, alpha = 0.7, position = position_dodge(width = 0.9), 
								 outlier.size = 0.5) +
		scale_fill_manual(values = c("Original" = "coral", "Corrected" = "steelblue")) +
		labs(
			title = "Intra-Batch vs Inter-Batch Sample Correlations",
			subtitle = "Batch effect correction should increase inter-batch correlations",
			x = "",
			y = "Pearson Correlation"
		) +
		theme_minimal(base_size = 14) +
		theme(legend.position = "top") +
		geom_text(
			data = summary_stats,
			aes(label = sprintf("μ=%.3f", mean), 
					y = 0.05,
					group = Dataset),
			position = position_dodge(width = 0.9),
			size = 3.5,
			fontface = "bold"
		)
}

#' Create Sample Correlation Intra vs Inter Batch Table
create_sample_correlation_intra_inter_table <- function(correlation_results, meta, batch_column, samples) {
	library(dplyr)
	library(tidyr)
	
	meta_samples <- meta[samples, ]
	cor_orig <- correlation_results$original$cor_matrix
	
	# Calculate intra and inter batch stats
	cor_df <- as.data.frame(cor_orig) %>%
		rownames_to_column("Sample") %>%
		pivot_longer(-Sample, names_to = "Sample_1", values_to = "Correlation") %>%
		filter(Sample != Sample_1) %>%
		left_join(
			meta_samples %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, Batch = !!sym(batch_column)),
			by = "Sample"
		) %>%
		left_join(
			meta_samples %>% 
				#rownames_to_column("Sample") %>% 
				select(Sample, Batch_1 = !!sym(batch_column)),
			by = c("Sample_1" = "Sample")
		) %>%
		mutate(Type = ifelse(Batch == Batch_1, "Intra", "Inter"))
	
	summary_orig <- cor_df %>%
		group_by(Type) %>%
		summarise(
			N_Comparisons = n(),
			Mean = round(mean(Correlation, na.rm = TRUE), 4),
			Median = round(median(Correlation, na.rm = TRUE), 4),
			Minimum = round(min(Correlation, na.rm = TRUE), 4),
			Maximum = round(max(Correlation, na.rm = TRUE), 4),
			Std_Dev = round(sd(Correlation, na.rm = TRUE), 4),
			.groups = "drop"
		) %>%
		mutate(Dataset = "Original")
	
	if (!is.null(correlation_results$corrected)) {
		cor_corr <- correlation_results$corrected$cor_matrix
		
		cor_df_corr <- as.data.frame(cor_corr) %>%
			rownames_to_column("Sample") %>%
			pivot_longer(-Sample, names_to = "Sample_1", values_to = "Correlation") %>%
			filter(Sample != Sample_1) %>%
			left_join(
				meta_samples %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, Batch = !!sym(batch_column)),
				by = "Sample"
			) %>%
			left_join(
				meta_samples %>% 
					#rownames_to_column("Sample") %>% 
					select(Sample, Batch_1 = !!sym(batch_column)),
				by = c("Sample_1" = "Sample")
			) %>%
			mutate(Type = ifelse(Batch == Batch_1, "Intra", "Inter"))
		
		summary_corr <- cor_df_corr %>%
			group_by(Type) %>%
			summarise(
				N_Comparisons = n(),
				Mean = round(mean(Correlation, na.rm = TRUE), 4),
				Median = round(median(Correlation, na.rm = TRUE), 4),
				Minimum = round(min(Correlation, na.rm = TRUE), 4),
				Maximum = round(max(Correlation, na.rm = TRUE), 4),
				Std_Dev = round(sd(Correlation, na.rm = TRUE), 4),
				.groups = "drop"
			) %>%
			mutate(Dataset = "Corrected")
		
		summary_orig <- rbind(summary_orig, summary_corr)
	}
	
	# Reorder columns
	summary_orig <- summary_orig %>%
		select(Dataset, Type, N_Comparisons, Mean, Median, Minimum, Maximum, Std_Dev)
	
	DT::datatable(
		summary_orig,
		options = list(
			pageLength = 10,
			dom = 't'
		),
		rownames = FALSE
	) %>%
		DT::formatStyle(
			'Mean',
			backgroundColor = DT::styleInterval(
				c(0.3, 0.5, 0.7), 
				c('lightcoral', 'lightyellow', 'lightgreen', 'darkseagreen')
			)
		) %>%
		DT::formatStyle(
			'Minimum',
			backgroundColor = DT::styleInterval(
				c(0, 0.3, 0.5), 
				c('darkred', 'lightcoral', 'lightyellow', 'lightgreen')
			)
		)
}