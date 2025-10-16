#' Save Denoised Data Results
#'
#' Saves denoised matrices, plots, and summary tables to specified directory
#'
#' @param denoised_results List; output from denoise_remove_PCs()
#' @param output_dir Character; directory path for saving results
#' @param descriptor Character; file descriptor to add to filenames
#' @param eset ExpressionSet; original ExpressionSet for metadata
#' @param annotation_cols Character vector; columns for heatmap annotations
#' @param create_plots Logical; whether to create and save plots (default = TRUE)
#'
#' @return Invisible NULL (files saved to disk)
#' @export
#'
#' @examples
#' save_denoise_results(
#'   denoised_results, 
#'   output_dir = "AAb_called/denoised",
#'   descriptor = "NetI_IgG",
#'   eset = eset,
#'   annotation_cols = c("Sample_Group", "Batch_ID")
#' )
#'
save_denoise_results <- function(denoised_results, output_dir, descriptor,
																 eset = NULL, annotation_cols = NULL,
																 create_plots = TRUE) {
	
	# Create output directory
	if (!dir.exists(output_dir)) {
		dir.create(output_dir, recursive = TRUE)
	}
	
	# Save variance explained
	variance_df <- data.frame(
		PC = paste0("PC", seq_along(denoised_results$variance_explained)),
		Variance_Percent = denoised_results$variance_explained
	)
	
	write.csv(
		variance_df,
		file.path(output_dir, paste0(descriptor, "_variance_explained.csv")),
		row.names = FALSE
	)
	
	# Save each denoised dataset
	for (i in seq_along(denoised_results$denoised_data)) {
		# Save CSV
		write.csv(
			denoised_results$denoised_data[[i]],
			file.path(output_dir, paste0(descriptor, "_", i, "PCs_removed.csv"))
		)
		
		# Create plots if requested
		if (create_plots && !is.null(eset)) {
			# Heatmap
			pdf(
				file.path(output_dir, paste0("heatmap_", descriptor, "_", i, "PCs_removed.pdf")),
				width = 30, height = 20
			)
			print(plot_denoise_heatmap(
				denoised_results$denoised_data[[i]],
				eset,
				annotation_cols = annotation_cols,
				title = paste0(descriptor, " - ", i, " PC(s) Removed"),
				show_rownames = FALSE
			))
			dev.off()
			
			# Detailed heatmap
			pdf(
				file.path(output_dir, paste0("heatmap_", descriptor, "_", i, "PCs_removed_detail.pdf")),
				width = 30, height = 200
			)
			print(plot_denoise_heatmap(
				denoised_results$denoised_data[[i]],
				eset,
				annotation_cols = annotation_cols,
				title = paste0(descriptor, " - ", i, " PC(s) Removed - Detail"),
				show_rownames = TRUE
			))
			dev.off()
		}
	}
	
	# Save summary log
	log_file <- file.path(output_dir, paste0(descriptor, "_denoise_log.txt"))
	cat("Denoising Summary\n", file = log_file)
	cat("=================\n\n", file = log_file, append = TRUE)
	cat(paste("Date:", Sys.Date(), "\n"), file = log_file, append = TRUE)
	cat(paste("Descriptor:", descriptor, "\n"), file = log_file, append = TRUE)
	cat(paste("Number of PCs removed:", length(denoised_results$denoised_data), "\n\n"), 
			file = log_file, append = TRUE)
	cat("Variance Explained by First 10 PCs:\n", file = log_file, append = TRUE)
	cat(paste(head(denoised_results$variance_explained, 10), collapse = ", "), 
			file = log_file, append = TRUE)
	cat("\n", file = log_file, append = TRUE)
	
	message(paste("Results saved to:", output_dir))
	invisible(NULL)
}


#' Save Cutpoint Analysis Results
#'
#' Saves cutpoint analysis tables, AAb-called matrices, and diagnostic plots
#'
#' @param cutpoint_results Data frame; output from denoise_find_cutpoints()
#' @param optimal_cutpoint Data frame; output from denoise_select_optimal_cutpoint()
#' @param aab_called_data Data frame; AAb-called data at optimal cutpoint
#' @param output_dir Character; directory path for saving results
#' @param descriptor Character; file descriptor
#' @param eset ExpressionSet; original ExpressionSet (optional, for plots)
#' @param annotation_cols Character vector; columns for annotations
#' @param create_plots Logical; whether to create plots (default = TRUE)
#'
#' @return Invisible NULL (files saved to disk)
#' @export
#'
#' @examples
#' save_cutpoint_results(
#'   cutpoint_results,
#'   optimal_cutpoint,
#'   aab_called_data,
#'   output_dir = "AAb_called",
#'   descriptor = "NetI_IgG"
#' )
#'
save_cutpoint_results <- function(cutpoint_results, optimal_cutpoint, 
																	aab_called_data, output_dir, descriptor,
																	eset = NULL, annotation_cols = NULL,
																	create_plots = TRUE) {
	
	# Create output directory
	if (!dir.exists(output_dir)) {
		dir.create(output_dir, recursive = TRUE)
	}
	
	# Save cutpoint analysis table
	write.csv(
		cutpoint_results,
		file.path(output_dir, paste0(descriptor, "_cutpoint_analysis.csv")),
		row.names = FALSE
	)
	
	# Save optimal cutpoint selection
	write.csv(
		optimal_cutpoint,
		file.path(output_dir, paste0(descriptor, "_optimal_cutpoint.csv")),
		row.names = FALSE
	)
	
	# Save AAb-called matrix
	write.csv(
		aab_called_data,
		file.path(output_dir, paste0(descriptor, "_AAb_called_optimal.csv"))
	)
	
	# Calculate AAb call rates
	AAb_call_rates <- apply(aab_called_data, 1, function(x) {
		length(which(x > 0)) / length(x) * 100
	})
	AAb_call_rates_df <- data.frame(
		Antigen = names(AAb_call_rates),
		Percent_Positive = round(AAb_call_rates, 1)
	)
	AAb_call_rates_df <- AAb_call_rates_df[order(-AAb_call_rates_df$Percent_Positive), ]
	
	write.csv(
		AAb_call_rates_df,
		file.path(output_dir, paste0(descriptor, "_AAb_call_rates.csv")),
		row.names = FALSE
	)
	
	# Create plots if requested
	if (create_plots) {
		# Cutpoint summary plot
		pdf(
			file.path(output_dir, paste0(descriptor, "_cutpoint_summary.pdf")),
			width = 10, height = 12
		)
		print(plot_cutpoint_summary(cutpoint_results, optimal_cutpoint$cutpoint))
		dev.off()
		
		# AAb call rates histogram
		pdf(
			file.path(output_dir, paste0(descriptor, "_AAb_call_rates_histogram.pdf")),
			width = 8, height = 6
		)
		hist(AAb_call_rates, 
				 breaks = 20,
				 main = "Distribution of AAb Call Rates",
				 xlab = "Percent Positive (%)",
				 ylab = "Number of Antigens",
				 col = "skyblue",
				 border = "white")
		dev.off()
		
		# If eset provided, create heatmap and t-SNE
		if (!is.null(eset)) {
			# Heatmap
			pdf(
				file.path(output_dir, paste0(descriptor, "_AAb_called_heatmap.pdf")),
				width = 30, height = 150
			)
			print(plot_denoise_heatmap(
				aab_called_data,
				eset,
				annotation_cols = annotation_cols,
				title = paste0(descriptor, " - AAb Called Data"),
				show_rownames = TRUE
			))
			dev.off()
			
			# t-SNE if enough samples
			if (ncol(aab_called_data) >= 10) {
				tryCatch({
					pdf(
						file.path(output_dir, paste0(descriptor, "_AAb_called_tsne.pdf")),
						width = 8, height = 6
					)
					print(plot_denoise_tsne(
						aab_called_data,
						eset,
						color_by = annotation_cols[1]
					))
					dev.off()
				}, error = function(e) {
					warning("t-SNE plot failed: ", e$message)
				})
			}
		}
	}
	
	# Save summary log
	log_file <- file.path(output_dir, paste0(descriptor, "_cutpoint_log.txt"))
	cat("Cutpoint Analysis Summary\n", file = log_file)
	cat("=========================\n\n", file = log_file, append = TRUE)
	cat(paste("Date:", Sys.Date(), "\n"), file = log_file, append = TRUE)
	cat(paste("Descriptor:", descriptor, "\n\n"), file = log_file, append = TRUE)
	cat("Optimal Cutpoint:\n", file = log_file, append = TRUE)
	cat(paste("  Cutpoint:", optimal_cutpoint$cutpoint, "\n"), file = log_file, append = TRUE)
	cat(paste("  PN AAb Count:", optimal_cutpoint$PN_Aab_count_67_perc, "\n"), 
			file = log_file, append = TRUE)
	cat(paste("  TP:FP Ratio:", optimal_cutpoint$TP_FP_ratio, "\n"), 
			file = log_file, append = TRUE)
	cat(paste("  ZZ_con2 Rate:", optimal_cutpoint$zz_2_frac, "\n"), 
			file = log_file, append = TRUE)
	cat(paste("  ZZ_con4 Rate:", optimal_cutpoint$zz_4_frac, "\n\n"), 
			file = log_file, append = TRUE)
	cat("AAb-Called Data Summary:\n", file = log_file, append = TRUE)
	cat(paste("  Total Antigens:", nrow(aab_called_data), "\n"), 
			file = log_file, append = TRUE)
	cat(paste("  Total Samples:", ncol(aab_called_data), "\n"), 
			file = log_file, append = TRUE)
	cat(paste("  Median AAbs/Sample:", optimal_cutpoint$sample_AAb_median, "\n"), 
			file = log_file, append = TRUE)
	
	message(paste("Cutpoint results saved to:", output_dir))
	invisible(NULL)
}


#' Save Complete Denoising Pipeline Results
#'
#' Comprehensive save function for entire denoising pipeline
#'
#' @param denoised_results List; output from denoise_remove_PCs()
#' @param cutpoint_results Data frame; output from denoise_find_cutpoints()
#' @param optimal_cutpoint Data frame; optimal cutpoint selection
#' @param aab_called_data Data frame; final AAb-called matrix
#' @param output_dir Character; base output directory
#' @param descriptor Character; file descriptor
#' @param eset ExpressionSet; original ExpressionSet
#' @param annotation_cols Character vector; annotation columns
#' @param background_data Data frame; background data for border plots (optional)
#'
#' @return Invisible NULL (files saved to disk)
#' @export
#'
#' @examples
#' save_complete_denoise_results(
#'   denoised_results,
#'   cutpoint_results,
#'   optimal_cutpoint,
#'   aab_called_data,
#'   output_dir = "AAb_called",
#'   descriptor = "NetI_IgG",
#'   eset = eset
#' )
#'
save_complete_denoise_results <- function(denoised_results, cutpoint_results,
																					optimal_cutpoint, aab_called_data,
																					output_dir, descriptor, eset,
																					annotation_cols = NULL,
																					background_data = NULL) {
	
	# Create subdirectories
	denoise_dir <- file.path(output_dir, "denoised_matrices")
	cutpoint_dir <- file.path(output_dir, "cutpoint_analysis")
	plots_dir <- file.path(output_dir, "plots")
	
	dir.create(denoise_dir, recursive = TRUE, showWarnings = FALSE)
	dir.create(cutpoint_dir, recursive = TRUE, showWarnings = FALSE)
	dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
	
	# Save denoised results
	save_denoise_results(
		denoised_results,
		output_dir = denoise_dir,
		descriptor = descriptor,
		eset = eset,
		annotation_cols = annotation_cols,
		create_plots = TRUE
	)
	
	# Save cutpoint results
	save_cutpoint_results(
		cutpoint_results,
		optimal_cutpoint,
		aab_called_data,
		output_dir = cutpoint_dir,
		descriptor = descriptor,
		eset = eset,
		annotation_cols = annotation_cols,
		create_plots = TRUE
	)
	
	# Create border plots if background data provided
	if (!is.null(background_data)) {
		pdf(
			file.path(plots_dir, paste0(descriptor, "_AAb_borders_NetI.pdf")),
			width = 30, height = 100
		)
		print(plot_denoise_borders(
			background_data,
			aab_called_data,
			eset,
			annotation_cols = annotation_cols,
			variable = annotation_cols[1],
			title = paste0(descriptor, " - AAb Borders on NetI")
		))
		dev.off()
	}
	
	# Create master summary file
	summary_file <- file.path(output_dir, paste0(descriptor, "_MASTER_SUMMARY.txt"))
	cat("=" %R% 70, "\n", file = summary_file)
	cat("DENOISING PIPELINE MASTER SUMMARY\n", file = summary_file, append = TRUE)
	cat("=" %R% 70, "\n\n", file = summary_file, append = TRUE)
	cat(paste("Date:", Sys.Date(), "\n"), file = summary_file, append = TRUE)
	cat(paste("User:", Sys.getenv("USER"), "\n"), file = summary_file, append = TRUE)
	cat(paste("Descriptor:", descriptor, "\n\n"), file = summary_file, append = TRUE)
	
	cat("DENOISING PARAMETERS:\n", file = summary_file, append = TRUE)
	cat(paste("  PCs Removed:", length(denoised_results$denoised_data), "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  Variance Explained (PC1):", 
						denoised_results$variance_explained[1], "%\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  Variance Explained (PC2):", 
						denoised_results$variance_explained[2], "%\n\n"), 
			file = summary_file, append = TRUE)
	
	cat("OPTIMAL CUTPOINT:\n", file = summary_file, append = TRUE)
	cat(paste("  Cutpoint Value:", optimal_cutpoint$cutpoint, "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  PN AAb Count (≥67%):", optimal_cutpoint$PN_Aab_count_67_perc, "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  PN AAb Hit Rate:", optimal_cutpoint$PN_AAb_hit_rate, "%\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  TP:FP Ratio:", optimal_cutpoint$TP_FP_ratio, "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  ZZ_con2 Positivity:", optimal_cutpoint$zz_2_frac, "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  ZZ_con4 Positivity:", optimal_cutpoint$zz_4_frac, "\n\n"), 
			file = summary_file, append = TRUE)
	
	cat("FINAL AAb-CALLED DATA:\n", file = summary_file, append = TRUE)
	cat(paste("  Total Antigens:", nrow(aab_called_data), "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  Total Samples:", ncol(aab_called_data), "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  Unique AAbs Detected:", optimal_cutpoint$N_unique_AAbs, "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  Median AAbs per Sample:", optimal_cutpoint$sample_AAb_median, "\n\n"), 
			file = summary_file, append = TRUE)
	
	cat("OUTPUT DIRECTORIES:\n", file = summary_file, append = TRUE)
	cat(paste("  Denoised Matrices:", denoise_dir, "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  Cutpoint Analysis:", cutpoint_dir, "\n"), 
			file = summary_file, append = TRUE)
	cat(paste("  Plots:", plots_dir, "\n"), 
			file = summary_file, append = TRUE)
	
	cat("\n" %R% "=" %R% 70, "\n", file = summary_file, append = TRUE)
	
	message(paste("Complete pipeline results saved to:", output_dir))
	invisible(NULL)
}