#' ==============================================================================
#' LIMMA ANALYSIS USAGE EXAMPLES
#' ==============================================================================
#'
#' This file demonstrates how to use the limma analysis functions from the
#' iOmeAiFunctions package, both in script form and within Shiny apps.
#'
#' @note
#' Version 1.0.0
#' File: examples/limma_usage_examples.R

library(iOmeAiFunctions)
library(Biobase)
library(limma)

# ==============================================================================
# EXAMPLE 1: Basic limma analysis without contrasts
# ==============================================================================

# Load your ExpressionSet
# eSet <- readRDS("path/to/expressionset.rds")

# Or extract from ExpSet_list
ExpSet_list <- readRDS("path/to/ExpSet_list.rds")
extracted <- ExpSet_list_extract_function(
	ExpSet_list = ExpSet_list,
	data = "clinical",
	normalisation = "loess_normalisation",
	ComBat = FALSE,
	ncf_select = "retain"
)

eSet <- extracted$ExpSet
feature_select <- rownames(extracted$features)

# Run basic limma analysis
results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = feature_select,
	FC_cut = 1.1,
	p_val = 0.05
)

# Access results
print(results$design)
head(results$topTable)
print(results$sig_features)
print(results$contrast_info)

# ==============================================================================
# EXAMPLE 2: Limma with simple custom contrast (2 groups)
# ==============================================================================

# When you have more than 2 groups, specify which comparison
metadata <- pData(eSet)
group_levels <- levels(as.factor(metadata$Sample_Group))

# Create contrast: case vs control
my_contrast <- makeContrasts(
	"case - control",
	levels = group_levels
)

results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = feature_select,
	user_contrast = my_contrast,
	FC_cut = 1.1,
	p_val = 0.05
)

# ==============================================================================
# EXAMPLE 3: Limma with complex contrast (combining groups)
# ==============================================================================

# Compare combined groups: (mild + severe)/2 vs control
# Note: Replace any '-' in group names with '_' first
metadata$Sample_Group <- gsub("-", "_", metadata$Sample_Group)

# Update the ExpressionSet
pData(eSet)$Sample_Group <- metadata$Sample_Group

group_levels <- levels(as.factor(metadata$Sample_Group))

my_contrast <- makeContrasts(
	"(mild + severe)/2 - control",
	levels = group_levels
)

results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = feature_select,
	user_contrast = my_contrast,
	FC_cut = 1.1,
	p_val = 0.05
)

# ==============================================================================
# EXAMPLE 4: Limma with covariates
# ==============================================================================

# Include covariates when confounding is detected
results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = feature_select,
	covariate1 = "age",
	covariate2 = "gender",
	FC_cut = 1.1,
	p_val = 0.05
)

# ==============================================================================
# EXAMPLE 5: Limma with contrast AND covariates
# ==============================================================================

# For contrasts with covariates, include covariate levels in contrast
metadata <- pData(eSet)
group_levels <- levels(as.factor(metadata$Sample_Group))
gender_levels <- levels(as.factor(metadata$gender))[-1]  # Remove first level (reference)

my_contrast <- makeContrasts(
	"case - control",
	levels = c(group_levels, gender_levels)
)

results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = feature_select,
	covariate1 = "gender",
	user_contrast = my_contrast,
	FC_cut = 1.1,
	p_val = 0.05
)

# ==============================================================================
# EXAMPLE 6: Generate visualizations from limma results
# ==============================================================================

# Prepare data for plotting
plot_data <- prepare_limma_plot_data(
	limma_results = results,
	add_anno = c("age", "gender"),
	row_center = FALSE
)

# Create heatmap
library(pheatmap)
pheatmap(
	plot_data$plot_data,
	annotation_col = plot_data$plot_metadata,
	annotation_colors = plot_data$annotation_colors,
	cluster_cols = FALSE,
	gaps_col = plot_data$gap_col,
	show_rownames = TRUE,
	show_colnames = FALSE,
	main = "Significant Features"
)

# Create volcano plot
library(ggplot2)
library(ggrepel)

TT <- results$topTable
TT$threshold <- TT$P.Value < 0.05 & abs(TT$logFC) > log2(1.1)
TT$symbol <- rownames(TT)

ggplot(TT) +
	geom_point(aes(x = logFC, y = -log10(P.Value), colour = threshold)) +
	scale_color_brewer(palette = "Dark2") +
	geom_text_repel(
		data = subset(TT, threshold),
		aes(x = logFC, y = -log10(P.Value), label = symbol),
		max.overlaps = 20
	) +
	geom_vline(xintercept = c(-log2(1.1), log2(1.1)), 
						 linetype = "dashed", alpha = 0.5) +
	geom_hline(yintercept = -log10(0.05), 
						 linetype = "dashed", alpha = 0.5) +
	theme_bw() +
	labs(title = "Volcano Plot", 
			 x = "log2 Fold Change", 
			 y = "-log10 P-value")

# Create violin plots
library(reshape2)

sig_features <- results$sig_features[1:min(9, length(results$sig_features))]
df <- melt(as.matrix(results$expression[sig_features, rownames(results$metadata)]))
colnames(df)[1:2] <- c("feature", "Sample")

meta_df <- results$metadata
meta_df$Sample <- rownames(meta_df)
merge_df <- merge(meta_df, df, by = "Sample")

ggplot(merge_df, aes(x = .data[[results$variable]], 
										 y = value, 
										 color = .data[[results$variable]])) +
	geom_violin(alpha = 0.5) +
	geom_point(position = position_jitter(seed = 1, width = 0.2)) +
	facet_wrap(~ feature, ncol = 3, scales = "free") +
	theme_minimal() +
	theme(legend.position = "none")

# ==============================================================================
# EXAMPLE 7: Export results with additional metrics
# ==============================================================================

# Add fold change column
results_export <- results$topTable
results_export$FC <- logratio2foldchange(results_export$logFC)

# Filter to significant features
sig_results <- results_export[results$sig_features, ]

# Rearrange columns
sig_results <- sig_results[, c("logFC", "FC", "AveExpr", "t", 
															 "P.Value", "adj.P.Val", "B")]

# Round numeric columns
sig_results$logFC <- round(sig_results$logFC, 3)
sig_results$FC <- round(sig_results$FC, 2)
sig_results$AveExpr <- round(sig_results$AveExpr, 3)
sig_results$t <- round(sig_results$t, 3)

# Sort by absolute fold change
sig_results <- sig_results[order(-abs(sig_results$FC)), ]

# Export
write.csv(sig_results, "limma_significant_results.csv", row.names = TRUE)

# ==============================================================================
# EXAMPLE 8: Using limma modules in a custom Shiny app
# ==============================================================================

# See the full example app in: examples/limma_app_example.R
# Here's a minimal example:

library(shiny)

ui <- fluidPage(
	titlePanel("Minimal Limma App"),
	
	sidebarLayout(
		sidebarPanel(
			fileInput("eset_file", "Upload ExpressionSet:"),
			limmaContrastUI("contrast")
		),
		mainPanel(
			tabsetPanel(
				tabPanel("Results", limmaResultsUI("results")),
				tabPanel("Volcano", limmaVolcanoUI("volcano")),
				tabPanel("Heatmap", limmaHeatmapUI("heatmap"))
			)
		)
	)
)

server <- function(input, output, session) {
	eSet <- reactive({
		req(input$eset_file)
		readRDS(input$eset_file$datapath)
	})
	
	feature_select <- reactive({
		req(eSet())
		rownames(exprs(eSet()))
	})
	
	limma_results <- limmaContrastServer("contrast", eSet, feature_select)
	
	limmaResultsServer("results", limma_results)
	limmaVolcanoServer("volcano", limma_results)
	limmaHeatmapServer("heatmap", limma_results)
}

# shinyApp(ui, server)

# ==============================================================================
# EXAMPLE 9: Batch processing multiple comparisons
# ==============================================================================

# Define multiple comparisons
comparisons <- list(
	list(
		name = "case_vs_control",
		contrast = makeContrasts("case - control", 
														 levels = c("case", "control", "other"))
	),
	list(
		name = "case_vs_other",
		contrast = makeContrasts("case - other", 
														 levels = c("case", "control", "other"))
	),
	list(
		name = "control_vs_other",
		contrast = makeContrasts("control - other", 
														 levels = c("case", "control", "other"))
	)
)

# Run all comparisons
all_results <- lapply(comparisons, function(comp) {
	result <- limma_analysis(
		eSet = eSet,
		variable = "Sample_Group",
		feature_select = feature_select,
		user_contrast = comp$contrast,
		FC_cut = 1.1,
		p_val = 0.05
	)
	
	# Add comparison name
	result$comparison_name <- comp$name
	
	return(result)
})

# Extract significant features from each comparison
all_sig_features <- lapply(all_results, function(x) {
	data.frame(
		comparison = x$comparison_name,
		feature = x$sig_features,
		stringsAsFactors = FALSE
	)
})

all_sig_features_df <- do.call(rbind, all_sig_features)

# Find features significant in multiple comparisons
library(dplyr)
feature_counts <- all_sig_features_df %>%
	group_by(feature) %>%
	summarise(n_comparisons = n(), 
						comparisons = paste(comparison, collapse = ", "))

# Features significant in all comparisons
universal_features <- feature_counts %>%
	filter(n_comparisons == length(comparisons)) %>%
	pull(feature)

print(paste("Features significant in all comparisons:", 
						length(universal_features)))

# ==============================================================================
# EXAMPLE 10: Integration with existing pipeline
# ==============================================================================

# If you have an existing pipeline using the original limma_func,
# you can migrate to the new modular approach:

# OLD APPROACH:
# limma_func(
#   metadata = meta_clinical, 
#   input = loess, 
#   variable = "Sample_Group", 
#   feature_select = keep,
#   file_descriptor = "case_v_control"
# )

# NEW APPROACH:
# First create ExpressionSet if you don't have one
meta_clinical <- pData(eSet)
loess <- exprs(eSet)
keep <- feature_select

results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = keep
)

# Generate plots manually or use Shiny modules
# All data is accessible in the results list:
# - results$topTable: full results table
# - results$sig_features: significant features
# - results$sig_data: expression data for sig features
# - results$sig_data_centered: row-centered expression data
# - results$design: design matrix
# - results$fit: limma fit object
# - results$metadata: processed metadata
# - results$expression: full expression matrix

# ==============================================================================
# EXAMPLE 11: Advanced filtering and feature selection
# ==============================================================================

# Select features based on variance
expr_data <- exprs(eSet)
feature_variance <- apply(expr_data, 1, var, na.rm = TRUE)
top_var_features <- names(sort(feature_variance, decreasing = TRUE)[1:500])

# Select features based on expression level
feature_mean <- apply(expr_data, 1, mean, na.rm = TRUE)
high_expr_features <- names(feature_mean[feature_mean > median(feature_mean)])

# Combine criteria
feature_select <- intersect(top_var_features, high_expr_features)

# Run analysis
results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = feature_select,
	FC_cut = 1.2,
	p_val = 0.05
)

# ==============================================================================
# EXAMPLE 12: Handling missing data and quality control
# ==============================================================================

# Check for missing values in metadata
metadata <- pData(eSet)
missing_summary <- sapply(metadata, function(x) sum(is.na(x)))
print(missing_summary)

# The limma_analysis function automatically removes samples with NA 
# in the specified variable or covariates and warns you

# Check expression data completeness
expr_completeness <- apply(exprs(eSet), 1, function(x) sum(!is.na(x)) / length(x))
complete_features <- names(expr_completeness[expr_completeness > 0.8])  # 80% threshold

# Run analysis with quality-filtered features
results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = complete_features,
	FC_cut = 1.1,
	p_val = 0.05
)

# ==============================================================================
# EXAMPLE 13: Saving and loading results
# ==============================================================================

# Save complete results object
saveRDS(results, "limma_results_case_vs_control.rds")

# Load results later
results <- readRDS("limma_results_case_vs_control.rds")

# You can regenerate all plots without rerunning limma:
plot_data <- prepare_limma_plot_data(results, row_center = FALSE)

# Save individual components
write.csv(results$topTable, "top_table.csv", row.names = TRUE)
write.csv(results$sig_data, "significant_features_expression.csv", row.names = TRUE)
saveRDS(results$fit, "limma_fit_object.rds")

# ==============================================================================
# NOTES AND TIPS
# ==============================================================================

# 1. Always check group names for special characters
#    Replace '-' with '_' in factor levels before creating contrasts:
#    metadata$Group <- gsub("-", "_", metadata$Group)

# 2. For continuous covariates (e.g., age), they're automatically handled
#    No need to create dummy variables

# 3. The user_contrast parameter expects a contrast matrix from makeContrasts()
#    The levels in makeContrasts should match the levels in your metadata

# 4. Significant features are determined by BOTH p-value AND fold change
#    Adjust FC_cut and p_val parameters as needed for your study

# 5. The prepare_limma_plot_data() function handles sample ordering and
#    clustering within groups automatically

# 6. All Shiny modules are designed to work independently
#    You can mix and match modules in your custom apps

# 7. Row-centered data (mean-centered) is useful for heatmaps to show
#    relative differences across samples

# 8. For very large datasets, consider pre-filtering features by variance
#    or expression level to improve performance

# 9. The modules handle reactive dependencies automatically
#    No need to worry about invalidation or update order

# 10. Export plots as PDF using the download buttons in Shiny modules
#     Or use ggsave() and pdf() functions in scripts