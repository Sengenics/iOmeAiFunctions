# ========================================
# EXAMPLE 1: Pipeline Usage (Maintains exact compatibility)
# ========================================

# Your original pipeline code:
labels_vars <- unique(meta_clinical$Labels)
var1 <- labels_vars[2]
var2 <- labels_vars[1]

# Simply replace pFC.func with pFC_analysis:
pFC_analysis(
	input = clinical_norm[keep, rownames(meta_clinical)],
	metadata = meta_clinical,
	var = "Labels",
	groupPos = var1,
	groupNeg = var2,
	fold_change = 2,
	descriptor = paste0(var1, '_', var2, '_pFC'),
	p_val = 0.2,
	plot_width = 15,
	plot_height = 10,
	add_anno = NULL,
	PSA_flag = TRUE,
	PSA_colname = "PSA_class"
)

# ========================================
# EXAMPLE 2: Shiny App Usage
# ========================================

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
	dashboardHeader(title = "pFC Analysis"),
	dashboardSidebar(
		sidebarMenu(
			menuItem("pFC Analysis", tabName = "pfc", icon = icon("chart-bar"))
		)
	),
	dashboardBody(
		tabItems(
			tabItem(
				tabName = "pfc",
				pFC_UI("pfc_module")
			)
		)
	)
)

server <- function(input, output, session) {
	
	# Your ExpressionSet reactive (defined elsewhere in your app)
	eset_data <- reactive({
		# This would be your actual ExpressionSet selection logic
		# For example, loading from a file or selecting from available datasets
		your_expression_set
	})
	
	# Call pFC module
	pfc_results <- pFC_Server("pfc_module", eset_reactive = eset_data)
	
	# You can access results elsewhere in your app:
	# observe({
	#   req(pfc_results())
	#   results <- pfc_results()$results
	#   plots <- pfc_results()$plots
	# })
}

shinyApp(ui, server)

# ========================================
# EXAMPLE 3: Programmatic Usage (Advanced)
# ========================================

# Build ExpressionSet
eset <- Biobase::ExpressionSet(
	assayData = your_expression_matrix,  # log2-transformed
	phenoData = Biobase::AnnotatedDataFrame(your_metadata)
)

# Run processing only
results <- pFC_process(
	eset = eset,
	var = "disease_status",
	groupPos = "disease",
	groupNeg = "healthy",
	fold_change = 2,
	p_val = 0.05
)

# Access specific results
sig_hits <- results$pfc_significant
all_stats <- results$pfc_stats

# Generate plots separately
plots <- pFC_plot(results, plot_width = 12, plot_height = 8)

# Save if desired
pFC_save(results, plots, descriptor = "my_analysis")

# Or just work with the data in R
print(head(sig_hits))
print(plots$violin_plots[[1]])