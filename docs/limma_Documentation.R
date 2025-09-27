# Limma Analysis Functions

Modular limma differential expression analysis functions for the iOmeAiFunctions package.

## Overview

This package provides a modular approach to limma differential expression analysis, with separate functions for analysis and visualization, plus Shiny modules for interactive exploration.

## Files

### Core Functions (`R/limma_function.R`)

- **`limma_analysis()`** - Main analysis function
- **`prepare_limma_plot_data()`** - Prepares data for visualization
- **`logratio2foldchange()`** - Converts log2 FC to linear FC
- **`generate_annotation_colors()`** - Internal function for color generation

### Shiny Modules (`R/limma_ShinyModule.R`)

- **`limmaContrastUI()` / `limmaContrastServer()`** - Contrast specification interface
- **`limmaVolcanoUI()` / `limmaVolcanoServer()`** - Interactive volcano plots
- **`limmaHeatmapUI()` / `limmaHeatmapServer()`** - Interactive heatmaps
- **`limmaViolinUI()` / `limmaViolinServer()`** - Interactive violin plots
- **`limmaResultsUI()` / `limmaResultsServer()`** - Results summary and tables

## Key Features

### Analysis Functions

✅ **Flexible input**: Works with ExpressionSet objects  
✅ **Custom contrasts**: Supports simple and complex contrasts  
✅ **Covariate adjustment**: Include up to 2 covariates  
✅ **Robust statistics**: Optional trend and robust eBayes  
✅ **Comprehensive output**: Returns all analysis components  
✅ **No file writing**: All data stays in memory for downstream use

### Shiny Modules

✅ **Interactive contrast builder**: Point-and-click contrast specification  
✅ **Live previews**: Sample distribution and design matrix display  
✅ **Customizable plots**: Adjust parameters without re-running analysis  
✅ **Modular design**: Use only the modules you need  
✅ **Download support**: Export plots and results

## Installation

```r
# Install from GitHub (if published)
# devtools::install_github("yourname/iOmeAiFunctions")

# Or install locally
devtools::install("path/to/iOmeAiFunctions")
```

## Dependencies

Required packages:
	- `Biobase`
- `limma`
- `dplyr`
- `tibble`
- `ggplot2`
- `ggrepel`
- `pheatmap`
- `reshape2`
- `vegan`
- `ggforce` (for violin plots)
- `shiny` (for Shiny modules)
- `DT` (for interactive tables)

## Quick Start

### Basic Analysis (Script)

```r
library(iOmeAiFunctions)
library(Biobase)

# Load your ExpressionSet
eSet <- readRDS("my_expressionset.rds")
features <- rownames(exprs(eSet))

# Run basic analysis
results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = features,
	FC_cut = 1.1,
	p_val = 0.05
)

# Access results
print(results$sig_features)
head(results$topTable)
```

### With Custom Contrast

```r
# Create contrast
my_contrast <- makeContrasts(
	"case - control",
	levels = levels(pData(eSet)$Sample_Group)
)

# Run analysis
results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = features,
	user_contrast = my_contrast
)
```

### Shiny App

```r
library(shiny)
library(iOmeAiFunctions)

ui <- fluidPage(
	titlePanel("Limma Analysis"),
	sidebarLayout(
		sidebarPanel(
			fileInput("eset", "Upload ExpressionSet:"),
			limmaContrastUI("limma")
		),
		mainPanel(
			tabsetPanel(
				tabPanel("Results", limmaResultsUI("results")),
				tabPanel("Volcano", limmaVolcanoUI("volcano")),
				tabPanel("Heatmap", limmaHeatmapUI("heatmap")),
				tabPanel("Violins", limmaViolinUI("violin"))
			)
		)
	)
)

server <- function(input, output, session) {
	eSet <- reactive({
		req(input$eset)
		readRDS(input$eset$datapath)
	})
	
	features <- reactive({
		req(eSet())
		rownames(exprs(eSet()))
	})
	
	# Run analysis
	results <- limmaContrastServer("limma", eSet, features)
	
	# Connect visualization modules
	limmaResultsServer("results", results)
	limmaVolcanoServer("volcano", results)
	limmaHeatmapServer("heatmap", results)
	limmaViolinServer("violin", results)
}

shinyApp(ui, server)
```

## Integration with ExpSet_list

Works seamlessly with the existing `ExpSet_list_extract_function()`:
	
	```r
# Extract from ExpSet_list
ExpSet_list <- readRDS("my_ExpSet_list.rds")

extracted <- ExpSet_list_extract_function(
	ExpSet_list = ExpSet_list,
	data = "clinical",
	normalisation = "loess_normalisation",
	ComBat = FALSE,
	ncf_select = "retain"
)

# Run limma
results <- limma_analysis(
	eSet = extracted$ExpSet,
	variable = "Sample_Group",
	feature_select = rownames(extracted$features)
)
```

## Function Reference

### limma_analysis()

Main analysis function that performs limma differential expression.

**Parameters:**
	- `eSet` - ExpressionSet object
- `variable` - Column name for comparison variable
- `feature_select` - Features to analyze (required)
- `covariate1` - Optional first covariate
- `covariate2` - Optional second covariate
- `user_contrast` - Optional contrast matrix
- `EB_trend` - Use eBayes trend (default TRUE)
- `EB_robust` - Use robust eBayes (default TRUE)
- `FC_cut` - Fold change threshold (default 1.1)
- `p_val` - P-value threshold (default 0.05)

**Returns:**
	List containing:
	- `design` - Design matrix
- `topTable` - Full limma results
- `sig_features` - Significant feature names
- `fit` - limma fit object
- `metadata` - Processed metadata
- `expression` - Expression matrix
- `variable` - Variable name used
- `contrast_info` - Contrast details
- `p_threshold` - P-value threshold
- `fc_threshold` - FC threshold
- `sig_data` - Expression data for significant features
- `sig_data_centered` - Row-centered sig_data

### prepare_limma_plot_data()

Prepares data for visualization with proper sample ordering.

**Parameters:**
	- `limma_results` - Output from limma_analysis()
- `add_anno` - Additional annotation columns
- `row_center` - Center rows (default FALSE)

**Returns:**
	List containing:
	- `plot_data` - Ordered expression matrix
- `plot_metadata` - Ordered metadata
- `annotation_colors` - Color scheme
- `gap_col` - Heatmap gap position

## Advanced Usage

### Complex Contrasts

```r
# Average of two groups vs. control
my_contrast <- makeContrasts(
	"(group_A + group_B)/2 - control",
	levels = c("group_A", "group_B", "control")
)
```

### With Covariates

```r
# Include age and gender as covariates
results <- limma_analysis(
	eSet = eSet,
	variable = "Disease_Status",
	feature_select = features,
	covariate1 = "age",
	covariate2 = "gender"
)
```

### Batch Processing

```r
# Multiple comparisons
comparisons <- list(
	case_vs_control = makeContrasts("case - control", levels = groups),
	case_vs_other = makeContrasts("case - other", levels = groups)
)

results_list <- lapply(comparisons, function(contrast) {
	limma_analysis(
		eSet = eSet,
		variable = "Sample_Group",
		feature_select = features,
		user_contrast = contrast
	)
})
```

## Important Notes

1. **Group names**: Replace special characters (especially '-') in factor levels before creating contrasts:
	```r
pData(eSet)$Group <- gsub("-", "_", pData(eSet)$Group)
```

2. **Missing data**: The function automatically removes samples with NA values in specified variables/covariates and issues a warning

3. **Feature selection**: Pre-filter features by variance or expression level for large datasets

4. **Significance**: Features are significant if they meet BOTH p-value AND fold change thresholds

5. **Contrast syntax**: When using `makeContrasts()`, levels must exactly match factor levels in metadata

## Migrating from Original limma_func

The original `limma_func()` wrote files to disk and combined analysis with visualization. The new modular approach:
	
	**Old approach:**
	```r
limma_func(
	metadata = meta_clinical,
	input = loess,
	variable = "Sample_Group",
	feature_select = keep,
	file_descriptor = "case_v_control"
)
```

**New approach:**
	```r
# Analysis
results <- limma_analysis(
	eSet = eSet,
	variable = "Sample_Group",
	feature_select = keep
)

# Visualization (separate)
plot_data <- prepare_limma_plot_data(results)
# Then create plots as needed
```

**Benefits:**
	- ✅ All data accessible in memory
- ✅ Replot without re-running analysis
- ✅ Modular and testable
- ✅ Works in Shiny apps
- ✅ No unwanted file creation

## Examples

See `examples/limma_usage_examples.R` for comprehensive examples including:
	- Basic usage
- Custom contrasts
- Covariate adjustment
- Batch processing
- Integration with existing pipelines
- Shiny app examples

See `examples/limma_app_example.R` for a complete Shiny app.

## Support

For issues or questions:
	1. Check the examples in `examples/limma_usage_examples.R`
2. Review function documentation: `?limma_analysis`
3. Contact the package maintainer

## Version History

### v1.0.0
- Initial modular implementation
- Core limma_analysis() function
- Complete Shiny module suite
- Integration with ExpSet_list_extract_function()
- Comprehensive documentation and examples