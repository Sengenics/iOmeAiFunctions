# Denoiser Functions Documentation

## Overview

The denoiser module provides a complete workflow for AAb calling using principal component denoising and optimal cutpoint selection. The functions are designed to work with ExpressionSet objects and are modular for use in both automated pipelines and interactive Shiny applications.

## Installation

```r
# Install from GitHub
devtools::install_github("Sengenics/iOmeAiFunctions")

# Load library
library(iOmeAiFunctions)
```

## Quick Start

### Basic Usage (Pipeline)

```r
library(iOmeAiFunctions)
library(Biobase)

# Load your ExpressionSet
eset <- readRDS("your_expressionset.rds")

# Run complete denoising pipeline
results <- denoise_pipeline(
  eset = eset,
  assay_name = "NetI",
  n_PCs = 7,
  PN_AAbs = c("PSIP1", "MAPK9", "MX1", "UBE2I"),
  exp_PN_AAbs = 6:12,
  output_dir = "AAb_called",
  descriptor = "MyProject_IgG"
)

# Access AAb-called data
aab_data <- results$aab_called_data
```

### Shiny App Usage

```r
# Run denoising for Shiny (returns all intermediate results)
shiny_results <- denoise_shiny(
  eset = eset,
  assay_name = "NetI",
  n_PCs = 3,
  PN_AAbs = expected_AAbs
)

# Access specific results for visualization
denoised_2PC <- shiny_results$denoised_list[[2]]
cutpoint_table <- shiny_results$cutpoint_tables[[2]]
aab_matrix <- shiny_results$aab_called_matrices[[2]][[15]]  # 2 PCs, 15th cutpoint
```

## Core Functions

### 1. Processing Functions

#### `denoise_remove_PCs()`
Removes principal components from expression data to denoise.

```r
denoised <- denoise_remove_PCs(
  eset = eset,
  assay_name = "NetI",
  n_PCs = 3,
  scale = TRUE
)

# Access denoised data
denoised_1PC <- denoised$denoised_data[[1]]
denoised_2PC <- denoised$denoised_data[[2]]
denoised_3PC <- denoised$denoised_data[[3]]

# Check variance explained
print(denoised$variance_explained)
```

#### `denoise_apply_cutpoint()`
Apply threshold to denoised data.

```r
# Singular cutpoint method
cut_data <- denoise_apply_cutpoint(
  denoised_data = denoised_1PC,
  cutpoint = 0.5,
  method = "singular"
)

# MAD method (per-antigen thresholds)
cut_data_mad <- denoise_apply_cutpoint(
  denoised_data = denoised_1PC,
  cutpoint = 2,  # 2 * MAD
  method = "MAD"
)
```

#### `denoise_find_cutpoints()`
Test multiple cutpoints and calculate metrics.

```r
cutpoint_results <- denoise_find_cutpoints(
  denoised_data = denoised_1PC,
  eset = eset,
  PN_column = "Sample_Group",
  PN_value = "Pooled Normal",
  PN_AAbs = c("PSIP1", "MAPK9"),
  cut_seq = seq(0.4, 3, 0.1),
  method = "singular"
)

# View results
View(cutpoint_results)
```

#### `denoise_select_optimal_cutpoint()`
Select best cutpoint based on criteria.

```r
optimal <- denoise_select_optimal_cutpoint(
  cutpoint_results = cutpoint_results,
  exp_PN_AAbs = 6:12
)

print(optimal)
```

### 2. Plotting Functions

#### `plot_denoise_heatmap()`
Create heatmap of denoised or AAb-called data.

```r
plot_denoise_heatmap(
  denoised_data = denoised_1PC,
  eset = eset,
  annotation_cols = c("Sample_Group", "Batch_ID"),
  title = "Denoised Data (1 PC Removed)",
  show_rownames = FALSE
)
```

#### `plot_denoise_borders()`
Create heatmap with black borders around AAb+ cells.

```r
# Row-centered NetI as background
NetI_RC <- t(scale(t(exprs(eset)), scale = FALSE, center = TRUE))

plot_denoise_borders(
  background_data = NetI_RC,
  aab_called_data = cut_data,
  eset = eset,
  annotation_cols = c("Sample_Group"),
  variable = "Sample_Group",
  title = "AAb Borders on NetI"
)
```

#### `plot_cutpoint_summary()`
Visualize cutpoint optimization metrics.

```r
plot_cutpoint_summary(
  cutpoint_results = cutpoint_results,
  optimal_cutpoint = optimal$cutpoint
)
```

#### `plot_denoise_tsne()`
Create t-SNE visualization.

```r
plot_denoise_tsne(
  aab_called_data = cut_data,
  eset = eset,
  color_by = "Sample_Group",
  shape_by = "PSA_class"
)
```

### 3. Save Functions

#### `save_denoise_results()`
Save denoised matrices and plots.

```r
save_denoise_results(
  denoised_results = denoised,
  output_dir = "AAb_called/denoised",
  descriptor = "NetI_IgG",
  eset = eset,
  annotation_cols = c("Sample_Group", "Batch_ID"),
  create_plots = TRUE
)
```

#### `save_cutpoint_results()`
Save cutpoint analysis and AAb-called data.

```r
save_cutpoint_results(
  cutpoint_results = cutpoint_results,
  optimal_cutpoint = optimal,
  aab_called_data = cut_data,
  output_dir = "AAb_called",
  descriptor = "NetI_IgG",
  eset = eset,
  create_plots = TRUE
)
```

### 4. Wrapper Functions

#### `denoise_pipeline()`
Complete automated pipeline.

```r
results <- denoise_pipeline(
  eset = eset,
  assay_name = "NetI",
  n_PCs = 7,
  PN_AAbs = expected_AAbs,
  exp_PN_AAbs = 6:12,
  output_dir = "AAb_called",
  descriptor = "MyProject"
)
```

#### `denoise_shiny()`
Shiny-optimized pipeline.

```r
shiny_results <- denoise_shiny(
  eset = eset,
  assay_name = "NetI",
  n_PCs = 3
)
```

#### `generate_aab_caller_template()`
Create AAb_caller_template.R script.

```r
generate_aab_caller_template(
  output_file = "AAb_caller_template_MyProject.R",
  project_name = "MyProject",
  eset_file = "data/eset.rds",
  assay_name = "NetI",
  PN_AAbs = c("PSIP1", "MAPK9", "MX1"),
  exp_PN_AAbs = 6:12
)
```

### 5. Utility Functions

#### `create_denoise_eset()`
Create ExpressionSet from matrix and metadata.

```r
eset <- create_denoise_eset(
  expr_data = NetI_matrix,
  metadata = meta
)
```

#### `validate_denoise_inputs()`
Check inputs before running pipeline.

```r
validate_denoise_inputs(
  eset = eset,
  assay_name = "NetI",
  PN_column = "Sample_Group",
  PN_value = "Pooled Normal"
)
```

## Workflow Examples

### Example 1: Standard Pipeline

```r
# 1. Load data
eset <- readRDS("eset.rds")

# 2. Validate inputs
validate_denoise_inputs(eset, assay_name = "NetI")

# 3. Run pipeline
results <- denoise_pipeline(
  eset = eset,
  assay_name = "NetI",
  n_PCs = 7,
  PN_AAbs = c("PSIP1", "MAPK9", "MX1", "UBE2I", "PTPN11", "MLH1"),
  exp_PN_AAbs = 6:12,
  output_dir = "AAb_called",
  descriptor = "IgG_SLE_vs_HC"
)

# 4. Extract results
aab_data <- results$aab_called_data
optimal_params <- results$optimal_cutpoint

# 5. Summary statistics
summary_stats <- calculate_aab_summary(
  aab_data, 
  eset, 
  group_by = "Sample_Group"
)
```

### Example 2: Manual Step-by-Step

```r
# 1. Denoise
denoised <- denoise_remove_PCs(eset, assay_name = "NetI", n_PCs = 3)

# 2. Test cutpoints for 2 PCs removed
cutpoints <- denoise_find_cutpoints(
  denoised$denoised_data[[2]],
  eset,
  PN_AAbs = expected_AAbs,
  cut_seq = seq(0.5, 2.5, 0.1)
)

# 3. Select optimal
optimal <- denoise_select_optimal_cutpoint(cutpoints, exp_PN_AAbs = 6:12)

# 4. Apply optimal cutpoint
aab_data <- denoise_apply_cutpoint(
  denoised$denoised_data[[2]],
  cutpoint = optimal$cutpoint
)

# 5. Visualize
plot_denoise_heatmap(aab_data, eset, annotation_cols = "Sample_Group")
```

### Example 3: Shiny App Integration

```r
# In Shiny server function
shinyServer(function(input, output, session) {
  
  # Load data
  eset <- reactive({
    req(input$eset_file)
    readRDS(input$eset_file$datapath)
  })
  
  # Run denoising
  denoise_results <- reactive({
    req(eset())
    denoise_shiny(
      eset = eset(),
      n_PCs = input$n_PCs,
      PN_AAbs = input$PN_AAbs
    )
  })
  
  # Render denoised heatmap
  output$heatmap_denoised <- renderPlot({
    req(denoise_results())
    pc_level <- input$pc_slider
    
    plot_denoise_heatmap(
      denoise_results()$denoised_list[[pc_level]],
      eset(),
      annotation_cols = input$annotation_cols
    )
  })
  
  # Render cutpoint table
  output$cutpoint_table <- renderDT({
    req(denoise_results())
    pc_level <- input$pc_slider
    denoise_results()$cutpoint_tables[[pc_level]]
  })
  
  # Render AAb-called heatmap
  output$heatmap_aab <- renderPlot({
    req(denoise_results())
    pc_level <- input$pc_slider
    cutpoint_idx <- input$cutpoint_slider
    
    aab_data <- denoise_results()$aab_called_matrices[[pc_level]][[cutpoint_idx]]
    
    plot_denoise_heatmap(
      aab_data,
      eset(),
      annotation_cols = input$annotation_cols,
      title = "AAb-Called Data"
    )
  })
})
```

## Parameter Guide

### Denoising Parameters

- **n_PCs**: Number of PCs to remove (typically 1-7)
  - Start with 1-3 for most datasets
  - More PCs = more aggressive denoising
  
- **method**: Cutting method
  - `"singular"`: Single threshold for all antigens (default)
  - `"MAD"`: Median Absolute Deviation-based per-antigen thresholds
  - `"AAD"`: Average Absolute Deviation-based per-antigen thresholds

- **cut_seq**: Range of cutpoints to test
  - Default: `seq(0.4, 3, 0.1)`
  - Lower values = more permissive (more AAb calls)
  - Higher values = more stringent (fewer AAb calls)

### Selection Criteria

- **exp_PN_AAbs**: Expected range of PN AAb count
  - Based on limma analysis of PN vs clinical samples
  - Example: If limma finds 8 AAbs elevated in PNs, use `6:12`

- **PSA threshold**: Maximum allowed PSA+ rate (default 5%)

## Output Structure

### Pipeline Output Directory

```
AAb_called/
├── denoised_matrices/
│   ├── NetI_1PCs_removed.csv
│   ├── NetI_2PCs_removed.csv
│   ├── heatmap_NetI_1PCs_removed.pdf
│   └── ...
├── cutpoint_analysis/
│   ├── NetI_cutpoint_analysis.csv
│   ├── NetI_optimal_cutpoint.csv
│   ├── NetI_AAb_called_optimal.csv
│   ├── NetI_AAb_call_rates.csv
│   └── ...
├── plots/
│   ├── NetI_cutpoint_summary.pdf
│   ├── NetI_AAb_borders_NetI.pdf
│   └── ...
└── NetI_MASTER_SUMMARY.txt
```

## Tips and Best Practices

1. **Always validate inputs** before running the full pipeline
2. **Start with fewer PCs** (1-3) and increase if needed
3. **Review cutpoint_summary plots** to understand trade-offs
4. **Check PN AAb hit rate** - should be >65% for good quality
5. **Monitor ZZ control rates** - should approach 0%
6. **Use AAb borders plots** to visually validate calls
7. **Save intermediate results** for troubleshooting

## Troubleshooting

### Common Issues

**Issue**: "No cutpoints meet PN AAb count criteria"
- **Solution**: Widen `exp_PN_AAbs` range or adjust `cut_seq`

**Issue**: High PSA+ rates
- **Solution**: Use more stringent cutpoints or remove more PCs

**Issue**: Low TP:FP ratios
- **Solution**: Use fewer PCs or more permissive cutpoints

**Issue**: t-SNE fails
- **Solution**: Need at least 10 samples; check for zero-only samples

## Citation

If you use these functions, please cite:
```
Lennard KL, et al. (2025). iOmeAiFunctions: R package for autoantibody analysis.
Sengenics Corporation. https://github.com/Sengenics/iOmeAiFunctions
```

## Support

For questions or issues:
- GitHub Issues: https://github.com/Sengenics/iOmeAiFunctions/issues
- Email: support@sengenics.com