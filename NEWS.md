# iOmeAiFunctions (Development)

## Version 2.1.0 (2025-10-15)

### Major Changes

#### New pFC Analysis Module
* **Modular pFC (penetrance Fold Change) Analysis**: Complete refactor for flexibility and Shiny integration
  - **Breaking change**: Original `pFC.func()` replaced with modular architecture
  - Designed for small sample size autoantibody detection
  - Fisher's exact testing for penetrance frequency comparisons
  - Per-feature, per-sample fold change calculations relative to control baseline
  - Author: @DrGarnett
  - Date: 2025-10-15

### New Functions

#### pFC Core Functions
* **pFC_process()**: Core pFC analysis using ExpressionSet objects
  - Accepts ExpressionSet input for seamless integration with i-Ome AI workflows
  - Returns comprehensive list of data tables (normalized data, FC data, statistics, Fisher's results)
  - Calculates penetrance frequencies and fold changes for two-group comparisons
  - Supports parallel processing via `cores` parameter
  - Optional PSA (Prostate Specific Antigen) flagging via linear modeling
  - Automatically handles baseline calculations using median of control group
  - Author: @DrGarnett
  - Date: 2025-10-15

* **pFC_plot()**: Generate visualizations from pFC results
  - Creates paginated violin plots for significant hits
  - Generates three heatmap variants: manual sort, manual sort row-centered, clustered row-centered
  - Supports additional metadata annotations
  - Returns plot objects for display in Shiny apps
  - Customizable plot dimensions and faceting
  - Author: @DrGarnett
  - Date: 2025-10-15

* **pFC_save()**: Save pFC results to disk
  - Exports all data tables as CSV files
  - Saves plots as PDF files
  - Organized output directory structure
  - Author: @DrGarnett
  - Date: 2025-10-15

* **pFC_analysis()**: Pipeline wrapper for backward compatibility
  - Drop-in replacement for original `pFC.func()`
  - Maintains exact functionality for existing pipelines
  - Automatically builds ExpressionSet from matrix + metadata inputs
  - Calls modular functions internally (process → plot → save)
  - Author: @DrGarnett
  - Date: 2025-10-15

#### pFC Shiny Module
* **pFC_UI()**: Shiny module UI component
  - Interactive parameter controls for all pFC settings
  - Auto-updating group selectors based on selected variable
  - Supports default variable specification via reactive input
  - Framework-agnostic (works with vanilla Shiny or shinydashboard)
  - Optional `use_box` parameter for shinydashboard integration
  - Author: @DrGarnett
  - Date: 2025-10-15

* **pFC_Server()**: Shiny module server component
  - Accepts reactive ExpressionSet and default variable inputs
  - Real-time parameter validation (prevents identical pos/neg groups)
  - Displays results in organized tabs: Summary, Violin Plots, Heatmaps, Data Tables, Download
  - Interactive plot pagination for multiple violin plot pages
  - Download handlers for CSV, PDF, and complete ZIP archives
  - Returns reactive results for use elsewhere in Shiny apps
  - Author: @DrGarnett
  - Date: 2025-10-15

#### Supporting Functions
* **fitPA_StandAlone()**: Parallel Fisher's exact testing for penetrance analysis
  - Renamed from `fitPA()` for clarity
  - Performs Fisher's exact tests on binary penetrance matrices
  - Supports parallel processing via cluster objects
  - Returns odds ratios, confidence intervals, p-values, and adjusted p-values
  - Author: @DrGarnett
  - Date: 2025-10-15

### Improvements

#### ExpressionSet Integration
* All pFC functions now use Biobase::ExpressionSet as primary data structure
  - Expression data in `exprs(eset)` (log2-transformed)
  - Metadata in `pData(eset)`
  - Feature data in `fData(eset)`
* Pipeline wrapper automatically converts matrix + metadata to ExpressionSet

#### Code Quality
* Comprehensive roxygen2 documentation for all pFC functions
* Modular architecture separates processing, plotting, and saving concerns
* Improved error handling with informative messages
* Removed dependency on `limma_func()` for Shiny module (PSA flagging optional)
* Uses `color_distinct()` function from iOmeAiFunctions for heatmap annotations

### Bug Fixes

* **Package Build**: Fixed critical NAMESPACE issue
  - **Root cause**: `tools/install_dependencies.R` was writing `exportPattern("^[[:alpha:]]+")` to NAMESPACE
  - This pattern-based export attempted to export all objects starting with letters
  - Caused "undefined exports" error for internal objects: BG/FG, Annotations, Create, Density, Dynamic, Metric, Plot, QC
  - **Solution**: Removed NAMESPACE writing from `install_dependencies.R` (lines 38-44)
  - Now relies exclusively on roxygen2 `@export` tags via `devtools::document()`
  - Date: 2025-10-15

* **Namespace Conflicts**: Resolved import conflicts
  - Fixed `Biobase::combine` vs `dplyr::combine` conflict
  - Fixed `DT::dataTableOutput` vs `shiny::dataTableOutput` conflict  
  - Updated to `DT::DTOutput()` and `DT::renderDT()` (deprecated function warnings resolved)
  - Date: 2025-10-15

### Documentation

* Added comprehensive pFC analysis documentation package (`?iOmeAiFunctions-pFC`)
* Included usage examples for pipeline, programmatic, and Shiny use cases
* Documented detailed pFC methodology and calculation steps
* Added migration guide from original `pFC.func()` to new modular functions

### Migration Guide

#### From pFC.func() to pFC_analysis()

**For Existing Pipelines (No Changes Required):**
```r
# Old and New - IDENTICAL USAGE
pFC.func(input = clinical_norm[keep, rownames(meta_clinical)],
         metadata = meta_clinical,
         var = "Labels",
         groupPos = "case",
         groupNeg = "control",
         fold_change = 2,
         descriptor = "case_vs_control_pFC",
         p_val = 0.2,
         PSA_flag = TRUE,
         PSA_colname = "PSA_class")

# Simply replace function name:
pFC_analysis(input = clinical_norm[keep, rownames(meta_clinical)],
             metadata = meta_clinical,
             var = "Labels",
             groupPos = "case",
             groupNeg = "control",
             fold_change = 2,
             descriptor = "case_vs_control_pFC",
             p_val = 0.2,
             PSA_flag = TRUE,
             PSA_colname = "PSA_class")
```

**For Programmatic Use (ExpressionSet):**
```r
# New modular approach with ExpressionSet
eset <- Biobase::ExpressionSet(
  assayData = your_expression_matrix,
  phenoData = Biobase::AnnotatedDataFrame(your_metadata)
)

# Process only
results <- pFC_process(eset, var = "disease", 
                       groupPos = "case", groupNeg = "control")

# Generate plots separately  
plots <- pFC_plot(results)

# Access specific results
significant_hits <- results$pfc_significant
all_statistics <- results$pfc_stats
```

**For Shiny Apps:**
```r
# UI
ui <- fluidPage(
  pFC_UI("pfc_module")
)

# Server
server <- function(input, output, session) {
  eset_data <- reactive({ your_expression_set })
  
  pfc_results <- pFC_Server(
    "pfc_module",
    eset_reactive = eset_data,
    default_var_reactive = reactive(input$AP_SampleGroup_column)
  )
}
```


# iOmeAiFunctions (Development)

## Version 2.0.0 (2025-01-08)

### Major Changes

#### New Quality Control Metrics
* **calculate_bg_fg_stats()**: Complete refactor of BG/FG quality assessment
  - **Breaking change**: Renamed from `raw_density_data_o_function()` to `calculate_bg_fg_stats()`
  - Added Signal-to-Background Ratio (SBR) metrics:
    - `SBR_log2`: Log2 signal-to-background ratio (FG_median - BG_median)
    - `SBR_fold`: Fold-change signal-to-background ratio (2^SBR_log2)
  - Added actual distribution overlap calculations:
    - `fg_overlap_pct`: Percentage of FG spots below BG 95th percentile
    - `bg_overlap_pct`: Percentage of BG spots above FG 5th percentile
    - `separation_gap`: Gap between FG 5th percentile and BG 95th percentile
  - Added separation_MAD: Effect size metric (Cohen's d equivalent)
  - Added comprehensive QC flags:
    - `SBR_fail`: Flags samples with <2-fold separation
    - `overlap_fail_pct`: Flags samples with >5% overlap
    - `separation_fail`: Flags samples with <2 MAD separation
    - `overall_BGFG_qc`: Overall PASS/FAIL decision based on all flags
  - Improved pattern matching for `BG_overlap_metric` parameter (now case-insensitive)
  - Supports all metric variants: Mode/Median/Mean with SD/MAD
  - **Backward compatibility**: All legacy metrics retained (BG_overlap, BG_overlap_BG_Percentage)
  - Added input validation with informative error messages
  - Author: @DrGarnett
  - Date: 2025-01-08

### New Functions

#### Density Plot Functions
* **create_density_plot()**: Core density plotting function
  - Renamed from `density_plot_function()` for clarity
  - Generates BG/FG/NetI density distributions with dual y-axes
  - Supports both individual sample and aggregated views
  - Customizable scaling (auto, full, fixed)
  - Reference lines for BG, FG statistics and thresholds
  - Optional Upper BG (red dashed) and Lower NetI (green dashed) lines
  - Fixed deprecated `size` parameter → `linewidth` for ggplot2 compatibility
  - Improved theme with `theme_minimal()` and better legend placement
  - Author: @DrGarnett
  - Date: 2025-01-08

* **create_bg_fg_density_plot()**: Sample-specific density plot wrapper
  - Renamed from `select_density_plot_function()` for clarity
  - Accepts `datCollate` and `QC` objects directly
  - Automatic metric detection (Mean/Median/Mode with SD/MAD)
  - Integrates with `create_density_plot()` for visualization
  - Shows BG overlap statistics and threshold lines
  - Facetable by Sample or custom variables
  - Author: @DrGarnett
  - Date: 2025-01-08

### Improvements

#### Better Metric Interpretation
* **BG_overlap metric**: Enhanced calculation logic
  - Pattern-based metric detection using `grepl()` 
  - Handles all variants: "Mode_sd", "Median_mad", "Mean_sd", etc.
  - Case-insensitive matching (e.g., "median_mad", "MEDIAN_MAD" both work)
  - Falls back to Mode_sd with warning if metric unrecognized
  - Better separation of concerns: metric detection vs. calculation
  
#### Code Quality
* Comprehensive roxygen2 documentation for all functions
  - Full parameter descriptions with data structure details
  - Detailed `@details` sections explaining metrics and calculations
  - `@note` sections with version history
  - Working `@examples` for each function
  - Proper `@import` and `@importFrom` declarations
* Removed all commented/debug code
* Improved variable naming for clarity
* Better error messages and input validation

### Bug Fixes

* **calculate_bg_fg_stats()**: Fixed NULL object error
  - Moved `plot_data` creation before `BG_count_function()` calls
  - Added NULL checks for `datCollate` components
  - Fixed issue where `BG_overlap_metric` could be NULL/NA
  - Prevented "attempt to select less than one element" error
  - Date: 2025-01-08

* **create_density_plot()**: Fixed ggplot2 deprecation warnings
  - Updated `size` → `linewidth` in all geom layers
  - Fixed scale_x_continuous breaks for custom xlim
  - Date: 2025-01-08

### Documentation

* Added comprehensive function documentation following roxygen2 best practices
* Included usage examples for all exported functions
* Documented metric calculations and QC thresholds
* Added version history in `@note` sections
* Cross-referenced related functions with `@seealso`

### Migration Guide

#### From v1.0.0 to v2.0.0

**Function Renames:**
```r
# Old (v1.0.0)
results <- raw_density_data_o_function(datCollate)
BGFG_stats <- results$flag_df

# New (v2.0.0)
results <- calculate_bg_fg_stats(datCollate)
BGFG_stats <- results$BGFG_stats