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