# Heatmap Viewer App

Interactive heatmap visualization for ExpressionSet objects with multi-level filtering and comprehensive annotation support.

## Features

- **ExpSet Import**: Upload custom ExpressionSet data or use package defaults
- **Assay Selection**: Choose which data matrix to visualize
- **Multi-Level Filtering**: Cascading filters for samples AND features
- **Dual Annotations**: Both sample (column) and feature (row) annotations
- **Export Options**: Download plots, data, or filtered ExpressionSets
- **Debug Mode**: Integrated debugging and diagnostics

## Quick Start

### Run Locally

```r
# From iOmeAiFunctions root directory
shiny::runApp("ExampleApps/HeatmapApp")
```

### Or from RStudio

Open `ExampleApps/HeatmapApp/ui.R` or `server.R` and click "Run App"

## Workflow

### 1. Data Selection

**Upload Data (Optional):**
- Click "Browse..." and select an `. rds` file containing an ExpressionSet or list of ExpressionSets
- Click "Load ExpSet File"
- App validates the data structure

**Or Use Default Data:**
- App automatically loads from package data if available

**Select ExpressionSet:**
- Choose which ExpressionSet to visualize from the dropdown
- View summary showing samples, features, and available assays

### 2. Configure Heatmap

**Select Assay:**
- Choose which data matrix to display (e.g., `exprs`, `log2_exprs`, `normalized`)

**Choose Annotations:**

*Sample Annotations (pData):*
- Select which sample metadata columns to display as colored bars above the heatmap
- Examples: Sample_Group, Disease_State, Treatment, Age

*Feature Annotations (fData):*
- Select which feature metadata columns to display as colored bars left of the heatmap
- Examples: Protein_Family, Function, Chromosome

**Apply Filters:**

*Sample Filters (Cascading):*
1. Click "Add Filter Level"
2. Select a pData column (e.g., Sample_Group)
3. Choose which values to include (e.g., "Clinical")
4. Add another filter level to further narrow (e.g., Disease_State = "Cancer")
5. Each level filters based on previous selections (AND logic)

*Feature Filters (Cascading):*
1.  Click "Add Filter Level"
2. Select an fData column (e.g., Protein_Family)
3.  Choose which values to include (e. g., "Kinase")
4. Add additional levels as needed

**Example Multi-Level Filter:**
```
Sample Filter Level 1: Sample_Group = ["Clinical"]
  └─> Shows only Clinical samples
      
Sample Filter Level 2: Disease_State = ["Cancer", "Pre-disease"]
  └─> Shows only Cancer or Pre-disease samples within Clinical group
      
Sample Filter Level 3: Age = [">50"]
  └─> Shows only patients over 50 with Cancer/Pre-disease in Clinical group
```

### 3.  View Heatmap

Click **"View Heatmap →"** to generate visualization

The heatmap displays:
- Expression values as color-coded cells
- Hierarchical clustering of samples (columns) and features (rows)
- Sample annotations as colored bars at top
- Feature annotations as colored bars on left
- Automatic row/column name display for small datasets

### 4. Export Results

**Download Plot (PNG):**
- High-resolution image of the current heatmap
- Includes all annotations and clustering

**Download Data (CSV):**
- Filtered expression matrix
- Can be opened in Excel or imported to other tools

**Download Filtered ExpSet (RDS):**
- Complete ExpressionSet object with only filtered samples/features
- Preserves all metadata and assay data
- Can be loaded into R for further analysis

## Data Requirements

### ExpressionSet Structure

Your `. rds` file must contain either:

1. **Single ExpressionSet:**
```r
library(Biobase)
eset <- ExpressionSet(
  assayData = exprs_matrix,
  phenoData = AnnotatedDataFrame(sample_metadata),
  featureData = AnnotatedDataFrame(feature_metadata)
)
saveRDS(eset, "my_data.rds")
```

2. **List of ExpressionSets:**
```r
ExpSet_list <- list(
  raw_data = eset_raw,
  normalized_data = eset_norm,
  log2_data = eset_log2
)
saveRDS(ExpSet_list, "my_data.rds")
```

### Required Components

**Essential:**
- `exprs()`: Expression matrix (features × samples)
- `pData()`: Sample metadata (at least 1 column)

**Optional but Recommended:**
- `fData()`: Feature metadata (for row annotations)
- Multiple assay data elements (for assay selection)

## Debug Mode

### Available Tools

**🔍 Enter Debug Mode:**
- Pauses execution in browser mode
- Inspect all reactive values
- Run diagnostic commands interactively

**📊 Run Full Diagnostics:**
- Prints comprehensive ExpSet structure
- Lists all available metadata
- Shows data dimensions and types

**⚡ Quick Check:**
- Fast validation of data structure
- Confirms ExpSet integrity

### Common Debug Commands

```r
# In debug mode browser:

# Inspect selected ExpressionSet
str(eset_selected())
quick_inspect_eset(eset_selected())

# Check available assays
Biobase::assayDataElementNames(eset_selected())

# View metadata
colnames(Biobase::pData(eset_selected()))
colnames(Biobase::fData(eset_selected()))

# Check filters
sample_filter$filtered_indices()
sample_filter$n_filtered()
feature_filter$filtered_indices()

# View current configuration
input$assay_name
input$sample_anno_cols
input$feature_anno_cols

# Exit debug mode
c  # Continue execution
Q  # Quit browser
```

## Tips & Tricks

### Performance

- **Large Datasets**: Feature/sample filters are especially useful to reduce heatmap complexity
- **Row/Column Names**: Automatically hidden for datasets > 50 rows/columns
- **Plot Height**: Dynamically adjusts based on number of features displayed

### Effective Filtering

1. Start broad, then narrow with each filter level
2. Use sample filters to focus on specific experimental groups
3. Use feature filters to focus on specific protein families or pathways
4. Remove filter levels you don't need by clicking "Remove"

### Annotation Strategy

**Sample Annotations:**
- Start with the most important grouping variable (e.g., Sample_Group)
- Add clinical variables (Disease_State, Treatment)
- Add technical variables (Batch, Run_Date) to check for batch effects

**Feature Annotations:**
- Protein classification (Family, Function, Pathway)
- Genomic location (Chromosome, Position)
- Expression characteristics (Variance, Mean_Expression)

## Troubleshooting

### "No data to display"
- **Cause**: All samples or features filtered out
- **Solution**: Remove or adjust filter levels

### "AssayData element not found"
- **Cause**: Selected assay doesn't exist in ExpSet
- **Solution**: Check available assays in Data Selection tab

### "Feature metadata not available"
- **Cause**: ExpSet doesn't have fData
- **Solution**: Feature annotations won't be available, but heatmap will still work

### App crashes on upload
- **Cause**: Invalid file format or corrupted data
- **Solution**: Verify file is valid .rds with ExpressionSet object

## Example Use Cases

### 1. Quality Control
- Load raw data
- No filters
- Annotate by Batch and Sample_Type
- Look for batch effects or outliers

### 2. Differential Expression Visualization
- Load normalized data
- Filter samples: Disease vs Healthy
- Filter features: Significant proteins (p < 0.05)
- Annotate by Disease_State and Protein_Family

### 3.  Pathway Analysis
- Load any data
- Filter features: Specific pathway (e.g., "Apoptosis")
- Filter samples: Treatment groups
- Annotate by Treatment and Timepoint

### 4. Exploratory Analysis
- Load normalized data
- No filters initially
- Add sample annotations for all clinical variables
- Identify interesting patterns
- Add filters to focus on specific subgroups

## Technical Details

- **Clustering**: Hierarchical clustering with euclidean distance
- **Colors**: Automatically generated distinct palettes using `color_distinct()`
- **Export Format**: PNG (300 dpi), CSV (UTF-8), RDS (R version 4.x)
- **Dependencies**: Biobase, pheatmap, shiny, shinydashboard

## Support

For issues or questions:
- Check Debug Mode output
- Review Data Summary in Data Selection tab
- Run Full Diagnostics
- Contact: DrGarnett@Sengenics

## Version

Version 1.0 - Initial release (2025-11-26)