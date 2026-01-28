# File Comparison Shiny App

**Purpose:** Compare protein data between two files  
**Created:** 2026-01-28  
**Organization:** Sengenics  

## Overview

This Shiny app allows users to upload and compare two data files containing protein measurements. It identifies common proteins, unique proteins, and provides visualizations for comparison.

## Features

- **File Upload:** Upload 2 files (CSV, TSV, or TXT format)
- **Data Overview:** Preview loaded data tables
- **Comparison Analysis:**
  - Identify common proteins
  - Find unique proteins in each file
  - Calculate overlap percentage
  - Venn diagram visualization
- **Visualizations:**
  - Correlation heatmap between samples
  - Scatter plots for sample comparison

## Expected File Format

- **First column:** Protein names (character)
- **Remaining columns:** Sample data (numeric)
- **Header row:** Required
- **File types:** CSV, TSV, or TXT

### Example:
```
Protein,Sample1,Sample2,Sample3
ProteinA,123.4,234.5,345.6
ProteinB,456.7,567.8,678.9
ProteinC,789.0,890.1,901.2
```

## Usage

### Running the App

```r
# From R console in app directory
shiny::runApp()

# Or specify ui/server
shiny::shinyApp(ui, server)
```

### Workflow

1. **Upload Files:** Navigate to "Upload Files" tab and select 2 files
2. **Review Data:** Check "Data Overview" to preview loaded data
3. **Compare:** View "Comparison" tab for protein overlap analysis
4. **Visualize:** Explore "Visualization" tab for correlations and scatter plots

## Dependencies

```r
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
```

Install missing packages:
```r
install.packages(c("shiny", "shinydashboard", "data.table", "DT", 
                   "ggplot2", "plotly", "dplyr", "tidyr"))
```

## Technical Details

- **Max file size:** 100 MB
- **Data loading:** Uses `data.table::fread()` for fast file reading
- **Debug mode:** Debug button available when running in RStudio

## File Structure

```
file_comparison_app/
├── global.R       # Libraries, settings, debug flag
├── ui.R           # User interface
├── server.R       # Server logic
├── functions.R    # Helper functions
└── README.md      # This file
```

## Development Notes

This app follows Sengenics development standards:
- ✅ Debug button pattern implemented
- ✅ Documented helper functions
- ✅ Simple, flat structure
- ✅ Follows tidyverse style guide
- ✅ Uses data.table::fread() as specified

## Future Enhancements

- [ ] Export comparison results
- [ ] Statistical tests between files
- [ ] Multiple file comparison (>2 files)
- [ ] Advanced filtering options
- [ ] Downloadable reports

## Contact

**Maintainer:** Sengenics Development Team  
**Questions:** See `developer_guide.md` or contact via Slack