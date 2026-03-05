# iOmeAiFunctions Pipeline Templates

This directory contains pipeline templates for common analysis workflows.

## Available Templates

### 1. Denoiser Pipeline
**File:** `pipelines/denoiser_pipeline_template.R`

Complete workflow for denoising protein microarray data:
- PC removal
- Cutpoint optimization
- AAb calling

**Usage:**
```r
library(iOmeAiFunctions)
# List available templates
list_templates()
# Copy template to current directory
use_template("denoiser_pipeline")