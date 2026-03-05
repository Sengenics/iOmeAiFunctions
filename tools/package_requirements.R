# tools/package_requirements.R
# =============================================================================
# Package Requirements for iOmeAiFunctions
# =============================================================================
# 
# ADD NEW PACKAGES HERE - everything else is automatic!
#
# Categories:
#   - core_depends: Load automatically when user loads iOmeAiFunctions
#   - imports_cran: Available to package code (CRAN)
#   - imports_bioc: Available to package code (Bioconductor)
#   - suggests: Optional packages
# =============================================================================

# Package metadata
PACKAGE_INFO <- list(
	name = "iOmeAiFunctions",
	version = "0.1.4",
	title = "Protein Microarray Analysis Tools",
	description = "Complete toolkit for protein microarray analysis including denoising, differential expression with limma, and interactive visualization.",
	author = "Shaun Garnett",
	email = "shaun.garnett@standardbio.com",
	license = "MIT + file LICENSE",
	encoding = "UTF-8",
	r_version = "4.0.0"
)

# Core dependencies (loaded automatically)
CORE_DEPENDS <- c(
	"dplyr",
	"ggplot2", 
	"tidyr"
)

# CRAN imports (available to your functions)
IMPORTS_CRAN <- c(
	"AUC",
	"callr",
	"caret",
	"circlize",
	"cluster",
	"correlationfunnel",
	"corrplot",
	"data.table",
	"doParallel",
	"DT",
	"flextable",
	"foreach",
	"ggforce",
	"ggplotify",
	"ggpubr",
	"ggrepel",
	"glmnet",
	"greekLetters",
	"gridExtra",
	"gtools",
	"hilbertSimilarity",
	"Hmisc",
	"kableExtra",
	"lubridate",
	"magick",
	"magrittr",
	"markdown",
	"naniar",
	"officer",
	"openxlsx",
	"OptimalCutpoints",
	"paletteer",
	"pals",
	"parallel",
	"PerformanceAnalytics",
	"pheatmap",
	"plotly",
	"plotROC",
	"plyr",
	"pROC",
	"pspline",
	"psych",
	"purrr",
	"randomForest",
	"RColorBrewer",
	"readr",
	"readtext",
	"reshape",
	"reshape2",
	"ROCR",
	"rvg",
	"Rtsne",
	"shiny",
	"shinyBS",
	"shinybusy",
	"shinycssloaders",
	"shinydashboard",
	"shinydisconnect",
	"shinyFiles",
	"shinyjs",
	"shinyWidgets",
	"stringr",
	"tibble",
	"tidyverse",
	"uuid",
	"vegan",
	"viridis",
	"writexl"
)

# Bioconductor imports
IMPORTS_BIOC <- c(
	"Biobase",
	"limma",
	"pcaMethods",
	"imputeLCMD",
	"impute",
	"EnhancedVolcano",
	"sva",
	"metagenomeSeq"
)

# Suggested packages (optional)
SUGGESTS <- c(
	"conflicted",
	"testthat",
	"knitr",
	"rmarkdown",
	"usethis"
)

# Specific imports (for R/imports.R generation)
SPECIFIC_IMPORTS <- list(
	# Core tidyverse
	dplyr = "@import",
	ggplot2 = "@import",
	tidyr = "@import",
	
	# Bioconductor
	Biobase = c("ExpressionSet", "AnnotatedDataFrame", "exprs", "pData", 
							"fData", "assayData", "assayDataElement", "assayDataElementNames"),
	limma = c("makeContrasts", "lmFit", "contrasts.fit", "eBayes", "topTable"),
	
	# Visualization
	pheatmap = "pheatmap",
	ggrepel = "geom_text_repel",
	ggforce = c("facet_wrap_paginate", "n_pages"),
	
	# Data manipulation
	tibble = c("rownames_to_column", "column_to_rownames"),
	reshape2 = "melt",
	
	# Statistics
	vegan = "vegdist",
	pROC = c("roc", "auc"),
	
	# Base R (needed for proper functionality)
	stats = c("model.matrix", "hclust", "prcomp", "cor", "sd", "median"),
	utils = c("write.csv", "read.csv")
)

# Export all
PACKAGE_REQUIREMENTS <- list(
	info = PACKAGE_INFO,
	core_depends = sort(unique(CORE_DEPENDS)),
	imports_cran = sort(unique(IMPORTS_CRAN)),
	imports_bioc = sort(unique(IMPORTS_BIOC)),
	suggests = sort(unique(SUGGESTS)),
	specific_imports = SPECIFIC_IMPORTS
)