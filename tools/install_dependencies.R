# tools/install_dependencies.R

required_cran <- c(
	'AUC', 'callr', 'caret', 'circlize', 'cluster', 'corrplot', 'data.table',
	'doParallel', 'dplyr', 'DT', 'flextable', 'foreach', 'ggforce',
	'ggplot2', 'ggplotify', 'ggpubr', 'ggrepel', 'glmnet', 'greekLetters', 'gridExtra',
	'gtools', 'hilbertSimilarity', 'Hmisc', 'kableExtra', 'lubridate', 'magick', 'magrittr','markdown',
	'naniar', 'officer', 'openxlsx', 'OptimalCutpoints', 'paletteer', 'pals', 'parallel',
	'PerformanceAnalytics', 'pheatmap', 'plotly', 'plotROC', 'plyr', 'pROC', 'pspline',
	'psych', 'purrr', 'randomForest', 'RColorBrewer', 'readr', 'readtext', 'reshape',
	'ROCR', 'Rtsne', 'rvg', 'shinyBS', 'shinybusy', 'shinycssloaders', 'shinydashboard',
	'shinydisconnect', 'shinyFiles', 'shinyjs', 'shinyWidgets', 'stringr', 'tidyr',
	'tidyverse', 'uuid', 'vegan', 'viridis', 'writexl', 'shiny', 'correlationfunnel',
	'tibble'
)

required_bioc <- c('pcaMethods', 'imputeLCMD', 'limma', 'EnhancedVolcano', 'sva', 'metagenomeSeq')

# --- Install everything with renv ---
install_if_missing <- function(pkg) {
	if (!requireNamespace(pkg, quietly = TRUE)) {
		message(paste("Installing:", pkg))
		renv::install(pkg)
	}
}

# CRAN
invisible(lapply(required_cran, install_if_missing))

# Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
for (pkg in required_bioc) install_if_missing(pkg)

# --- Write NAMESPACE ---
names_file <- "NAMESPACE"
namespace_lines <- c(
	"exportPattern(\"^[[:alpha:]]+\")",
	"export(QC_Spot_Filtering_Report)"
)
writeLines(namespace_lines, names_file)
message("\u2714 NAMESPACE updated")

# --- Update DESCRIPTION with Imports ---
desc_path <- "DESCRIPTION"
desc <- readLines(desc_path)

# Remove old Imports block
imports_start <- grep("^Imports:", desc)
if (length(imports_start) > 0) {
	continuation <- which(grepl("^\\t|^ ", desc[(imports_start + 1):length(desc)])) + imports_start
	desc <- desc[-c(imports_start, continuation)]
}

# Add new Imports block
all_imports <- sort(unique(c(required_cran, required_bioc)))
imports_block <- paste0("Imports:\n    ", paste(all_imports, collapse = ",\n    "))
package_line <- grep("^Package:", desc)[1]
desc <- append(desc, imports_block, after = package_line)
writeLines(desc, desc_path)
message("\u2714 DESCRIPTION Imports updated")


