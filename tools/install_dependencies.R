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

required_bioc <- c('pcaMethods', 'imputeLCMD','impute','limma', 'EnhancedVolcano', 'sva', 'metagenomeSeq', 'Biobase')

# --- Install CRAN packages ---
message("Installing CRAN packages...")
for (pkg in required_cran) {
	if (!requireNamespace(pkg, quietly = TRUE)) {
		message(paste("  Installing:", pkg))
		renv::install(pkg)
	} else {
		message(paste("  ✓", pkg, "already installed"))
	}
}

# --- Install Bioconductor packages ---
message("\nInstalling Bioconductor packages...")
if (!requireNamespace("BiocManager", quietly = TRUE)) {
	install.packages("BiocManager")
}

for (pkg in required_bioc) {
	fif (!requireNamespace(pkg, quietly = TRUE)) {
		message(paste("  Installing:", pkg))
		BiocManager::install(pkg, update = FALSE, ask = FALSE)
	} else {
		message(paste("  ✓", pkg, "already installed"))
	}
}

# Tell renv about Bioconductor packages
message("\nUpdating renv snapshot...")
renv::snapshot(prompt = FALSE)

# --- Write NAMESPACE ---
message("\nUpdating NAMESPACE...")
names_file <- "NAMESPACE"
namespace_lines <- c(
	"exportPattern(\"^[[:alpha:]]+\")",
	"export(QC_Spot_Filtering_Report)"
)
writeLines(namespace_lines, names_file)
message("  ✓ NAMESPACE updated")

# --- Update DESCRIPTION with Imports ---
message("\nUpdating DESCRIPTION...")
desc_path <- "DESCRIPTION"
desc <- readLines()

# Remove old Imports block
imports_start <- grep("^Imports:", desc)
if (length(imports_start) > 0) {
	# Find all continuation lines
	next_section <- grep("^[A-Z][a-zA-Z]+:", desc)
	next_section <- next_section[next_section > imports_start[1]]
	end_line <- if(length(next_section) > 0) next_section[1] - 1 else length(desc)
	desc <- desc[-(imports_start[1]:end_line)]
}

# Add new Imports block
all_imports <- sort(unique(c(required_cran, required_bioc)))
imports_block <- c(
	"Imports:",
	paste0("    ", paste(all_imports, collapse = ",\n    "))
)
package_line <- grep("^Package:", desc)[1]
desc <- append(desc, imports_block, after = package_line)
writeLines(desc, desc_path)
message("  ✓ DESCRIPTION Imports updated")

message("\n✅ All dependencies installed successfully!")