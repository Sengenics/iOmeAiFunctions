# tools/install_dependencies.R

required_cran = c('AUC','callr','caret','caret','circlize','cluster','cluster','corrplot','data.table','doParallel','dplyr','DT','flextable','foreach','ggforce',
						 'ggplot2','ggplotify','ggpubr','ggrepel','glmnet','greekLetters','gridExtra','gridExtra','gtools','hilbertSimilarity','Hmisc','kableExtra',
						 'lubridate','magick','markdown','markdown','naniar','officer','openxlsx','OptimalCutpoints','paletteer','pals','parallel','PerformanceAnalytics',
						 'pheatmap','plotly','plotROC','plyr','pROC','pspline','psych','purrr','randomForest','RColorBrewer','readr','readtext','reshape','ROCR','ROCR','Rtsne','Rtsne','rvg','shinyBS','shinyBS','shinybusy',
						 'shinycssloaders','shinydashboard','shinydashboard','shinydisconnect','shinydisconnect','shinyFiles','shinyjs','shinyjs','shinyWidgets','shinyWidgets','stringr','tidyr','tidyverse',
						 'uuid','vegan','vegan','viridis','writexl','rlang','shiny','correlationfunnel')
required_bioc <- c('pcaMethods', 'imputeLCMD', 'limma', 'EnhancedVolcano', 'sva', 'metagenomeSeq','jpeg')
required_github <- c()

temporarily_removed = c('rJava','xlsx','jpg')

install_if_missing <- function(pkg, installer) {
	if (!requireNamespace(pkg, quietly = TRUE)) installer(pkg)
}

# CRAN
lapply(required_cran, install_if_missing, installer = install.packages)

# Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
lapply(required_bioc, install_if_missing, installer = BiocManager::install)

# GitHub
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
lapply(required_github, function(pkg) devtools::install_github(pkg, quiet = TRUE))


# --- Create or overwrite R/zzz.R to auto-load dependencies silently ---

zzz_path <- file.path("R", "zzz.R")

zzz_content = paste("
.onLoad <- function(libname, pkgname) {
  packages_to_load <- ",c(required_cran,required_bioc),"

  for (pkg in packages_to_load) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
      )
    }
  }
}
")

writeLines(zzz_content, con = zzz_path)
message("✔ Auto-generated R/zzz.R to load dependencies silently on package load.")




# 2. Read DESCRIPTION file as text
desc_path <- "DESCRIPTION"
desc_lines <- readLines(desc_path)

# 3. Remove existing Imports section (simple heuristic)
imports_start <- grep("^Imports:", desc_lines)
if (length(imports_start) > 0) {
	# Remove Imports line plus any continuation lines (starting with whitespace)
	imports_end <- which(!grepl("^\\s", desc_lines[(imports_start+1):length(desc_lines)]))
	imports_end <- ifelse(length(imports_end) == 0, length(desc_lines), imports_start + imports_end[1])
	desc_lines <- desc_lines[-(imports_start:imports_end)]
}

# 4. Create new Imports text with all CRAN and Bioconductor packages
all_imports <- sort(unique(c(required_cran, required_bioc)))
imports_text <- paste0("Imports:\n    ", paste(all_imports, collapse = ",\n    "))

# 5. Insert new Imports section after 'Package:' line (or at line 2)
package_line <- grep("^Package:", desc_lines)[1]
desc_lines <- append(desc_lines, imports_text, after = package_line)

# 6. Write back DESCRIPTION
writeLines(desc_lines, desc_path)

message("✔ DESCRIPTION Imports updated from install_dependencies.R")

