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
