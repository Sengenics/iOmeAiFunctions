# tools/required_packages.R

required_packages = c('AUC','callr','caret','caret','circlize','cluster','cluster','corrplot','data.table','doParallel','dplyr','DT','flextable','foreach','ggforce',
						 'ggplot2','ggplotify','ggpubr','ggrepel','glmnet','greekLetters','gridExtra','gridExtra','gtools','hilbertSimilarity','Hmisc','kableExtra',
						 'lubridate','magick','markdown','markdown','naniar','officer','openxlsx','OptimalCutpoints','paletteer','pals','parallel','PerformanceAnalytics',
						 'pheatmap','plotly','plotROC','plyr','pROC','pspline','psych','purrr','randomForest','RColorBrewer','readr','readtext','reshape','rJava','ROCR','ROCR','Rtsne','Rtsne','rvg','shinyBS','shinyBS','shinybusy',
						 'shinycssloaders','shinydashboard','shinydashboard','shinydisconnect','shinydisconnect','shinyFiles','shinyjs','shinyjs','shinyWidgets','shinyWidgets','stringr','tidyr','tidyverse',
						 'uuid','vegan','vegan','viridis','writexl','xlsx','rlang','shiny','jpg','correlationfunnel')

install_if_missing <- function(pkg) {
	if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
	library(pkg, character.only = TRUE)
}

invisible(lapply(required_packages, install_if_missing))
