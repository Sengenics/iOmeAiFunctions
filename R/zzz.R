.onLoad <- function(libname, pkgname) {
  packages_to_load <- c("AUC", "callr", "caret", "caret", "circlize", "cluster", "cluster", "corrplot", "data.table", "doParallel", "dplyr", "DT", "flextable", "foreach", "ggforce", "ggplot2", "ggplotify", "ggpubr", "ggrepel", "glmnet", "greekLetters", "gridExtra", "gridExtra", "gtools", "hilbertSimilarity", "Hmisc", "kableExtra", "lubridate", "magick", "markdown", "markdown", "naniar", "officer", "openxlsx", "OptimalCutpoints", "paletteer", "pals", "parallel", "PerformanceAnalytics", "pheatmap", "plotly", "plotROC", "plyr", "pROC", "pspline", "psych", "purrr", "randomForest", "RColorBrewer", "readr", "readtext", "reshape", "ROCR", "ROCR", "Rtsne", "Rtsne", "rvg", "shinyBS", "shinyBS", "shinybusy", "shinycssloaders", "shinydashboard", "shinydashboard", "shinydisconnect", "shinydisconnect", "shinyFiles", "shinyjs", "shinyjs", "shinyWidgets", "shinyWidgets", "stringr", "tidyr", "tidyverse", "uuid", "vegan", "vegan", "viridis", "writexl", "rlang", "shiny", "correlationfunnel", "pcaMethods", "imputeLCMD", "limma", "EnhancedVolcano", "sva", "metagenomeSeq", "jpeg")

  for (pkg in packages_to_load) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
      )
    }
  }
}

