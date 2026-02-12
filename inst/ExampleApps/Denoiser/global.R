#' Complete Denoiser Demo App
#'
#' Full-featured demo with auto-reload during development
#'
#' @export


run_debug <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
#run_debug = F
print(run_debug)

# Package Management #####

## iOmeAiFunctions ####
if (run_debug) {
  ### Local Development ###
	local_path <- "../../../../iOmeAiFunctions/"
  #local_path <- "../../../iOmeAiFunctions/"
  file.exists(local_path)
  message("→ Using LOCAL iOmeAiFunctions at ", local_path)

  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::document(local_path)
  }
  pkgload::load_all(local_path)

} else {
  ### Production: GitHub Release v0.1.1 ###
  if (!requireNamespace("iOmeAiFunctions", quietly = TRUE)) {
    message("→ Installing iOmeAiFunctions v0.1.1 from GitHub")

    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }

    #detach("package:iOmeAiFunctions", unload = TRUE, force = TRUE)
    renv::remove("iOmeAiFunctions")
    renv::purge("iOmeAiFunctions")
    iOmeAiFunction_version = 'v0.1.3'
    #iOmeAiFunction_verion = 'dev_minimal'
    remotes::install_github(
      paste0("Sengenics/iOmeAiFunctions@",iOmeAiFunction_version),
      upgrade = "never",
      force = TRUE
    )
    renv::remove("gert")
    renv::remove("usethis")
    renv::remove("devtools")
    renv::purge("gert")
    #renv::restore()
    renv::snapshot()
    rsconnect::writeManifest()
    

  } else {
    message("→ Using installed iOmeAiFunctions package")
  }
}

library(iOmeAiFunctions)
exists("mod_pn_limma_ui")

#source('../../R/denoise_ShinyModule.R')
#source('../../R/mod_pn_limma.R')

# Development mode: reload package functions
# if (interactive()) {
# 	local_path <- "../"
#
# 	# Document and reload
# 	message("Documenting package...")
# 	devtools::document(local_path)
#
# 	message("Loading all package functions...")
# 	devtools::load_all(local_path)
#
# 	# Verify key functions loaded
# 	required_functions <- c(
# 		"plot_denoise_heatmap",
# 		"denoise_remove_PCs",
# 		"mod_denoiser_ui",
# 		"mod_denoiser_server",
# 		"mod_eset_selector_ui",
# 		"mod_eset_selector_server",
# 		"diagnose_ExpSet_list",
# 		"quick_inspect_eset"
# 	)
#
# 	for (fn in required_functions) {
# 		if (exists(fn)) {
# 			message("✓ ", fn, " loaded")
# 		} else {
# 			warning("✗ ", fn, " NOT loaded")
# 		}
# 	}
# }

# Load required libraries
library(shiny)
library(shinydashboard)
library(Biobase)
library(DT)
library(ggplot2)
library(pheatmap)
library(limma)
library(tidyverse)
library(ggrepel)
library(ggforce)
library(reshape2)

# If not in development, load the package normally
# if (!interactive()) {
# 	library(iOmeAiFunctions)
# }


# Basic mode (Public app)
features_basic <- list(
	generate_heatmaps = FALSE,
	generate_roc = FALSE,
	generate_violins = TRUE,
	allow_continuous = FALSE
)

# Advanced mode (InHouse app)
features_advanced <- list(
	generate_heatmaps = TRUE,
	generate_roc = TRUE,
	generate_violins = TRUE,
	allow_continuous = TRUE
)

# Run app
message("\n═══════════════════════════════════════════════════════════")
message("🚀 Starting Denoiser App")
message("═══════════════════════════════════════════════════════════")
message("User: DrGarnett")
message("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"))
message("═══════════════════════════════════════════════════════════\n")

