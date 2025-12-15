# GLOBAL : Batch Correction App #####
#'
#' @export

version = "v1.1.0"
run_debug <- ! is.na(Sys.getenv("RSTUDIO", unset = NA))
run_debug = F
print(run_debug)

# Package Management #####

## iOmeAiFunctions ####
if (run_debug) {
	### Local Development ###
	local_path <- "../../../../iOmeAiFunctions/"
	file.exists(local_path)
	message("→ Using LOCAL iOmeAiFunctions at ", local_path)
	
	if (requireNamespace("devtools", quietly = TRUE)) {
		devtools::document(local_path)
	}
	pkgload::load_all(local_path)
	
} else {
	### Production: GitHub Release ###
	if (! requireNamespace("iOmeAiFunctions", quietly = TRUE)) {
		message("→ Installing iOmeAiFunctions from GitHub")
		
		if (!requireNamespace("remotes", quietly = TRUE)) {
			install.packages("remotes")
		}
		
		renv::remove("iOmeAiFunctions")
		renv::purge("iOmeAiFunctions")
		iOmeAiFunction_version = 'BatchCorrect_v1.1.0'
		options(timeout = 300) 
		gh_token <- Sys.getenv("GITHUB_PAT")
		remotes::install_github(
			paste0("Sengenics/iOmeAiFunctions@", iOmeAiFunction_version),
			upgrade = "never",
			force = TRUE,
			auth_token = gh_token
		)
		renv::remove("gert")
		renv::remove("usethis")
		renv::remove("devtools")
		renv::purge("gert")
		renv::snapshot()
		rsconnect::writeManifest()
		
	} else {
		message("→ Using installed iOmeAiFunctions package")
	}
}

library(iOmeAiFunctions)



# Load required packages
library(shiny)
library(shinydashboard)
library(Biobase)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(broom)
library(purrr)
library(sva)  # For ComBat
library(vegan)
library(Rtsne)
library(dendextend)
library(pheatmap)
library(shinyBS)
library(shinyWidgets)



# Set default options
options(shiny.maxRequestSize = 100 * 1024^2)  # 100MB upload limit