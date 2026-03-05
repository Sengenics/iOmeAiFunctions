#' Denoiser App - Global Setup
#' 
#' Loads iOmeAiFunctions from local source (development) or GitHub (production)

# Detect Environment ----
run_debug <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
run_debug = F
message("Running in ", if(run_debug) "DEVELOPMENT" else "PRODUCTION", " mode")

# Load iOmeAiFunctions ----
if (run_debug) {
	## Local Development ##
	local_path <- "~/iOmeAiFunctions/"
	
	if (!file.exists(local_path)) {
		stop("Local package not found at: ", local_path)
	}
	
	message("→ Loading LOCAL iOmeAiFunctions from ", local_path)
	
	# Document and load
	if (requireNamespace("devtools", quietly = TRUE)) {
		devtools::document(local_path)
	}
	pkgload::load_all(local_path, export_all = FALSE, helpers = FALSE)
	
	# Load dev datasets if available
	if (file.exists("dev_datasets.R")) {
		source("dev_datasets.R")
	}
	
} else {
	## Production: GitHub ##
	if (!requireNamespace("iOmeAiFunctions", quietly = TRUE)) {
		message("→ Installing iOmeAiFunctions from GitHub")
		
		if (!requireNamespace("remotes", quietly = TRUE)) {
			install.packages("remotes")
		}
		
		# Install specific version
		iOmeAiFunction_version <- "v0.1.4"
		iOmeAiFunction_version <- "denoiser_debug"
		remotes::install_github(
			paste0("Sengenics/iOmeAiFunctions@", iOmeAiFunction_version),
			upgrade = "never",
			force = FALSE
		)
	}
	dev_datasets = NULL
	message("→ Using iOmeAiFunctions from GitHub")
}

# Load Package ----
library(iOmeAiFunctions)
print(packageVersion("iOmeAiFunctions"))
!exists("dashboardPage")
#shiny_packages <- c("shiny", "shinydashboard", "DT", "shinyjs")

# Load Shiny Dependencies ----
# (These are imported by iOmeAiFunctions but not re-exported)
# library(shiny)
# library(shinydashboard)
# library(DT)
# 
# # Verify Key Functions ----
# required_functions <- c(
# 	"mod_denoiser_ui",
# 	"mod_denoiser_server",
# 	"mod_pn_limma_ui",
# 	"mod_pn_limma_server"
# )

# missing <- required_functions[!sapply(required_functions, exists)]
# if (length(missing) > 0) {
# 	stop("Missing functions: ", paste(missing, collapse = ", "))
# }

# App Configuration ----
features_basic <- list(
	generate_heatmaps = FALSE,
	generate_roc = FALSE,
	generate_violins = TRUE,
	allow_continuous = FALSE
)

features_advanced <- list(
	generate_heatmaps = TRUE,
	generate_roc = TRUE,
	generate_violins = TRUE,
	allow_continuous = TRUE
)

# Startup Message ----
message("\n═══════════════════════════════════════════════════════════")
message("🚀 Denoiser App Ready")
message("═══════════════════════════════════════════════════════════")
message("Version: ", packageVersion("iOmeAiFunctions"))
message("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message("═══════════════════════════════════════════════════════════\n")



#' run_debug <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
#' #run_debug = F
#' print(run_debug)
#' 
#' # Package Management #####
#' dev_datasets = c()
#' ## iOmeAiFunctions ####
#' if (run_debug) {
#'   ### Local Development ###
#' 	local_path <- "~/iOmeAiFunctions/"
#'   #local_path <- "../../../iOmeAiFunctions/"
#'   file.exists(local_path)
#'   message("→ Using LOCAL iOmeAiFunctions at ", local_path)
#' 
#'   if (requireNamespace("devtools", quietly = TRUE)) {
#'     devtools::document(local_path)
#'   }
#'   pkgload::load_all(local_path)
#'   source('dev_datasets.R')
#' 
#' } else {
#'   ### Production: GitHub Release v0.1.1 ###
#'   if (!requireNamespace("iOmeAiFunctions", quietly = TRUE)) {
#'     message("→ Installing iOmeAiFunctions v0.1.1 from GitHub")
#' 
#'     if (!requireNamespace("remotes", quietly = TRUE)) {
#'       install.packages("remotes")
#'     }
#' 
#'     #detach("package:iOmeAiFunctions", unload = TRUE, force = TRUE)
#'     renv::remove("iOmeAiFunctions")
#'     renv::purge("iOmeAiFunctions")
#'     iOmeAiFunction_version = 'v0.1.3'
#'     #iOmeAiFunction_verion = 'dev_minimal'
#'     remotes::install_github(
#'       paste0("Sengenics/iOmeAiFunctions@",iOmeAiFunction_version),
#'       upgrade = "never",
#'       force = TRUE
#'     )
#'     renv::remove("gert")
#'     renv::remove("usethis")
#'     renv::remove("devtools")
#'     renv::purge("gert")
#'     #renv::restore()
#'     renv::snapshot()
#'     rsconnect::writeManifest()
#'     
#' 
#'   } else {
#'     message("→ Using installed iOmeAiFunctions package")
#'   }
#' }
#' 
#' library(iOmeAiFunctions)
#' exists("mod_pn_limma_ui")
#' 
#' #source('../../R/denoise_ShinyModule.R')
#' #source('../../R/mod_pn_limma.R')
#' 
#' # Development mode: reload package functions
#' # if (interactive()) {
#' # 	local_path <- "../"
#' #
#' # 	# Document and reload
#' # 	message("Documenting package...")
#' # 	devtools::document(local_path)
#' #
#' # 	message("Loading all package functions...")
#' # 	devtools::load_all(local_path)
#' #
#' # 	# Verify key functions loaded
#' # 	required_functions <- c(
#' # 		"plot_denoise_heatmap",
#' # 		"denoise_remove_PCs",
#' # 		"mod_denoiser_ui",
#' # 		"mod_denoiser_server",
#' # 		"mod_eset_selector_ui",
#' # 		"mod_eset_selector_server",
#' # 		"diagnose_ExpSet_list",
#' # 		"quick_inspect_eset"
#' # 	)
#' #
#' # 	for (fn in required_functions) {
#' # 		if (exists(fn)) {
#' # 			message("✓ ", fn, " loaded")
#' # 		} else {
#' # 			warning("✗ ", fn, " NOT loaded")
#' # 		}
#' # 	}
#' # }
#' 
#' # Load required libraries
#' #library(shiny)
#' #library(shinydashboard)
#' # library(Biobase)
#' # library(DT)
#' # library(ggplot2)
#' # library(pheatmap)
#' # library(limma)
#' # library(tidyverse)
#' # library(ggrepel)
#' # library(pheatmap)
#' # library(vegan)
#' # library(pROC)
#' # library(DT)
#' # library(RColorBrewer)
#' # library(reshape2)
#' # library(ggforce) 
#' 
#' # If not in development, load the package normally
#' # if (!interactive()) {
#' # 	library(iOmeAiFunctions)
#' # }
#' 
#' 
#' # Basic mode (Public app)
#' features_basic <- list(
#' 	generate_heatmaps = FALSE,
#' 	generate_roc = FALSE,
#' 	generate_violins = TRUE,
#' 	allow_continuous = FALSE
#' )
#' 
#' # Advanced mode (InHouse app)
#' features_advanced <- list(
#' 	generate_heatmaps = TRUE,
#' 	generate_roc = TRUE,
#' 	generate_violins = TRUE,
#' 	allow_continuous = TRUE
#' )
#' 
#' # Run app
#' message("\n═══════════════════════════════════════════════════════════")
#' message("🚀 Starting Denoiser App")
#' message("═══════════════════════════════════════════════════════════")
#' message("User: DrGarnett")
#' message("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"))
#' message("═══════════════════════════════════════════════════════════\n")
#' 
