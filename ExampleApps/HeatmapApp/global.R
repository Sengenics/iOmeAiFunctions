#' Complete Heatmap Demo App
#'
#' Full-featured demo with auto-reload during development
#'
#' @export


run_debug <- ! is.na(Sys.getenv("RSTUDIO", unset = NA))
#run_debug = F
print(run_debug)

# Package Management #####

## iOmeAiFunctions ####
if (run_debug) {
	### Local Development ###
	local_path <- "../../"
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
		iOmeAiFunction_version = 'v0.1.3'
		remotes::install_github(
			paste0("Sengenics/iOmeAiFunctions@", iOmeAiFunction_version),
			upgrade = "never",
			force = TRUE
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


# Verify key functions loaded
required_functions <- c(
	"mod_expset_import_ui",
	"mod_expset_import_server",
	"mod_eset_selector_ui",
	"mod_eset_selector_server",
	"mod_data_filter_ui",
	"mod_data_filter_server",
	"heatmap_from_eset",
	"heatmap_eset_module_ui",
	"heatmap_eset_module_server",
	"diagnose_ExpSet_list",
	"quick_inspect_eset"
)

for (fn in required_functions) {
	if (exists(fn)) {
		message("✓ ", fn, " loaded")
	} else {
		warning("✗ ", fn, " NOT loaded")
	}
}

# Load required libraries
library(shiny)
library(shinydashboard)
library(Biobase)
library(DT)
library(ggplot2)
library(pheatmap)
library(shinyWidgets)
library(shinycssloaders)

# Run app
message("\n═══════════════════════════════════════════════════════════")
message("🎨 Starting Heatmap App")
message("═══════════════════════════════════════════════════════════")
message("User: DrGarnett")
message("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"))
message("═══════════════════════════════════════════════════════════\n")