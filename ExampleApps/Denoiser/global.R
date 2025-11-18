#' Complete Denoiser Demo App
#'
#' Full-featured demo with auto-reload during development
#'
#' @export

# Development mode: reload package functions
if (interactive()) {
	local_path <- "../"

	# Document and reload
	message("Documenting package...")
	devtools::document(local_path)

	message("Loading all package functions...")
	devtools::load_all(local_path)

	# Verify key functions loaded
	required_functions <- c(
		"plot_denoise_heatmap",
		"denoise_remove_PCs",
		"mod_denoiser_ui",
		"mod_denoiser_server",
		"mod_eset_selector_ui",
		"mod_eset_selector_server",
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
}

# Load required libraries
library(shiny)
library(shinydashboard)
library(Biobase)
library(DT)
library(ggplot2)
library(pheatmap)

# If not in development, load the package normally
if (!interactive()) {
	library(iOmeAiFunctions)
}


# Run app
message("\n═══════════════════════════════════════════════════════════")
message("🚀 Starting Denoiser App")
message("═══════════════════════════════════════════════════════════")
message("User: DrGarnett")
message("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"))
message("═══════════════════════════════════════════════════════════\n")

