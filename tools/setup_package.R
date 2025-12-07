# tools/setup_package.R
version = "0.1.3"
release_data = '2025-12-08'

options(
	renv.consent = TRUE,
	renv.config.install.prompt = FALSE,
	renv.config.restore.prompt = FALSE,
	renv.config.snapshot.prompt = FALSE,
	install.packages.check.source = "no",
	install.packages.compile.from.source = "never",
	menu.graphics = FALSE
)

# --- Scaffolding ---
if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

if (file.exists("renv/activate.R")) {
	source("renv/activate.R")
}

usethis::use_mit_license("Shaun Garnett")
usethis::use_readme_rmd()
usethis::use_build_ignore("tools/")

# Source the PSA_ROs vector from the temp directory
source("data-raw/PSA_ROs_data.R")
usethis::use_data(PSA_ROs_pkg, overwrite = TRUE)
usethis::use_data(PSA_ROs_pkg, internal = TRUE, overwrite = TRUE)

# Source and prepare ExpSet example data
source("data-raw/ExpSet_data.R")

# --- Install & sync dependencies ---
source("tools/install_dependencies.R")

# --- Set Version and Encoding if missing ---
desc_path <- "DESCRIPTION"
desc <- readLines(desc_path)

if (!any(grepl("^Version:", desc))) {
	desc <- append(desc, paste("Version:", version), after = grep("^Package:", desc))
	writeLines(desc, desc_path)
	message(paste("✔ DESCRIPTION Version set to", version))
}
if (!any(grepl("^Encoding:", desc))) {
	desc <- append(desc, "Encoding: UTF-8", after = grep("^Version:", desc))
	writeLines(desc, desc_path)
	message("✔ DESCRIPTION Encoding set to UTF-8")
}

# ✅ Clean up DESCRIPTION - remove base packages from Imports
desc <- readLines(desc_path)
base_packages <- c("base", "stats", "utils", "graphics", "grDevices", "datasets", "methods")

in_imports <- FALSE
cleaned_desc <- c()
skip_next <- FALSE

for (i in seq_along(desc)) {
	if (skip_next) {
		skip_next <- FALSE
		next
	}
	
	line <- desc[i]
	
	# Detect Imports section
	if (grepl("^Imports:", line)) {
		in_imports <- TRUE
		cleaned_desc <- c(cleaned_desc, line)
		next
	}
	
	# Detect end of Imports section
	if (in_imports && grepl("^[A-Z][a-zA-Z]+:", line)) {
		in_imports <- FALSE
	}
	
	# Remove base packages from Imports
	if (in_imports) {
		# Check if line contains a base package
		is_base <- any(sapply(base_packages, function(pkg) {
			grepl(paste0("^\\s*", pkg, "\\s*(,|$)"), line)
		}))
		
		if (is_base) {
			message("✔ Removed base package from Imports: ", trimws(line))
			next
		}
	}
	
	cleaned_desc <- c(cleaned_desc, line)
}

writeLines(cleaned_desc, desc_path)
message("✔ DESCRIPTION cleaned of base package imports")

# ✅ Create R/zzz.R for . onLoad and conflict resolution
zzz_content <- '
#\' @keywords internal
.onLoad <- function(libname, pkgname) {
	# Suppress import conflict warnings
	options(
		conflicts.policy = list(
			warn = FALSE,
			error = FALSE
		)
	)
}

#\' @keywords internal
.onAttach <- function(libname, pkgname) {
	# Set package-level conflict preferences
	if (requireNamespace("conflicted", quietly = TRUE)) {
		conflicted::conflict_prefer("exprs", "Biobase", quiet = TRUE)
		conflicted::conflict_prefer("pData", "Biobase", quiet = TRUE)
		conflicted::conflict_prefer("fData", "Biobase", quiet = TRUE)
		conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("mutate", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("combine", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("box", "shinydashboard", quiet = TRUE)
		conflicted::conflict_prefer("renderDataTable", "DT", quiet = TRUE)
		conflicted::conflict_prefer("dataTableOutput", "DT", quiet = TRUE)
	}
}
'

if (!dir.exists("R")) dir.create("R")
writeLines(zzz_content, "R/zzz.R")
message("✔ Created R/zzz.R for conflict resolution")

# ✅ Remove problematic @importFrom in R files
message("✔ Scanning R files for base package imports...")

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

for (r_file in r_files) {
	content <- readLines(r_file, warn = FALSE)
	original_content <- content
	
	# Remove @importFrom for base packages
	content <- content[!grepl("^#'\\s*@importFrom\\s+(base|stats|utils|graphics|grDevices|datasets|methods)\\s+", content)]
	
	# Write back if changed
	if (! identical(content, original_content)) {
		writeLines(content, r_file)
		message("  ✔ Cleaned: ", basename(r_file))
	}
}

# ✅ Clean everything before rebuild
message("✔ Cleaning previous build artifacts...")
unlink("NAMESPACE")
unlink("man", recursive = TRUE)

# Try to clean DLL if devtools is available
if (requireNamespace("devtools", quietly = TRUE)) {
	try(devtools::clean_dll(), silent = TRUE)
}

# --- Finalize ---
message("✔ Documenting package...")
devtools::document()

message("✔ Installing package...")
devtools::install(upgrade = "never")

message("✔ Snapshotting renv...")
renv::snapshot(prompt = FALSE, force = TRUE)

# --- Manage package conflicts (for interactive use) ---
if (!requireNamespace("conflicted", quietly = TRUE)) install.packages("conflicted")
library(conflicted)

# Set conflict preferences for interactive session
conflict_prefer('exprs', 'Biobase')
conflict_prefer('pData', 'Biobase')
conflict_prefer('filter', 'dplyr')
conflict_prefer('select', 'dplyr')
conflicts_prefer(shinydashboard::box)
conflicts_prefer(DT::renderDataTable)
conflicts_prefer(DT::dataTableOutput)

message("✔ Package setup complete!")
message(paste("  Version:", version))
message(paste("  Release:", release_data))