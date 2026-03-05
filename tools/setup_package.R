# tools/setup_package.R
# =============================================================================
# iOmeAiFunctions Package Setup (with renv)
# =============================================================================

cat("\n╔══════════════════════════���═════════════════════════════════╗\n")
cat("║  iOmeAiFunctions Package Setup (renv mode)                ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Setup options
options(
	renv.consent = TRUE,
	renv.config.install.prompt = FALSE,
	install.packages.check.source = "no",
	install.packages.compile.from.source = "never"
)

# Step 0: Initialize renv (if needed) ----
if (!file.exists("renv/activate.R")) {
	cat("Step 0: Initializing renv...\n")
	if (!requireNamespace("renv", quietly = TRUE)) {
		install.packages("renv")
	}
	renv::init()
	message("  ✓ renv initialized")
	cat("\n")
} else {
	cat("Step 0: Activating renv...\n")
	source("renv/activate.R")
	message("  ✓ renv activated")
	cat("\n")
}

# Load requirements
source("tools/package_requirements.R")

# Step 1: Install dependencies via renv ----
cat("Step 1: Installing dependencies (via renv)...\n")

install_if_missing <- function(pkg, type = "cran") {
	if (!requireNamespace(pkg, quietly = TRUE)) {
		message("  Installing: ", pkg)
		if (type == "bioc") {
			if (!requireNamespace("BiocManager", quietly = TRUE)) {
				renv::install("BiocManager")
			}
			renv::install(paste0("bioc::", pkg))
		} else {
			renv::install(pkg)
		}
	} else {
		message("  ✓ ", pkg)
	}
}

# Install CRAN packages
message("\nCRAN packages:")
all_cran <- c(PACKAGE_REQUIREMENTS$core_depends, 
							PACKAGE_REQUIREMENTS$imports_cran,
							PACKAGE_REQUIREMENTS$suggests)
for (pkg in all_cran) {
	install_if_missing(pkg, "cran")
}

# Install Bioconductor packages
message("\nBioconductor packages:")
for (pkg in PACKAGE_REQUIREMENTS$imports_bioc) {
	install_if_missing(pkg, "bioc")
}

cat("\n✓ All dependencies installed\n\n")

# Step 2: Generate DESCRIPTION ----
cat("Step 2: Generating DESCRIPTION...\n")
source("tools/update_description.R")
cat("\n")

# Step 3: Generate R/imports.R ----
cat("Step 3: Generating R/imports.R...\n")
source("tools/update_imports.R")
cat("\n")

# Step 4: Process data ----
cat("Step 4: Processing package data...\n")

if (file.exists("data-raw/PSA_ROs_data.R")) {
	source("data-raw/PSA_ROs_data.R")
	if (exists("PSA_ROs_pkg")) {
		usethis::use_data(PSA_ROs_pkg, overwrite = TRUE)
		usethis::use_data(PSA_ROs_pkg, internal = TRUE, overwrite = TRUE)
		message("  ✓ PSA_ROs_pkg data added")
	}
}

if (file.exists("data-raw/ExpSet_data.R")) {
	source("data-raw/ExpSet_data.R")
	message("  ✓ ExpSet example data processed")
}

cat("\n")

# Step 5: Clean previous build ----
cat("Step 5: Cleaning previous build...\n")

if (file.exists("NAMESPACE")) {
	unlink("NAMESPACE")
	message("  ✓ Removed old NAMESPACE")
}

if (dir.exists("man")) {
	unlink("man", recursive = TRUE)
	message("  ✓ Removed old documentation")
}

cat("\n")

# Step 6: Document package ----
cat("Step 6: Documenting package...\n")

if (requireNamespace("devtools", quietly = TRUE)) {
	devtools::document()
	message("  ✓ Documentation generated")
} else {
	stop("devtools not available")
}

cat("\n")

# Step 7: Install package ----
cat("Step 7: Installing package...\n")
devtools::install(upgrade = "never")
cat("\n")

# Step 8: Snapshot renv ----
cat("Step 8: Updating renv.lock...\n")
renv::snapshot(prompt = FALSE, force = TRUE)
message("  ✓ renv.lock updated with all package versions")
cat("\n")

# Summary ----
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  ✓ Package Setup Complete (renv mode)                     ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("Package:", PACKAGE_REQUIREMENTS$info$name, 
		"v", PACKAGE_REQUIREMENTS$info$version, "\n")
cat("Dependencies:", 
		length(PACKAGE_REQUIREMENTS$core_depends), "core +",
		length(c(PACKAGE_REQUIREMENTS$imports_cran, PACKAGE_REQUIREMENTS$imports_bioc)), 
		"imports\n")
cat("renv.lock: ", nrow(renv::status()$library), " packages locked\n\n")

cat("Next steps:\n")
cat("  • Test: library(iOmeAiFunctions)\n")
cat("  • Check: check_dependencies()\n")
cat("  • Restore on other machine: renv::restore()\n\n")