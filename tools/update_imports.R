# tools/update_imports.R
# Auto-generate R/imports.R from package_requirements.R

source("tools/package_requirements.R")

# Build roxygen import statements
import_lines <- c(
	"#' Package Imports",
	"#' @name imports",
	"#' @keywords internal",
	"#' Auto-generated - DO NOT EDIT",
	"NULL",
	""
)

# Process specific imports
for (pkg_name in names(PACKAGE_REQUIREMENTS$specific_imports)) {
	funcs <- PACKAGE_REQUIREMENTS$specific_imports[[pkg_name]]
	
	if (length(funcs) == 1 && funcs == "@import") {
		# Full package import
		import_lines <- c(import_lines, paste0("#' @import ", pkg_name))
	} else {
		# Specific functions
		for (func in funcs) {
			import_lines <- c(import_lines, 
												paste0("#' @importFrom ", pkg_name, " ", func))
		}
	}
}

# Write R/imports.R
dir.create("R", showWarnings = FALSE)
writeLines(import_lines, "R/imports.R")
message("✓ R/imports.R generated")
message("  ", length(import_lines) - 5, " import statements")