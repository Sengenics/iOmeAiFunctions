# tools/update_description.R
# Auto-generate DESCRIPTION from package_requirements.R

source("tools/package_requirements.R")

# Build DESCRIPTION content
desc_lines <- c(
	paste0("Package: ", PACKAGE_REQUIREMENTS$info$name),
	"Type: Package",
	paste0("Title: ", PACKAGE_REQUIREMENTS$info$title),
	paste0("Version: ", PACKAGE_REQUIREMENTS$info$version),
	paste0("Authors@R: person('", 
				 strsplit(PACKAGE_REQUIREMENTS$info$author, " ")[[1]][1], "', '",
				 strsplit(PACKAGE_REQUIREMENTS$info$author, " ")[[1]][2], "',"),
	paste0("    email = '", PACKAGE_REQUIREMENTS$info$email, "',"),
	"    role = c('aut', 'cre'))",
	paste0("Description: ", PACKAGE_REQUIREMENTS$info$description),
	paste0("License: ", PACKAGE_REQUIREMENTS$info$license),
	paste0("Encoding: ", PACKAGE_REQUIREMENTS$info$encoding),
	"Roxygen: list(markdown = TRUE)",
	"RoxygenNote: 7.3.3",
	"LazyData: true"
)

# Add Depends
if (length(PACKAGE_REQUIREMENTS$core_depends) > 0) {
	depends_line <- c(
		"Depends:",
		paste0("    R (>= ", PACKAGE_REQUIREMENTS$info$r_version, "),")
	)
	for (i in seq_along(PACKAGE_REQUIREMENTS$core_depends)) {
		pkg <- PACKAGE_REQUIREMENTS$core_depends[i]
		comma <- if (i < length(PACKAGE_REQUIREMENTS$core_depends)) "," else ""
		depends_line <- c(depends_line, paste0("    ", pkg, comma))
	}
	desc_lines <- c(desc_lines, depends_line)
}

# Add Imports
all_imports <- c(PACKAGE_REQUIREMENTS$imports_cran, 
								 PACKAGE_REQUIREMENTS$imports_bioc)

if (length(all_imports) > 0) {
	imports_line <- "Imports:"
	for (i in seq_along(all_imports)) {
		pkg <- all_imports[i]
		comma <- if (i < length(all_imports)) "," else ""
		imports_line <- c(imports_line, paste0("    ", pkg, comma))
	}
	desc_lines <- c(desc_lines, imports_line)
}

# Add Suggests
if (length(PACKAGE_REQUIREMENTS$suggests) > 0) {
	suggests_line <- "Suggests:"
	for (i in seq_along(PACKAGE_REQUIREMENTS$suggests)) {
		pkg <- PACKAGE_REQUIREMENTS$suggests[i]
		comma <- if (i < length(PACKAGE_REQUIREMENTS$suggests)) "," else ""
		suggests_line <- c(suggests_line, paste0("    ", pkg, comma))
	}
	desc_lines <- c(desc_lines, suggests_line)
}

# Write DESCRIPTION
writeLines(desc_lines, "DESCRIPTION")
message("✓ DESCRIPTION generated")
message("  ", length(PACKAGE_REQUIREMENTS$core_depends), " core dependencies")
message("  ", length(all_imports), " imports")
message("  ", length(PACKAGE_REQUIREMENTS$suggests), " suggested packages")