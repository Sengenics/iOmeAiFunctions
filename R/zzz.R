
#' @keywords internal
.onLoad <- function(libname, pkgname) {
	# Suppress import conflict warnings
	options(
		conflicts.policy = list(
			warn = FALSE,
			error = FALSE
		)
	)
}

#' @keywords internal
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

