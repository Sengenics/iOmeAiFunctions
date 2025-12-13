#' @keywords internal
.onLoad <- function(libname, pkgname) {
	# Suppress import conflict warnings during package load
	options(
		conflicts.policy = list(
			warn = FALSE,
			error = FALSE
		)
	)
}

#' @keywords internal
.onAttach <- function(libname, pkgname) {
	# Set package-level conflict preferences (only if conflicted is installed)
	if (requireNamespace("conflicted", quietly = TRUE)) {
		# Biobase preferences (for ExpressionSet operations)
		conflicted::conflict_prefer("exprs", "Biobase", quiet = TRUE)
		conflicted::conflict_prefer("pData", "Biobase", quiet = TRUE)
		conflicted::conflict_prefer("fData", "Biobase", quiet = TRUE)
		conflicted::conflict_prefer("combine", "Biobase", quiet = TRUE)
		
		# dplyr preferences (for data manipulation)
		conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("mutate", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("desc", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("arrange", "dplyr", quiet = TRUE)
		
		# DT preferences (for Shiny datatables)
		conflicted::conflict_prefer("dataTableOutput", "DT", quiet = TRUE)
		conflicted::conflict_prefer("renderDataTable", "DT", quiet = TRUE)
		
		# shinydashboard preferences
		conflicted::conflict_prefer("box", "shinydashboard", quiet = TRUE)
		
		# plyr preferences (if you use plyr)
		conflicted::conflict_prefer("rename", "plyr", quiet = TRUE)
	}
}