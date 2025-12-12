#' @keywords internal
.onLoad <- function(libname, pkgname) {
	# Suppress import conflict warnings
	options(
		conflicts.policy = list(
			warn = FALSE,
			error = FALSE
		)
	)
	
	# Force base set operations to be available in package namespace
	ns <- getNamespace(pkgname)
	assign("setdiff", base::setdiff, envir = ns)
	assign("intersect", base::intersect, envir = ns)
	assign("union", base::union, envir = ns)
	assign("setequal", base::setequal, envir = ns)
	
	namespaceExport(ns, c("setdiff", "intersect", "union", "setequal"))
}

#' @keywords internal
.onAttach <- function(libname, pkgname) {
	# Set package-level conflict preferences
	if (requireNamespace("conflicted", quietly = TRUE)) {
		# Biobase preferences
		conflicted::conflict_prefer("exprs", "Biobase", quiet = TRUE)
		conflicted::conflict_prefer("pData", "Biobase", quiet = TRUE)
		conflicted::conflict_prefer("fData", "Biobase", quiet = TRUE)
		conflicted::conflict_prefer("combine", "Biobase", quiet = TRUE)
		
		# dplyr preferences
		conflicted:: conflict_prefer("filter", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("mutate", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("desc", "dplyr", quiet = TRUE)
		conflicted::conflict_prefer("arrange", "dplyr", quiet = TRUE)
		
		# Base R preferences (CRITICAL - was missing!)
		conflicted::conflict_prefer("setdiff", "base", quiet = TRUE)
		conflicted::conflict_prefer("intersect", "base", quiet = TRUE)
		conflicted::conflict_prefer("union", "base", quiet = TRUE)
		conflicted::conflict_prefer("setequal", "base", quiet = TRUE)
		
		# Other package preferences
		conflicted::conflict_prefer("box", "shinydashboard", quiet = TRUE)
		conflicted::conflict_prefer("renderDataTable", "DT", quiet = TRUE)
		conflicted::conflict_prefer("dataTableOutput", "DT", quiet = TRUE)
		conflicted::conflict_prefer("rename", "plyr", quiet = TRUE)
	}
}