#' Determine Optimal Number of Cores for Parallel Processing
#'
#' Calculates the optimal number of cores to use based on: 
#' - Available system cores
#' - Number of groups/tasks to process
#' - Whether parallel processing is enabled
#'
#' @param use_parallel Logical.  Enable parallel processing? 
#' @param n_groups Integer. Number of groups or tasks to process. 
#' @param reserve_cores Integer. Number of cores to reserve for system (default 1).
#' @param min_cores Integer. Minimum cores to use (default 1).
#' @param max_cores Integer. Maximum cores to use (default: all available).
#' @param verbose Logical. Print diagnostic messages?
#'
#' @return Integer.  Number of cores to use.
#' @export
get_optimal_cores <- function(use_parallel = TRUE,
															n_groups = NULL,
															reserve_cores = 1,
															min_cores = 1,
															max_cores = NULL,
															verbose = TRUE) {
	
	# If parallel disabled, return 1
	if (!use_parallel) {
		if (verbose) message("Parallel processing disabled")
		return(1)
	}
	
	# Get available cores
	available_cores <- parallel::detectCores()
	
	if (is.null(available_cores) || is.na(available_cores)) {
		warning("Could not detect number of cores, using 1")
		return(1)
	}
	
	# Set max_cores if not specified
	if (is.null(max_cores)) {
		max_cores <- available_cores
	}
	
	# Calculate usable cores (leave some for system)
	usable_cores <- max(min_cores, available_cores - reserve_cores)
	
	# If n_groups specified, don't use more cores than groups
	if (!is.null(n_groups) && n_groups > 0) {
		usable_cores <- min(usable_cores, n_groups)
	}
	
	# Apply min/max constraints
	n_cores <- max(min_cores, min(usable_cores, max_cores))
	
	# Verbose output
	if (verbose) {
		message(sprintf(
			"Parallel processing: using %d core%s (available: %d%s%s)",
			n_cores,
			ifelse(n_cores == 1, "", "s"),
			available_cores,
			if (! is.null(n_groups)) sprintf(", groups: %d", n_groups) else "",
			if (reserve_cores > 0) sprintf(", reserved: %d", reserve_cores) else ""
		))
	}
	
	return(n_cores)
}