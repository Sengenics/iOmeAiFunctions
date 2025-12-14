#' Create Mode-Specific Module ID
#'
#' @param base_id Character string. The base module ID. 
#' @param mode_suffix Character string. Optional suffix.  If NULL, searches
#'   for MODE_SUFFIX in calling environment.  Returns base_id if not found. 
#'
#' @return Character string. Module ID with suffix (if available).
#'
#' @export
mid <- function(base_id, mode_suffix = NULL) {
	
	# Get suffix from parameter or search for MODE_SUFFIX
	if (is.null(mode_suffix)) {
		for (i in 1:sys.nframe()) {
			if (exists("MODE_SUFFIX", envir = parent.frame(i), inherits = FALSE)) {
				mode_suffix <- get("MODE_SUFFIX", envir = parent.frame(i))
				break
			}
		}
	}
	
	# Return with suffix if available, otherwise just base_id
	if (! is.null(mode_suffix) && mode_suffix != "") {
		paste0(base_id, "_", mode_suffix)
	} else {
		base_id
	}
}