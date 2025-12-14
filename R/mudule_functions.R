#' Create Mode-Specific Module ID
#'
#' Generates a module ID with a mode-specific suffix appended. This allows
#' the same server logic to work with different UI modes (e.g., "full" vs "simple")
#' by creating unique namespaces for each mode's modules.
#'
#' @param base_id Character string. The base module ID without any suffix.
#' @param mode_suffix Character string. Optional. The suffix to append.  If not
#'   provided, uses the `MODE_SUFFIX` variable from the parent environment.
#'
#' @return Character string. The base ID with the mode suffix appended,
#'   separated by an underscore. 
#'
#' @details
#' By default, this function looks for a `MODE_SUFFIX` variable in the calling
#' environment. This is typically set based on the current interface mode
#' before sourcing server logic files.
#'
#' Alternatively, you can explicitly provide the suffix via the `mode_suffix`
#' parameter for more control or testing purposes.
#'
#' This pattern prevents namespace conflicts when dynamically switching between
#' different UI modes that share the same underlying server logic but need
#' separate reactive contexts. 
#'
#' @examples
#' \dontrun{
#' # Standard usage - relies on MODE_SUFFIX in environment
#' MODE_SUFFIX <- "full"
#' mid("batch_viz")        # Returns "batch_viz_full"
#'
#' # Explicit suffix
#' mid("batch_viz", mode_suffix = "simple")  # Returns "batch_viz_simple"
#'
#' # Use in module server calls
#' mod_batch_visualization_server(
#'   id = mid("batch_viz"),
#'   eset = my_eset,
#'   debug = TRUE
#' )
#' }
#'
#' @seealso
#' Used in dynamic server sourcing patterns where UI mode selection
#' determines which interface variant is loaded.
#'
#' @export
mid <- function(base_id, mode_suffix = NULL) {
	
	# Validate base_id
	if (missing(base_id) || is.null(base_id) || base_id == "") {
		stop("base_id must be a non-empty character string")
	}
	
	# Get suffix from parameter or environment
	if (is.null(mode_suffix)) {
		if (! exists("MODE_SUFFIX", envir = parent.frame(), inherits = TRUE)) {
			stop(
				"MODE_SUFFIX not found in environment. ",
				"Either set MODE_SUFFIX before calling mid() or provide mode_suffix parameter."
			)
		}
		mode_suffix <- get("MODE_SUFFIX", envir = parent.frame(), inherits = TRUE)
	}
	
	# Validate suffix
	if (is.null(mode_suffix) || mode_suffix == "") {
		stop("mode_suffix must be a non-empty character string")
	}
	
	# Create and return the mode-specific ID
	paste0(base_id, "_", mode_suffix)
}