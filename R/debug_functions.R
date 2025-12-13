# Debug Utilities for Shiny Modules
# Provides consistent, hierarchical debug messaging with module ID support

#' Debug Message
#'
#' Print a message only when debug mode is enabled
#'
#' @param ...   Message components to paste together
#' @param debug Enable debug output (default FALSE)
#' @param prefix Prefix for the message (default "🔍")
#' @param level Indentation level (default 0)
#' @param id Module ID to include in output (optional)
#'
#' @export
dmsg <- function(..., debug = FALSE, prefix = "🔍", level = 0, id = NULL) {
	if (isTRUE(debug)) {
		indent <- paste(rep("  ", level), collapse = "")
		id_str <- if (!is.null(id)) paste0("[", id, "] ") else ""
		message(indent, prefix, " ", id_str, ...)
	}
}

#' Debug Success Message
#'
#' @param ...  Message components
#' @param debug Enable debug output
#' @param level Indentation level
#' @param id Module ID (optional)
#'
#' @export
dsuccess <- function(..., debug = FALSE, level = 0, id = NULL) {
	dmsg(... , debug = debug, prefix = "✓", level = level, id = id)
}

#' Debug Error Message
#'
#' @param ... Message components
#' @param debug Enable debug output
#' @param level Indentation level
#' @param id Module ID (optional)
#'
#' @export
derror <- function(..., debug = FALSE, level = 0, id = NULL) {
	dmsg(..., debug = debug, prefix = "✖", level = level, id = id)
}

#' Debug Arrow Message
#'
#' @param ... Message components
#' @param debug Enable debug output
#' @param level Indentation level
#' @param id Module ID (optional)
#'
#' @export
darrow <- function(..., debug = FALSE, level = 0, id = NULL) {
	dmsg(..., debug = debug, prefix = "→", level = level, id = id)
}

#' Debug Warning Message
#'
#' @param ... Message components
#' @param debug Enable debug output
#' @param level Indentation level
#' @param id Module ID (optional)
#'
#' @export
dwarn <- function(..., debug = FALSE, level = 0, id = NULL) {
	dmsg(..., debug = debug, prefix = "⚠️", level = level, id = id)
}

#' Debug Section Start
#'
#' @param title Section title
#' @param id Module ID (optional)
#' @param debug Enable debug output
#'
#' @export
dstart <- function(title, id = NULL, debug = FALSE) {
	if (isTRUE(debug)) {
		if (!is.null(id)) {
			message("\n", "═══ ", title, " :  ", id, " ═══")
		} else {
			message("\n", "═══ ", title, " ═══")
		}
	}
}

#' Debug Section End
#'
#' @param title Section title
#' @param id Module ID (optional)
#' @param debug Enable debug output
#'
#' @export
dend <- function(title, id = NULL, debug = FALSE) {
	if (isTRUE(debug)) {
		if (!is.null(id)) {
			message("═══ END: ", title, " : ", id, " ═══\n")
		} else {
			message("═══ END:  ", title, " ═══\n")
		}
	}
}

#' Debug Try-Catch Wrapper
#'
#' Wraps code in tryCatch with debug output
#'
#' @param expr Expression to evaluate
#' @param desc Description of what's being tried
#' @param debug Enable debug output
#' @param level Indentation level
#' @param id Module ID (optional)
#' @param on_error What to return on error:  "stop" (re-throw), "null" (return NULL), or "expr" (return expr unevaluated)
#'
#' @return Result of expr, or behavior specified by on_error
#' @export
dtry <- function(expr, desc = "operation", debug = FALSE, level = 0, id = NULL, on_error = "stop") {
	darrow(desc, ".. .", debug = debug, level = level, id = id)
	
	result <- tryCatch(
		{
			res <- force(expr)
			dsuccess(desc, "complete", debug = debug, level = level, id = id)
			res
		},
		error = function(e) {
			derror(desc, "FAILED:", e$message, debug = debug, level = level, id = id)
			if (debug) {
				message("Stack trace:")
				print(sys.calls())
			}
			
			# Handle error based on strategy
			if (on_error == "stop") {
				stop(e)  # Re-throw the error
			} else if (on_error == "null") {
				return(NULL)
			} else {
				return(expr)  # Return unevaluated expression (might cause issues)
			}
		}
	)
	
	return(result)
}