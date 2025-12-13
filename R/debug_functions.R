#' Debug Message
#'
#' Print a message only when debug mode is enabled
#'
#' @param ...  Message components to paste together
#' @param debug Enable debug output (default FALSE)
#' @param prefix Prefix for the message (default "🔍")
#' @param level Indentation level (default 0)
#'
#' @export
dmsg <- function(..., debug = FALSE, prefix = "🔍", level = 0) {
	if (isTRUE(debug)) {
		indent <- paste(rep("  ", level), collapse = "")
		message(indent, prefix, " ", ...)
	}
}

#' Debug Success Message
#'
#' @param ... Message components
#' @param debug Enable debug output
#' @param level Indentation level
#'
#' @export
dsuccess <- function(..., debug = FALSE, level = 0) {
	dmsg(... , debug = debug, prefix = "✓", level = level)
}

#' Debug Error Message
#'
#' @param ... Message components
#' @param debug Enable debug output
#' @param level Indentation level
#'
#' @export
derror <- function(..., debug = FALSE, level = 0) {
	dmsg(..., debug = debug, prefix = "✖", level = level)
}

#' Debug Arrow Message
#'
#' @param ... Message components
#' @param debug Enable debug output
#' @param level Indentation level
#'
#' @export
darrow <- function(..., debug = FALSE, level = 0) {
	dmsg(..., debug = debug, prefix = "→", level = level)
}

#' Debug Warning Message
#'
#' @param ... Message components
#' @param debug Enable debug output
#' @param level Indentation level
#'
#' @export
dwarn <- function(..., debug = FALSE, level = 0) {
	dmsg(..., debug = debug, prefix = "⚠️", level = level)
}

#' Debug Section Start
#'
#' @param title Section title
#' @param debug Enable debug output
#'
#' @export
dstart <- function(title, debug = FALSE) {
	if (isTRUE(debug)) {
		message("\n", "═══ ", title, " ═══")
	}
}

#' Debug Section End
#'
#' @param title Section title
#' @param debug Enable debug output
#'
#' @export
dend <- function(title, debug = FALSE) {
	if (isTRUE(debug)) {
		message("═══ END: ", title, " ═══\n")
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
#' @param on_error What to return on error:  "stop" (re-throw), "null" (return NULL), or "expr" (return expr unevaluated)
#'
#' @return Result of expr, or behavior specified by on_error
#' @export
dtry <- function(expr, desc = "operation", debug = FALSE, level = 0, on_error = "stop") {
	darrow(desc, ".. .", debug = debug, level = level)
	
	result <- tryCatch(
		{
			res <- force(expr)
			dsuccess(desc, "complete", debug = debug, level = level)
			res
		},
		error = function(e) {
			derror(desc, "FAILED:", e$message, debug = debug, level = level)
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