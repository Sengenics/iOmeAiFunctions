#' Example Expression Set Data
#'
#' An example dataset containing protein microarray expression data
#' used for demonstrating analysis workflows in iOmeAiFunctions.
#'
#' @format A list containing expression set data with the following structure:
#' \describe{
#'   \item{manifest}{Data frame with sample information}
#'   \item{data}{List containing:
#'     \itemize{
#'       \item \code{RawData}: Data frame with GPR raw data (Sample, X, Y, FG, BG, NetI, Protein, Block, Row, Column, Flags)
#'       \item \code{Data}: Data frame with protein annotations (Protein, data, flag, num_test)
#'     }
#'   }
#'   \item{param}{List containing analysis parameters:
#'     \itemize{
#'       \item \code{Wavelength}: Character, either "532" or "635"
#'     }
#'   }
#' }
#'
#' @source Internal Sengenics protein microarray data
#' @examples
#' data(ExpSet)
#' names(ExpSet)
#' head(ExpSet$manifest)
"ExpSet"