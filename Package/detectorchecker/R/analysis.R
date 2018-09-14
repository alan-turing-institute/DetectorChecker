#' A function to calculate pixel distances from edges
#'
#' @slot layout The name of the layout to be used
#' @slot file Path(s) to the file(s) containing dead pixel information
load_module <- function(layout = NA, file = NA) {

  # Initial checks
  if (is.na(layout)) {
    stop("Layout has not been specified.")
  }

  # Initial checks
  if (is.na(file)) {
    stop("File(s) containing information regarding dead pixels have(s) not been
         specified.")
  }


}
