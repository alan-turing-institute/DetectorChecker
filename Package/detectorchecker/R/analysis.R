#' A function to calculate pixel distances from edges
#'
#' @slot layout The name of the layout to be used
#' @slot file Path(s) to the file(s) containing dead pixel information
load_module <- function(layout_name = NA, file = NA) {

  # Initial checks
  if (is.na(layout_name)) {
    stop("Layout has not been specified.")
  }

  if (is.na(file)) {
    stop("File(s) regarding dead pixels have(s) not been specified.")
  }

  # create the named layout
  layout <- create_module(layout_name)

  # check whether we accept the file(s)
  file_cnt <- length(file)

  if (file_cnt == 1) {
    file_extansion <- file_ext(file)

    if (file_extansion == "tif") {
      dead_data <- read_tiff(file = file, layout = layout)

    } else if (file_extansion == "xml") {
      dead_data <- read_xml(file = file, layout = layout)

    } else {
      stop(c("Undefined file extension: ", file_extansion, " [", file, "]"))
    }

  } else {
    # loading

    dead_data <- read_hdf(file_list = file)
  }


}
