#' A S3 class to represent a detector layout.
#'
#' @slot name detector's name
#' @slot detector_width detector's width
#' @slot detector_height detector's height
#' @slot module_col_n number of columns in the grid of modules
#' @slot modulerow.n number of rows in the grid of modules
#' @slot module_col_sizes vector with widths of the modules
#' @slot module_row_sizes vector with heights of the modules
#' @slot module_edges_col
#' @slot module_edges_row
#' @slot gap_col_sizes vector with widths of the gaps
#' @slot gap_row_sizes vector with heights of the gaps
#' @slot detector_inconsistency counts inconsistencies found in parameters entered
Default_Layout <- function(name = "Default", detector_width = NA, detector_height = NA,
                           module_col_n = NA, module_row_n = NA, module_col_sizes = NA, module_row_sizes = NA,
                           gap_col_sizes = NA, gap_row_sizes = NA,module_edges_col = NA, module_edges_row = NA,
                           detector_inconsistency = NA) {

  layout <- list(
    name = name,

    detector_width = detector_width,
    detector_height = detector_height,

    module_col_n = module_col_n,
    module_row_n = module_row_n,
    module_col_sizes = module_col_sizes,
    module_row_sizes = module_row_sizes,
    module_edges_col = module_edges_col,
    module_edges_row = module_edges_row,

    gap_col_sizes = gap_col_sizes,
    gap_row_sizes = gap_row_sizes,

    detector_inconsistency = detector_inconsistency
  )

  # Set the name for the class
  class(layout) <- append(class(layout), "DefaultLayout")

  return(layout)
}

#' Layout consistency checks
#'
#' Basic checks if parameters entered (slightly redundant on purpose) add up
#' @param layout Layout object
Layout.consistancyCheck <- function(layout = NA) {
  if (is.list(layout)) {

    error <- ""

    detector_inconsistency <- 0   # counts inconsistencies found

    # Total size of detector
    check_value <- sum(layout$module_col_sizes) + sum(layout$gap_col_sizes)
    if (layout$detector_width != check_value) {

      # This is not need for the first check, but just in case more tests will be added before it
      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Dectector width is not the sum or the widths of the modules and the gaps between them.
          Please check if you entered the correct sizes. ", layout$detector_width, " /= ", check_value)

      detector_inconsistency <- detector_inconsistency + 1
    }

    check_value <- sum(layout$module_row_sizes) + sum(layout$gap_row_sizes)
    if (layout$detector_height != check_value) {

      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Dectector height is not the sum or the heights of the modules and the gaps between them.
          Please check if you entered the correct sizes. ", layout$detector_height, " /= ", check_value)

      detector_inconsistency <- detector_inconsistency + 1
    }

    # Module numbers and size vectors
    check_value <- length(layout$module_col_sizes)
    if (layout$module_col_n != check_value) {

      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Number of modules per row does not match the length of the vector of their widths. ",
                 layout$module_col_n, " /= ", check_value)

      detector_inconsistency <- detector_inconsistency + 1
    }

    check_value <- length(layout$module_row_sizes)
    if (layout$module_row_n != check_value) {

      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Number of modules per column does not match the length of the vector of their heights. ",
                 layout$module_row_n, " /= ", check_value)

      detector_inconsistency <- detector_inconsistency + 1
    }

    # If inconsistencies were detected, stop
    if (detector_inconsistency > 0) {
      layout$detector_inconsistency <- detector_inconsistency
      stop(error)
    }

  } else {
    stop("Detector layout object has not been initialized.")
  }
}

#' A S3 class to represent the Excalibur detector layout.
#'
Excalibur_Layout <- function() {
  name <- "Excalibur"

  layout <- Default_Layout(name = name,
                           detector_width = 2048,
                           detector_height = 1536,
                           module_col_n = 8,
                           module_row_n = 6,
                           module_col_sizes = rep(0, 7),
                           module_row_sizes = rep(0, 5),
                           gap_col_sizes = rep(256, 8),
                           gap_row_sizes = rep(256, 6),
                           module_edges_col = NA,
                           module_edges_row = NA,
                           detector_inconsistency = 0)

  # Set the name for the class
  class(layout) <- append(class(layout), name)

  return(layout)
}

#' A S3 class to represent the PerkinElmerFull detector layout.
#'
PerkinElmerFull_Layout <- function() {

  name <- "PerkinElmerFull"

  layout <- Default_Layout(name = name,
                           detector_width = 2000,
                           detector_height = 2000,
                           module_col_n = 16,
                           module_row_n = 2,
                           module_col_sizes = c(104, rep(128, 14), 104),
                           module_row_sizes = rep(1000, 2),
                           gap_col_sizes = rep(0, 15),
                           gap_row_sizes = c(0),
                           module_edges_col = NA,
                           module_edges_row = NA,
                           detector_inconsistency = 0)

  # Set the name for the class
  class(layout) <- append(class(layout), name)

  return(layout)
}
