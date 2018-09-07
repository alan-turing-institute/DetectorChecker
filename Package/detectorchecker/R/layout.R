#' A S3 class to represent a detector layout.
#'
#' @slot name detector's name
#' @slot detector.width detector's width
#' @slot detector.height detector's height
#' @slot module.col.n number of columns in the grid of modules
#' @slot module.row.n number of rows in the grid of modules
#' @slot module.col.sizes vector with widths of the modules
#' @slot module.row.sizes vector with heights of the modules
#' @slot module.edges.col
#' @slot module.edges.row
#' @slot gap.col.sizes vector with widths of the gaps
#' @slot gap.row.sizes vector with heights of the gaps
#' @slot detector.inconsistency counts inconsistencies found in parameters entered
Layout.Default <- function(name = "Default", detector.width = NA, detector.height = NA,
                           module.col.n = NA, module.row.n = NA, module.col.sizes = NA, module.row.sizes = NA,
                           gap.col.sizes = NA, gap.row.sizes = NA,module.edges.col = NA, module.edges.row = NA,
                           detector.inconsistency = NA) {

  layout <- list(
    name = name,

    detector.width = detector.width,
    detector.height = detector.height,

    module.col.n = module.col.n,
    module.row.n = module.row.n,
    module.col.sizes = module.col.sizes,
    module.row.sizes = module.row.sizes,
    module.edges.col = module.edges.col,
    module.edges.row = module.edges.row,

    gap.col.sizes = gap.col.sizes,
    gap.row.sizes = gap.row.sizes,

    detector.inconsistency = detector.inconsistency
  )

  # Set the name for the class
  class(layout) <- append(class(layout), "DefaultLayout")

  return(layout)
}

#' Layout consistency checks
#'
#' Basic checks if parameters entered (slightly redundant on purpose) add up
#' @param layout Layout object
Layout.consistancyCheck <- function(layout=NA) {
  if (is.list(layout)) {

    error <- ""

    detector.inconsistency <- 0   # counts inconsistencies found

    # Total size of detector
    layout.sum <- sum(layout$module.col.sizes) + sum(layout$gap.col.sizes)
    if (layout$detector.width != layout.sum) {

      # This is not need for the first check, but just in case more tests will be added before it
      if (detector.inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Dectector width is not the sum or the widths of the modules and the gaps between them.
          Please check if you entered the correct sizes. ", layout$detector.width, " /= ", layout.sum)

      detector.inconsistency <- detector.inconsistency + 1
    }

    layout.sum <- sum(layout$module.row.sizes) + sum(layout$gap.row.sizes)
    if (layout$detector.height != layout.sum) {

      if (detector.inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Dectector height is not the sum or the heights of the modules and the gaps between them.
          Please check if you entered the correct sizes. ", layout$detector.height, " /= ", layout.sum)

      detector.inconsistency <- detector.inconsistency + 1
    }

    # Module numbers and size vectors
    layout.sum <- length(layout$module.col.sizes)
    if (layout$module.col.n != layout.sum) {

      if (detector.inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Number of modules per row does not match the length of the vector of their widths. ",
                 layout$module.col.n, " /= ", layout.sum)

      detector.inconsistency <- detector.inconsistency + 1
    }

    layout.sum <- length(layout$module.row.sizes)
    if (layout$module.row.n != layout.sum) {

      if (detector.inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Number of modules per column does not match the length of the vector of their heights. ",
                 layout$module.row.n, " /= ", layout.sum)

      detector.inconsistency <- detector.inconsistency + 1
    }

    # If inconsistencies were detected, stop
    if (detector.inconsistency > 0) {
      layout$detector.inconsistency <- detector.inconsistency
      stop(error)
    }

  } else {
    stop("Detector layout object has not been initialized.")
  }
}

#' A S3 class to represent the Excalibur detector layout.
#'
Layout.Excalibur <- function() {
  layout.name <- "Excalibur"

  layout <- Layout.Default(name = layout.name,
                           detector.width = 2048,
                           detector.height = 1536,
                           module.col.n = 8,
                           module.row.n = 6,
                           module.col.sizes = rep(0, 7),
                           module.row.sizes = rep(0, 5),
                           gap.col.sizes = rep(256, 8),
                           gap.row.sizes = rep(256, 6),
                           module.edges.col = NA,
                           module.edges.row = NA,
                           detector.inconsistency = 0)

  # Set the name for the class
  class(layout) <- append(class(layout), layout.name)

  return(layout)
}

#' A S3 class to represent the PerkinElmerFull detector layout.
#'
Layout.PerkinElmerFull <- function() {

  layout.name <- "PerkinElmerFull"

  layout <- Layout.Default(name = layout.name,
                           detector.width = 2000,
                           detector.height = 2000,
                           module.col.n = 16,
                           module.row.n = 2,
                           module.col.sizes = c(104, rep(128, 14), 104),
                           module.row.sizes = rep(1000, 2),
                           gap.col.sizes = rep(0, 15),
                           gap.row.sizes = c(0),
                           module.edges.col = NA,
                           module.edges.row = NA,
                           detector.inconsistency = 0)

  # Set the name for the class
  class(layout) <- append(class(layout), layout.name)

  return(layout)
}
