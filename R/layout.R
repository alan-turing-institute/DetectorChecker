#' @title Detector module

#' A S3 class to represent a detector.
#'
#' @param name detector's name
#' @param date date
#' @param detector_width detector's width
#' @param detector_height detector's height
#' @param module_col_n number of columns in the grid of modules
#' @param module_row_n number of rows in the grid of modules
#' @param module_col_sizes vector with widths of the modules
#' @param module_row_sizes vector with heights of the modules
#' @param module_edges_col vector of columns that contain edges of modules
#' @param module_edges_row vector of rows that contain edges of modules
#' @param gap_col_sizes vector with widths of the gaps
#' @param gap_row_sizes vector with heights of the gaps
#' @param detector_inconsistency counts inconsistencies found in parameters entered
#' @param pix_matrix pixel matrix
#' @param pix_dead dead pixels coordinates
#' @param dead_stats dead pixel statistics
#' @param pix_dead_modules assigned module for each dead pixel
#' @param clumps clumps data (xyc_df data frame with pixels and their clump ID's, xyc_events data frame with clusters (clumps) and their clump ID's and centre coordinates)
#' @return Detector object
.Default_Detector <- function(name = "Default", date = NA,
                              detector_width = NA, detector_height = NA,
                              module_col_n = NA, module_row_n = NA,
                              module_col_sizes = NA, module_row_sizes = NA,
                              gap_col_sizes = NA, gap_row_sizes = NA,
                              module_edges_col = NA, module_edges_row = NA,
                              detector_inconsistency = NA,
                              pix_matrix = NA, pix_dead = NA,
                              dead_stats = NA, pix_dead_modules = NA,
                              clumps = NA) {
  detector <- list(
    name = name,
    date = date,

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

    detector_inconsistency = detector_inconsistency,

    pix_matrix = pix_matrix,
    pix_dead = pix_dead,
    dead_stats = dead_stats,
    pix_dead_modules = pix_dead_modules,

    # Clumps
    clumps = clumps
  )

  detector <- .derive_detector(detector)

  return(detector)
}

# Available detectors ------------------------------------------------------------

.Excalibur_name <- "Excalibur"
.PerkinElmerFull_name <- "PerkinElmerFull"
.PerkinElmerCropped1600_name <- "PerkinElmerCropped1600"
.PerkinElmerRefurbished_name <- "PerkinElmerRefurbished"
.Pilatus_name <- "Pilatus"

#' A list of available detectors
#' @export
available_detectors <- c(
  .Excalibur_name, .PerkinElmerFull_name,
  .PerkinElmerCropped1600_name, .PerkinElmerRefurbished_name,
  .Pilatus_name
)

# ------------------------------------------------------------------------------

#' A S3 class to represent the Excalibur detector.
#'
#' @return Excalibur detector object
#' @export
Excalibur_Detector <- function() {
  name <- .Excalibur_name

  detector <- .Default_Detector(
    name = name,
    detector_width = 2048,
    detector_height = 1536,
    module_col_n = 8,
    module_row_n = 6,
    module_col_sizes = rep(256, 8),
    module_row_sizes = rep(256, 6),
    gap_col_sizes = rep(0, 7),
    gap_row_sizes = rep(0, 5),
    module_edges_col = NA,
    module_edges_row = NA,
    detector_inconsistency = 0
  )

  return(detector)
}

#' A S3 class to represent the PerkinElmerFull detector.
#'
#' @return PerkinElmerFul detector object
#' @export
PerkinElmerFull_Detector <- function() {
  name <- .PerkinElmerFull_name

  detector <- .Default_Detector(
    name = name,
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
    detector_inconsistency = 0
  )

  return(detector)
}

#' A S3 class to represent the PerkinElmerCropped1600 detector.
#'
#' @return PerkinElmerCropped1600 detector object
#' @export
PerkinElmerCropped1600_Detector <- function() {
  name <- .PerkinElmerCropped1600_name

  detector <- .Default_Detector(
    name = name,
    detector_width = 2000,
    detector_height = 1600,
    module_col_n = 16,
    module_row_n = 2,
    module_col_sizes = c(104, rep(128, 14), 104),
    module_row_sizes = rep(800, 2),
    gap_col_sizes = rep(0, 15),
    gap_row_sizes = c(0),
    module_edges_col = NA,
    module_edges_row = NA,
    detector_inconsistency = 0
  )

  return(detector)
}

#' A S3 class to represent the PerkinElmerRefurbished detector.
#'
#' @return PerkinElmerRefurbished detector object
#' @export
PerkinElmerRefurbished_Detector <- function() {
  name <- .PerkinElmerRefurbished_name

  detector <- .Default_Detector(
    name = name,
    detector_width = 2000,
    detector_height = 2000,
    module_col_n = 16,
    module_row_n = 2,
    module_col_sizes = c(rep(128, 15), 80),
    module_row_sizes = c(1004, 996),
    gap_col_sizes = rep(0, 15),
    gap_row_sizes = c(0),
    module_edges_col = NA,
    module_edges_row = NA,
    detector_inconsistency = 0
  )

  return(detector)
}

#' A S3 class to represent the PerkinElmerRefurbished detector.
#'
#' @return Pilatus detector object
#' @export
Pilatus_Detector <- function() {
  name <- .Pilatus_name

  detector <- .Default_Detector(
    name = name,
    detector_width = 2527,
    detector_height = 2463,
    module_col_n = 12,
    module_row_n = 5,
    module_col_sizes = rep(195, 12),
    module_row_sizes = rep(487, 5),
    gap_col_sizes = rep(17, 11),
    gap_row_sizes = rep(7, 4),
    module_edges_col = NA,
    module_edges_row = NA,
    detector_inconsistency = 0
  )

  return(detector)
}

# Detector selection -------------------------------------------------------------

#' Checks whether specified detector is available
#'
#' @param detector_name The name of the detector
#' @return True or False
#' @export
check_detector_avail <- function(detector_name) {
  avail <- detector_name %in% available_detectors

  if (!avail) {
    cnt <- 0
    for (available_detector in available_detectors) {
      if (cnt == 0) {
        available_detectors_list <- available_detector
      } else {
        available_detectors_list <- paste(available_detectors_list,
          available_detector,
          sep = ", "
        )
      }

      cnt <- cnt + 1
    }

    stop(c(
      "Detector [", detector_name, "] is not available.\n",
      "Available detectors: ", available_detectors_list
    ))
  }

  return(avail)
}

#' Checks whether detector is available, if so, creates a Detector object
#'
#' @param detector_name The name of the detector
#' @return Detector object
#' @export
create_detector <- function(detector_name) {
  detector <- NA

  # Check if we know about detector_name
  if (check_detector_avail(detector_name)) {
    if (detector_name == .Excalibur_name) {
      detector <- Excalibur_Detector()
    } else if (detector_name == .PerkinElmerFull_name) {
      detector <- PerkinElmerFull_Detector()
    } else if (detector_name == .PerkinElmerCropped1600_name) {
      detector <- PerkinElmerCropped1600_Detector()
    } else if (detector_name == .PerkinElmerRefurbished_name) {
      detector <- PerkinElmerRefurbished_Detector()
    } else if (detector_name == .Pilatus_name) {
      detector <- Pilatus_Detector()
    } else {
      stop(c("Detector [", detector_name, "] is available but has not been properly
             implemented."))
    }
  }

  return(detector)
}

# Detector functions -------------------------------------------------------------

#' Basic checks if parameters entered (slightly redundant on purpose) add up
#'
#' @param detector Detector object
#' @export
detector_consist_check <- function(detector = NA) {
  if (is.list(detector)) {
    error <- ""

    detector_inconsistency <- 0 # counts inconsistencies found

    # Total size of detector
    check_value <- sum(detector$module_col_sizes) + sum(detector$gap_col_sizes)
    if (detector$detector_width != check_value) {

      # This is not need for the first check, but just in case more tests will
      #   be added before it
      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(
        error, "Dectector width is not the sum or the widths of the
                 modules and the gaps between them. Please check if you entered
                 the correct sizes. ", detector$detector_width, " /= ",
        check_value
      )

      detector_inconsistency <- detector_inconsistency + 1
    }

    check_value <- sum(detector$module_row_sizes) + sum(detector$gap_row_sizes)
    if (detector$detector_height != check_value) {
      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(
        error, "Dectector height is not the sum or the heights of the
                 modules and the gaps between them. Please check if you entered
                 the correct sizes. ", detector$detector_height, " /= ",
        check_value
      )

      detector_inconsistency <- detector_inconsistency + 1
    }

    # Module numbers and size vectors
    check_value <- length(detector$module_col_sizes)
    if (detector$module_col_n != check_value) {
      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(
        error, "Number of modules per row does not match the length of
                 the vector of their widths. ", detector$module_col_n, " /= ",
        check_value
      )

      detector_inconsistency <- detector_inconsistency + 1
    }

    check_value <- length(detector$module_row_sizes)
    if (detector$module_row_n != check_value) {
      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(
        error, "Number of modules per column does not match the length
                 of the vector of their heights. ", detector$module_row_n, " /= ",
        check_value
      )

      detector_inconsistency <- detector_inconsistency + 1
    }

    # If inconsistencies were detected, stop
    if (detector_inconsistency > 0) {
      detector$detector_inconsistency <- detector_inconsistency
      stop(error)
    }
  } else {
    stop("Detector detector object has not been initialized.")
  }

  return(TRUE)
}

#' Defines the coordinates of detector's edges using module and gap sizes
#' Function is in 1d context to be applied to rows and cols separately.
#' Edges are inside the modules (first/last row/col of module).
#'
#' @param m vector of module sizes
#' @param g vectors of gap sizes
#' @return Matrix with the information about the edges
.detector_edges <- function(m, g) {

  if (((length(m) > 1) && (length(m) - 1 != length(g))) &&
      ((length(m) == 1) && (length(g) != 1))) {

    # This should be picked by the consistency check too
    stop("The number of modules or gaps is incorrect.")
  } else {
    detector_edges <- matrix(nrow = 2, ncol = length(m))

    detector_edges[1, 1] <- 1
    detector_edges[2, 1] <- m[1]

    if (length(m) > 1) {
      for (i in 2:length(m)) {
        detector_edges[1, i] <- detector_edges[2, i - 1] + g[i - 1] + 1
        detector_edges[2, i] <- detector_edges[1, i] - 1 + m[i]
      }
    }
  }

  return(detector_edges)
}

#' Deriving additional detector elements
#' Conditions additional elements of Detector object that are frequently used later
#' They are calculated from parameters defined in examples
#' Matrices that contains xy coordiantes of edges of modules
#' By definition, edges are part of modules (not part of gaps)
#' i.e. for each module two pairs: first/last col and first/last row.
#'
#' @param detector Detector object
#' @return Detector object
.derive_detector <- function(detector) {

  module_edges_col <- .detector_edges(detector$module_col_sizes, detector$gap_col_sizes)
  dimnames(module_edges_col)[[1]] <- c("left", "right")

  # displayed in transposed (rows are listed in columns)
  module_edges_row <- .detector_edges(detector$module_row_sizes, detector$gap_row_sizes)
  dimnames(module_edges_row)[[1]] <- c("top", "bottom")

  detector$module_edges_col <- module_edges_col
  detector$module_edges_row <- module_edges_row

  return(detector)
}

#' This is the ppp_edges_col creation function
#'
#' @param detector Detector object
#' @return Point pattern dataset
.create_ppp_edges_col <- function(detector) {
  vedges <- as.vector(detector$module_edges_col)

  ytmp <- rep(1:detector$detector_height, length(vedges))
  xtmp <- rep(vedges, rep(detector$detector_height, length(vedges)))

  ppp_edges_col <- spatstat::ppp(
    xtmp, ytmp, c(1, detector$detector_width),
    c(1, detector$detector_height)
  )

  return(ppp_edges_col)
}


#' This is the create_ppp_edges_row creation function
#'
#' @param detector Detector object
#' @return Point pattern dataset
.create_ppp_edges_row <- function(detector) {
  vedges <- as.vector(detector$module_edges_row)

  xtmp <- rep(1:detector$detector_width, length(vedges))
  ytmp <- rep(vedges, rep(detector$detector_width, length(vedges)))

  ppp_edges_row <- spatstat::ppp(
    xtmp, ytmp, c(1, detector$detector_width),
    c(1, detector$detector_height)
  )

  return(ppp_edges_row)
}

#' Creates ppp object of horizontal gaps
#'
#' @param detector Detector object
#' @return Point pattern dataset
.create_ppp_gaps_col <- function(detector) {
  vgaps <- c()

  for (i in 1:(detector$module_col_n - 1)) {
    vgaps <- c(vgaps, ((detector$module_edges_col[2, i] + 1):
    (detector$module_edges_col[1, i + 1] - 1)))
  }

  ytmp <- rep(1:detector$detector_height, length(vgaps))
  xtmp <- rep(vgaps, rep(detector$detector_height, length(vgaps)))

  ppp_gaps_col <- spatstat::ppp(
    xtmp, ytmp, c(1, detector$detector_width),
    c(1, detector$detector_height)
  )

  return(ppp_gaps_col)
}

#' Creates ppp object of vertical gaps
#'
#' @param detector Detector object
#' @return Point pattern dataset
.create_ppp_gaps_row <- function(detector) {
  vgaps <- c()

  for (i in 1:(detector$module_row_n - 1)) {
    vgaps <- c(vgaps, ((detector$module_edges_row[2, i] + 1):
    (detector$module_edges_row[1, i + 1] - 1)))
  }

  xtmp <- rep(1:detector$detector_width, length(vgaps))
  ytmp <- rep(vgaps, rep(detector$detector_width, length(vgaps)))

  ppp_gaps_row <- spatstat::ppp(
    xtmp, ytmp, c(1, detector$detector_width),
    c(1, detector$detector_height)
  )

  return(ppp_gaps_row)
}

#' Generate detector ppps for edges and gaps
#'
#' @param detector Detector object
#' @return a list of ppps for edges and gaps
.get_detector_ppps <- function(detector) {
  ppp_edges_col <- .create_ppp_edges_col(detector)
  ppp_edges_row <- .create_ppp_edges_row(detector)

  # Does the detector have gaps?
  if (sum(detector$gap_col_sizes) + sum(detector$gap_row_sizes) == 0) {
    ppp_gaps_col <- NULL
    ppp_gaps_row <- NULL
  } else {
    ppp_gaps_col <- .create_ppp_gaps_col(detector)
    ppp_gaps_row <- .create_ppp_gaps_row(detector)
  }

  return(list(ppp_edges_col, ppp_edges_row, ppp_gaps_col, ppp_gaps_row))
}

#' A function to plot detector with damaged pixels
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @param events Flag toalter caption if plotting events
#' @importFrom graphics points
#' @export
plot_detector_damaged <- function(detector, file_path = NA, caption = TRUE, events = FALSE) {

  if (events){
    caption_type = "events"
  } else {
    caption_type = "pixels"
  }

  main_caption <- ""
  if (!caption) par(mar = c(0, 0, 0, 0))

  ppp_dead <- get_ppp_dead(detector)

  ppp_edges_col <- .create_ppp_edges_col(detector)
  ppp_edges_row <- .create_ppp_edges_row(detector)

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if (detector$pix_matrix_modified)
    caption_begining = paste(detector$name, "(modified) with damaged", caption_type)
  else
    caption_begining = paste(detector$name, "with damaged", caption_type)

  if (sum(detector$gap_col_sizes) + sum(detector$gap_row_sizes) == 0) {
    if (caption) {
      main_caption <- paste(caption_begining, "\n(black=module edges)")
    }

    # vertical lines in x-positions given by xlines
    plot(ppp_edges_col, pch = ".", cex.main = 0.7, main = main_caption)

    # horizontal lines in y-positions given by ylines
    points(ppp_edges_row, pch = ".")
  } else {

    # Define point patterns (spatstat) capturing gaps
    ppp_gaps_col <- .create_ppp_gaps_col(detector)
    ppp_gaps_row <- .create_ppp_gaps_row(detector)

    if (caption) {
      main_caption <- paste(caption_begining, "\n(black=module edges, grey=gaps)")
    }

    # vertical lines in x-positions given by xlines
    plot(ppp_edges_col, pch = ".", cex.main = 0.7, main = main_caption)

    # horizontal lines in y-positions given by ylines
    points(ppp_edges_row, pch = ".")

    # cols without pixels (gaps)
    points(ppp_gaps_col, pch = ".", col = "grey")

    # rows without pixels (gaps)
    points(ppp_gaps_row, pch = ".", col = "grey")
  }

  # Question:
  # Instead of pch=22 (empty square) would like dead pixels
  # in full but opaque squares (pch=15)like below (works!)
  # plot(ppp.dead, pch=15,  cex=0.7, transparent=TRUE)
  # Tried using par transparent and alpha also in points(), but but there is does not work
  # Changing order of plot() and points() above is not a way out,
  # because of the titles and because they detector should be printed under the
  # damaged pixels rather than cover them up.

  points(ppp_dead, pch = 22, col = "brown", cex = 0.7)

  if (!is.na(file_path)) {
    dev.off()
  }
}


#' Plot detector
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @export
plot_detector <- function(detector, file_path = NA, caption = TRUE) {
  if (!caption) par(mar = c(0, 0, 0, 0))
  main_caption <- ""


  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  edges_gaps <- .get_detector_ppps(detector)

  ppp_edges_col <- edges_gaps[[1]]
  ppp_edges_row <- edges_gaps[[2]]

  if (sum(detector$gap_col_sizes) + sum(detector$gap_row_sizes) == 0) {
    if (caption) {
      main_caption <- paste(detector$name, "detector\n (black=module edges)")
    }

    # vertical lines in x-positions given by xlines
    plot(ppp_edges_col, pch = ".", cex.main = 0.7, main = main_caption)

    # horizontal lines in y-positions given by ylines
    points(ppp_edges_row, pch = ".")
  } else {
    if (caption) {
      main_caption <- paste(detector$name, "detector\n (black=module edges, grey=gaps)")
    }

    # Define point patterns (spatstat) capturing gaps
    ppp_gaps_col <- edges_gaps[[3]]
    ppp_gaps_row <- edges_gaps[[4]]

    # vertical lines in x-positions given by xlines
    plot(ppp_edges_col, pch = ".", cex.main = 0.7, main = main_caption)

    points(ppp_edges_row, pch = ".") # horizontal lines in y-positions given by ylines

    # cols without pixels (gaps)
    points(ppp_gaps_col, pch = ".", col = "grey")

    # rows without pixels (gaps)
    points(ppp_gaps_row, pch = ".", col = "grey")
  }

  if (!is.na(file_path)) {
    # shuts down the specified (by default the current) device
    dev.off()
  }
}

#' Generates a string with the detector summary
#'
#' @param detector Detector object
#' @return String with the detector summary
#' @export
detector_summary <- function(detector) {
  summary <- paste("Detector:", "\n", "")
  summary <- paste(summary, "Name: ", detector$name, "\n", "")
  summary <- paste(summary, "Date: ", detector$date, "\n", "")
  summary <- paste(summary, "Width: ", detector$detector_width, "\n", "")
  summary <- paste(summary, "Height: ", detector$detector_height, "\n", "")
  summary <- paste(summary, "Number of columns in array of module (= number of modules per row): ", detector$module_col_n, "\n", "")
  summary <- paste(summary, "Number of rows in array of module (= number of modules per column): ", detector$module_row_n, "\n", "")
  summary <- paste(summary, "Widths of modules: ", paste(detector$module_col_sizes, collapse = " "), "\n", "")
  summary <- paste(summary, "Heights of modules: ", paste(detector$module_row_sizes, collapse = " "), "\n", "")
  summary <- paste(summary, "Widths of gaps between modules: ", paste(detector$gap_col_sizes, collapse = " "), "\n", "")
  summary <- paste(summary, "Heights of gaps between modules: ", paste(detector$gap_row_sizes, collapse = " "), "\n", "")

  return(summary)
}

#' Reads in a user defined detector from a file
#'
#' @param file_path A path to the user defined detector file
#' @return Detector object
#' @export
readin_detector <- function(file_path) {
  name <- "user-defined"

  # reads file as a string line
  file_string <- readr::read_file(file_path)

  detector_width <- .extract_detector_parameter(file_string, "detector_width")
  detector_height <- .extract_detector_parameter(file_string, "detector_height")

  if (is.na(detector_width) || is.na(detector_height)) {
    stop("Cannot determine detector's width/height. Is the file format correct?")
  }

  module_col_n <- .extract_detector_parameter(file_string, "module_col_n")
  module_row_n <- .extract_detector_parameter(file_string, "module_row_n")
  module_col_sizes <- .extract_detector_parameter(file_string, "module_col_sizes")
  module_row_sizes <- .extract_detector_parameter(file_string, "module_row_sizes")
  gap_col_sizes <- .extract_detector_parameter(file_string, "gap_col_sizes")
  gap_row_sizes <- .extract_detector_parameter(file_string, "gap_row_sizes")
  module_edges_col <- .extract_detector_parameter(file_string, "module_edges_col")
  module_edges_row <- .extract_detector_parameter(file_string, "module_edges_row")

  detector <- .Default_Detector(
    name = name,
    detector_width = detector_width,
    detector_height = detector_height,
    module_col_n = module_col_n,
    module_row_n = module_row_n,
    module_col_sizes = module_col_sizes,
    module_row_sizes = module_row_sizes,
    gap_col_sizes = gap_col_sizes,
    gap_row_sizes = gap_row_sizes,
    module_edges_col = module_edges_col,
    module_edges_row = module_edges_row,
    detector_inconsistency = 0
  )

  if (detector_consist_check(detector)) {
    return(detector)
  } else {
    return(NA)
  }
}
