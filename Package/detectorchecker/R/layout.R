#' @title Layout module

#' A S3 class to represent a detector layout.
#'
#' @param name detector's name
#' @param date date
#' @param detector_width detector's width
#' @param detector_height detector's height
#' @param module_col_n number of columns in the grid of modules
#' @param module_row_n number of rows in the grid of modules
#' @param module_col_sizes vector with widths of the modules
#' @param module_row_sizes vector with heights of the modules
#' @param module_edges_col !
#' @param module_edges_row !
#' @param gap_col_sizes vector with widths of the gaps
#' @param gap_row_sizes vector with heights of the gaps
#' @param detector_inconsistency counts inconsistencies found in parameters entered
#' @param pix_matrix pixel matrix
#' @param pix_dead dead pixels coordinates
#' @param dead_stats dead pixel statistics
#' @param pix_dead_modules assigned module for each dead pixel
#' @return Layout object
#' @export
Default_Layout <- function(name = "Default", date = NA,
                           detector_width = NA, detector_height = NA,
                           module_col_n = NA, module_row_n = NA,
                           module_col_sizes = NA, module_row_sizes = NA,
                           gap_col_sizes = NA, gap_row_sizes = NA,
                           module_edges_col = NA, module_edges_row = NA,
                           detector_inconsistency = NA,
                           pix_matrix = NA, pix_dead = NA,
                           dead_stats = NA, pix_dead_modules = NA) {

  layout <- list(
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
    pix_dead_modules = pix_dead_modules
  )

  layout <- derive_layout(layout)

  return(layout)
}

# Available layouts ------------------------------------------------------------

.Excalibur_name <- "Excalibur"
.PerkinElmerFull_name <- "PerkinElmerFull"
.PerkinElmerCropped1600_name <- "PerkinElmerCropped1600"
.PerkinElmerRefurbished_name <- "PerkinElmerRefurbished"
.Pilatus_name <- "Pilatus"

#' @export
available_layouts <- c(.Excalibur_name, .PerkinElmerFull_name,
                       .PerkinElmerCropped1600_name, .PerkinElmerRefurbished_name,
                       .Pilatus_name)

# ------------------------------------------------------------------------------

#' A S3 class to represent the Excalibur detector layout.
#'
#' @return Excalibur layout object
#' @export
Excalibur_Layout <- function() {
  name <- .Excalibur_name

  layout <- Default_Layout(name = name,
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
                           detector_inconsistency = 0)

  return(layout)
}

#' A S3 class to represent the PerkinElmerFull detector layout.
#'
#' @return PerkinElmerFul layout object
#' @export
PerkinElmerFull_Layout <- function() {

  name <- .PerkinElmerFull_name

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

  return(layout)
}

#' A S3 class to represent the PerkinElmerCropped1600 detector layout.
#'
#' @return PerkinElmerCropped1600 layout object
#' @export
PerkinElmerCropped1600_Layout <- function() {

  name <- .PerkinElmerCropped1600_name

  layout <- Default_Layout(name = name,
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
                           detector_inconsistency = 0)

  return(layout)
}

#' A S3 class to represent the PerkinElmerRefurbished detector layout.
#'
#' @return PerkinElmerRefurbished layout object
#' @export
PerkinElmerRefurbished_Layout <- function() {

  name <- .PerkinElmerRefurbished_name

  layout <- Default_Layout(name = name,
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
                           detector_inconsistency = 0)

  return(layout)
}

#' A S3 class to represent the PerkinElmerRefurbished detector layout.
#'
#' @return Pilatus layout object
#' @export
Pilatus_Layout <- function() {

  name <- .Pilatus_name

  layout <- Default_Layout(name = name,
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
                           detector_inconsistency = 0)

  return(layout)
}

# Layout selection -------------------------------------------------------------

#' Checks whether specified layout is available
#'
#' @param layout_name The name of the layout
#' @return True or False
#' @export
check_layout_avail <- function(layout_name) {

  avail <- layout_name %in% available_layouts

  if (!avail) {

    cnt <- 0
    for (available_layout in available_layouts) {

      if (cnt == 0) {
        available_layouts_list <- available_layout
      } else {
        available_layouts_list <- paste(available_layouts_list,
                                        available_layout, sep=", ")
      }

      cnt <- cnt + 1
    }

    stop(c("Layout [", layout_name, "] is not available.\n",
           "Available layouts: ", available_layouts_list))
  }

  return(avail)
}

#' Checks whether layout is available, if so, creates a Layout object
#'
#' @param layout_name The name of the layout
#' @return Layout object
#' @export
create_module <- function(layout_name) {

  layout <- NA

  # Check if we know about layout_name
  if (check_layout_avail(layout_name)) {

    if (layout_name == .Excalibur_name) {
      layout <- Excalibur_Layout()

    } else if (layout_name == .PerkinElmerFull_name) {
      layout <- PerkinElmerFull_Layout()

    } else if (layout_name == .PerkinElmerCropped1600_name) {
      layout <- PerkinElmerCropped1600_Layout()

    } else if (layout_name == .PerkinElmerRefurbished_name) {
      layout <- PerkinElmerRefurbished_Layout()

    } else if (layout_name == .Pilatus_name) {
      layout <- Pilatus_Layout()

    } else {
      stop(c("Layout [", layout_name, "] is available but has not been properly
             implemented."))
    }
  }

  return(layout)
}

# Layout functions -------------------------------------------------------------

#' Basic checks if parameters entered (slightly redundant on purpose) add up
#'
#' @param layout Layout object
#' @export
layout_consist_check <- function(layout = NA) {
  if (is.list(layout)) {

    error <- ""

    detector_inconsistency <- 0   # counts inconsistencies found

    # Total size of detector
    check_value <- sum(layout$module_col_sizes) + sum(layout$gap_col_sizes)
    if (layout$detector_width != check_value) {

      # This is not need for the first check, but just in case more tests will
      #   be added before it
      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Dectector width is not the sum or the widths of the
                 modules and the gaps between them. Please check if you entered
                 the correct sizes. ", layout$detector_width, " /= ",
                 check_value)

      detector_inconsistency <- detector_inconsistency + 1
    }

    check_value <- sum(layout$module_row_sizes) + sum(layout$gap_row_sizes)
    if (layout$detector_height != check_value) {

      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Dectector height is not the sum or the heights of the
                 modules and the gaps between them. Please check if you entered
                 the correct sizes. ", layout$detector_height, " /= ",
                 check_value)

      detector_inconsistency <- detector_inconsistency + 1
    }

    # Module numbers and size vectors
    check_value <- length(layout$module_col_sizes)
    if (layout$module_col_n != check_value) {

      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Number of modules per row does not match the length of
                 the vector of their widths. ", layout$module_col_n, " /= ",
                 check_value)

      detector_inconsistency <- detector_inconsistency + 1
    }

    check_value <- length(layout$module_row_sizes)
    if (layout$module_row_n != check_value) {

      if (detector_inconsistency != 0) error <- c(error, "\n")

      error <- c(error, "Number of modules per column does not match the length
                 of the vector of their heights. ", layout$module_row_n, " /= ",
                 check_value)

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

#TODO: improve the definition of the function
#' Defines the coordinates of layout's edges using module and gap sizes
#'
#' Function is in 1d context to be applied to rows and cols separately.
#' Edges are inside the modules (first/last row/col of module).
#' @param m vector of module sizes
#' @param g vectors of gap sizes
#' @return Matrix with the information about the edges
#' @export
layout_edges <- function(m, g) {

  if (length(m) - 1 != length(g)) {
    # This should be picked by the consistency check too
    stop("The number of modules or gaps is incorrect.")

  } else {
    layout_edges <- matrix(nrow = 2, ncol = length(m))

    layout_edges[1, 1] <- 1
    layout_edges[2, 1] <- m[1]

    for (i in 2:length(m)) {
      layout_edges[1, i] <- layout_edges[2, i-1] + g[i-1] + 1
      layout_edges[2, i] <- layout_edges[1, i] - 1 + m[i]
    }
  }

  return(layout_edges)
}

#TODO: improve the definition of the function
#' Deriving additional layout elements
#'
#' Conditions additional elements of Layout object that are frequently used later
#' They are calculated from parameters defined in examples
#' Matrices that contains xy coordiantes of edges of modules
#' By definition, edges are part of modules (not part of gaps)
#' i.e. for each module two pairs: first/last col and first/last row.
#' @param layout Layout object
#' @return Layout object
#' @export
derive_layout <- function(layout){

  module_edges_col <- layout_edges(layout$module_col_sizes, layout$gap_col_sizes)
  dimnames(module_edges_col)[[1]] <- c("left", "right")

  # displayed in transposed (rows are listed in columns)
  module_edges_row <- layout_edges(layout$module_row_sizes, layout$gap_row_sizes)
  dimnames(module_edges_row)[[1]] <- c("top", "bottom")

  layout$module_edges_col <- module_edges_col
  layout$module_edges_row <- module_edges_row

  return(layout)
}

#' TODO: Better description of the function
#' This is the ppp_edges_col function
#'
#' @param layout Layout object
#' @return Point pattern dataset
#' @export
create_ppp_edges_col <- function(layout) {

  vedges <- as.vector(layout$module_edges_col)

  ytmp <- rep(1:layout$detector_height, length(vedges))
  xtmp <- rep(vedges, rep(layout$detector_height, length(vedges)))

  ppp_edges_col <- spatstat::ppp(xtmp, ytmp, c(1, layout$detector_width),
                                 c(1, layout$detector_height))

  return(ppp_edges_col)
}

#' TODO: Better description of the function
#' This is the create_ppp_edges_row function
#'
#' @param layout Layout object
#' @return Point pattern dataset
#' @export
create_ppp_edges_row <- function(layout) {

  vedges <- as.vector(layout$module_edges_row)

  xtmp <- rep(1:layout$detector_width, length(vedges))
  ytmp <- rep(vedges, rep(layout$detector_width, length(vedges)))

  ppp_edges_row <- spatstat::ppp(xtmp, ytmp, c(1, layout$detector_width),
                                 c(1, layout$detector_height))

  return(ppp_edges_row)
}

#' TODO: Better description of the function
#' This is a function
#'
#' @param layout Layout object
#' @return Point pattern dataset
#' @export
create_ppp_gaps_col <- function(layout) {
  vgaps <- c()

  for (i in 1:(layout$module_col_n-1)) {
    vgaps <- c(vgaps, ((layout$module_edges_col[2, i] + 1):
                         (layout$module_edges_col[1, i+1] - 1)))
  }

  ytmp <- rep(1:layout$detector_height, length(vgaps))
  xtmp <- rep(vgaps, rep(layout$detector_height, length(vgaps)))

  ppp_gaps_col <- spatstat::ppp(xtmp, ytmp, c(1, layout$detector_width),
                                c(1, layout$detector_height))

  return(ppp_gaps_col)
}

#' TODO: Better description of the function
#' This is a function
#'
#' @param layout Layout object
#' @return Point pattern dataset
#' @export
create_ppp_gaps_row <- function(layout) {
  vgaps <- c()

  for (i in 1:(layout$module_row_n - 1)) {
    vgaps <- c(vgaps, ((layout$module_edges_row[2, i] + 1):
                         (layout$module_edges_row[1, i+1] - 1)))
  }

  xtmp <- rep(1:layout$detector_width, length(vgaps))
  ytmp <- rep(vgaps, rep(layout$detector_width, length(vgaps)))

  ppp_gaps_row <- spatstat::ppp(xtmp, ytmp, c(1, layout$detector_width),
                                c(1, layout$detector_height))

  return(ppp_gaps_row)
}

#' Plotting layout
#'
#' @param layout Layout object
#' @param file_path Output file path
#' @export
plot_layout <- function(layout, file_path = NA) {

  ppp_edges_col <- create_ppp_edges_col(layout)
  ppp_edges_row <- create_ppp_edges_row(layout)

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if (sum(layout$gap_col_sizes) + sum(layout$gap_row_sizes) == 0) {

    # vertical lines in x-positions given by xlines
    plot(ppp_edges_col, pch = ".", cex.main = 0.7,
         main = paste(layout$name, "layout\n (black=module edges)"), res = 10)

    # horizontal lines in y-positions given by ylines
    points(ppp_edges_row, pch = ".")

  } else {
    # Define point patterns (spatstat) capturing gaps
    ppp_gaps_col <- create_ppp_gaps_col(layout)
    ppp_gaps_row <- create_ppp_gaps_row(layout)

    # vertical lines in x-positions given by xlines
    plot(ppp_edges_col, pch = ".", cex.main = 0.7,
         main = paste(layout$name, "layout\n (black=module edges, grey=gaps)"))

    points(ppp_edges_row, pch = ".") # horizontal lines in y-positions given by ylines

    # cols without pixels (gaps)
    points(ppp_gaps_col, pch = ".", col = "grey")

    # rows without pixels (gaps)
    points(ppp_gaps_row, pch = ".", col = "grey")
  }

  if(!is.na(file_path)) {
    # shuts down the specified (by default the current) device
    dev.off()
  }
}

#' Returns a string with the layout summary
#'
#' @param layout Layout object
#' @return String with the layout summary
#' @export
layout_summary <- function(layout) {

  summary <- paste("Detector:", "\n", "")
  summary <- paste(summary, "Name: ", layout$name, "\n", "")
  summary <- paste(summary, "Date: ", layout$date, "\n", "")
  summary <- paste(summary, "Width: ", layout$detector_width, "\n", "")
  summary <- paste(summary, "Height: ", layout$detector_height, "\n", "")
  summary <- paste(summary, "Number of columns in array of module (= number of modules per row): ", layout$module_col_n, "\n", "")
  summary <- paste(summary, "Number of rows in array of module (= number of modules per column): ", layout$module_row_n, "\n", "")
  summary <- paste(summary, "Widths of modules: ", paste(layout$module_col_sizes, collapse = ' '), "\n", "")
  summary <- paste(summary, "Heights of modules: ", paste(layout$module_row_sizes, collapse = ' '), "\n", "")
  summary <- paste(summary, "Widths of gaps between modules: ", paste(layout$gap_col_sizes, collapse = ' '), "\n", "")
  summary <- paste(summary, "Heights of gaps between modules: ", paste(layout$gap_row_sizes, collapse = ' '), "\n", "")

  return(summary)
}
