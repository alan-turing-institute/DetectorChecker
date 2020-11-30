#' @title Module module


#' Returns row or column of a module that a dead pixel belongs to
#'
#' @param coo x or y coordinate of a dead pixel
#' @param me module edges
#' @return row or column number
#' @export
which_module <- function(coo, me) {
  which_module <- NA

  for (i in 1:dim(me)[2]) {
    if ((coo >= me[1, i]) & (coo <= me[2, i])) {
      which_module <- i
    }
  }

  return(which_module)
}


#' Function returns both col and row number of a dead pixel.
#'
#' @param x pixel x coordinate
#' @param y pixel y coordinate
#' @param module_edges_col vector of columns that contain edges of modules
#' @param module_edges_row vector of rows that contain edges of modules
#' @return tmp
#' @export
which_module_idx <- function(x, y, module_edges_col, module_edges_row) {
  tmp <- list(
    col = which_module(x, module_edges_col),
    row = which_module(y, module_edges_row)
  )

  return(tmp)
}


#' Function returns distance of a pixel to module edges.
#'
#' @param xy Coordinate of pixel
#' @param module_edges vector of edges of a module
#' @return tmp Distance to edges
#' @keywords internal
.dist_edge <- function(xy, module_edges) {
  i <- which_module(xy, module_edges)

  tmp <- min(xy - module_edges[1, i], module_edges[2, i] - xy)

  return(tmp)
}

#' Checks if the selected row and column are within the boundaries of the detector
#'
#' @param detector Detector object
#' @param row module row
#' @param col module col
#' @return Boolean
#' @keywords internal
.check_select <- function(detector, row, col) {
  ok <- TRUE

  if ((row < 1) || (col < 1) || (row > detector$module_row_n) || (col > detector$module_col_n)) {
    ok <- FALSE
  }

  if (!ok) {
    stop("Incorrectly chosen row and/or column indices.\n",
         "'row' must be >= 1 and <= ", detector$module_row_n, ", chosen 'row' value = ", row , "; ",
         "'col' must be >= 1 and <= ", detector$module_col_n, ", chosen 'col' value = ", col , ".")
  }
}
