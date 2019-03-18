#' @title Module module

# TODO: improve the definition of the function
#' Which module function
#'
#' @param coo 
#' @param me ?
#' @return which_module what does this mean?
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

# TODO: improve the definition of the function
#' Function returns both col and row wrt detector grid.
#' Given xy coo of pixel and matrices with positions of edges in both directions.
#'
#' @param x 
#' @param y ?
#' @param module_edges_col ?
#' @param module_edges_row ?
#' @return tmp ?
#' @export
which_module_idx <- function(x, y, module_edges_col, module_edges_row) {
  tmp <- list(
    col = which_module(x, module_edges_col),
    row = which_module(y, module_edges_row)
  )

  return(tmp)
}

# TODO: improve the definition of the function
#' Function returns distance to both edges.
#' Given xy coo of pixel and matrices with positions of edges in both directions.
#'
#' @param xy Coordinate of pixel
#' @param module_edges vector of edges of a module
#' @return tmp Distance to edges
.dist_edge <- function(xy, module_edges) {
  i <- which_module(xy, module_edges)

  tmp <- min(xy - module_edges[1, i], module_edges[2, i] - xy)

  return(tmp)
}

# Checks if the selected row and column are within the boundaries of the detector
.check_select <- function(detector, row, col) {
  ok <- TRUE

  if ((row < 1) || (col < 1) || (row > detector$module_row_n) || (col > detector$module_col_n)) {
    ok <- FALSE
  }

  if (!ok) {
    stop("Incorrectly chosen row and/or column indices.", "row: ", row, ". Must be > 0 and < ", detector$module_row_n)
  }
}
