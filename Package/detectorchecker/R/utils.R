#' Calculates the trace value of a square matrix
#'
#' @param m A square matrix
#' @return tr The trace value
tr <- function (m) {
  tr <- NA

  # is the object matrix?
  if (is.matrix(m)) {
    row_count <- nrow(m)
    col_count <- ncol(m)

    # is it a square matrix?
    if(row_count == col_count) {
      tr <- 0.0
      tr <- sum(diag(m))
    }
  }

  return(tr)
}
