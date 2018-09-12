# Pixel analysis functions -----------------------------------------------------

#' A function to calculate euclidean distance from the centre
#'
#' @slot layout Layout object
#' @return dist Matrix containing euclidean distances from the centre for each
#'   pixel
pixel_dist_ctr_eucl <- function(layout) {

  dist <- matrix(NA, nrow = layout$detector_height, ncol = layout$detector_width)

  xx <- matrix(abs(1:layout$detector_width - layout$detector_width / 2),
               nrow = layout$detector_height, ncol = layout$detector_width,
               byrow = TRUE)

  yy <- matrix(abs(1:layout$detector_height - layout$detector_height / 2),
               nrow = layout$detector_height, ncol = layout$detector_width)

  dist <- sqrt(xx^2 + yy^2)

  return(dist)
}

#' A function to calculate parallel maxima from the centre
#'
#' @slot layout Layout object
#' @return dist Matrix containing parallel maxima from the centre for each pixel
pixel_dist_ctr_linf <- function(layout) {

  dist <- matrix(NA, nrow = layout$detector_height, ncol = layout$detector_width)

  # TODO: Should it be done only once per layout?
  xx <- matrix(abs(1:layout$detector_width - layout$detector_width / 2),
               nrow = layout$detector_height, ncol = layout$detector_width,
               byrow = TRUE)

  yy <- matrix(abs(1:layout$detector_height - layout$detector_height / 2),
               nrow = layout$detector_height, ncol = layout$detector_width)

  dist <- pmax(xx, yy, na.rm = TRUE)

  return(dist)
}

# TODO: modify the description.
#' A function to calclutate closest distance to an edge
#'
#' @slot x something
#' @slot size something else
#' @return what does this mean?
dist_closest_edge <- function(x, size) {
  # Why x-1? Because pixel locations start in 1, but we want both edges inside detector for symmetry
  return(min(x - 1, size - x))
}

# TODO: modify the description.
#' A function to calculate pixel distances from corners
#'
#' @slot layout Layout object
#' @return dist Matrix containing parallel maxima from the centre for each pixel
dist_corner <- function(layout) {

  #dist <- matrix(NA, nrow = layout$detector_height, ncol = layout$detector_width)

  xx <- matrix(sapply(c(1:layout$detector_width),
                      function(x) dist_closest_edge(x,layout$detector_width)),
               nrow=layout$detector_height,
               ncol=layout$detector_width, byrow = TRUE)

  yy <- matrix(sapply(c(1:layout$detector_height),
                      function(x) dist_closest_edge(x, layout$detector_height)),
               nrow=layout$detector_height,
               ncol=layout$detector_width)

  dist <- sqrt(xx^2 + yy^2)

  return(dist)
}

# TODO: modify the description.
#' A function to calculate pixel distances from edges by column
#'
#' @slot layout Layout object
#' @return dist ?
dist_edge_col <- function(layout) {

  dist <- matrix(NA, nrow = layout$detector_height, ncol = layout$detector_width)

  for (x in 1:layout$detector_width) {
    dist[1, x] <- dist_edge(x, layout$module_edges_col)
  }

  # TODO: Address this issue?
  # Mystery: should work but does not! dist.matrix.col[2:detector.height, ] <- dist.matrix.col[1, ]    # all rows are like first row
  # Replace by pedestrain version:
  # dist[2:layout$detector_height, ] <- dist[1, ]
  for (y in 2:layout$detector_height){
    dist[y, ] <- dist[1, ] # all rows are like first row
  }

  return(dist)
}

# TODO: modify the description.
#' A function to calculate pixel distances from edges by row
#'
#' @slot layout Layout object
#' @return dist ?
dist_edge_row <- function(layout) {

  dist <- matrix(NA, nrow = layout$detector_height, ncol = layout$detector_width)

  for (y in 1:layout$detector_height) {
    dist[y, 1] <- dist_edge(y, layout$module_edges_row)
  }

  dist[ , 2:layout$detector_width] <- dist[ , 1] # all cols are like first col

  return(dist)
}

# TODO: modify the description.
#' A function to calculate pixel distances from edges
#'
#' @slot layout Layout object
#' @return dist ?
dist_edge_min <- function(layout) {

  dist_col <- dist_edge_col(layout)
  dist_row <- dist_edge_row(layout)

  dist <- pmin(dist_col, dist_row)

  return(dist)
}


