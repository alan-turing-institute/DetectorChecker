
#' #TODO: better param description
#' #' A S3 class to represent pixel specific layout information
#' #'
#' #'
#' #' @slot matrix_col describe what it is
#' #' @slot matrix_row describe what it is
#' #' @slot dist_matrix_centre_eucl describe what it is
#' #' @slot dist_matrix_centre_linf describe what it is
#' #' @slot dist_matrix_corner describe what it is
#' #' @slot dist_matrix_col describe what it is
#' #' @slot dist_matrix_row describe what it is
#' #' @slot dist_matrix_minb describe what it is
#' #' @return pixel_layout Pixel object
#' Pixel_Layout <- function(matrix_col = NA, matrix_row = NA,
#'                          dist_matrix_centre_eucl = NA,
#'                          dist_matrix_centre_linf = NA,
#'                          dist_matrix_corner = NA, dist_matrix_col = NA,
#'                          dist_matrix_row = NA, dist_matrix_min = NA) {
#'
#'   pixel_layout <- list(
#'     matrix_col =  matrix_col,
#'     matrix_row = matrix_row,
#'     dist_matrix_centre_eucl = dist_matrix_centre_eucl,
#'     dist_matrix_centre_linf = dist_matrix_centre_linf,
#'     dist_matrix_corner = dist_matrix_corner,
#'     dist_matrix_col = dist_matrix_col,
#'     dist_matrix_row = dist_matrix_row,
#'     dist_matrix_min = dist_matrix_minb
#'   )
#'
#'   return(pixel_layout)
#' }

# TODO: check whether all the pixels are within the detector

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
#'
#'
#' @slot x something
#' @slot size something else
#' @return
dist_closest_edge <- function(x, size) {
  # Why x-1? Because pixel locations start in 1, but we want both edges inside detector for symmetry
  return(min(x - 1, size - x))
}

#' A function to calculate pixel distances from corners
#'
#' @slot layout Layout object
#' @return dist Matrix containing parallel maxima from the centre for each pixel
pixel_dist_corner <- function(layout) {

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

