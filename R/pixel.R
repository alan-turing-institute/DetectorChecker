#' @title Pixel module

#' Function assign a module to each dead pixel
#'
#' @param detector Detector object
#' @return dead_modules
#' @keywords internal
.assign_module <- function(detector) {
  dead_n <- length(as.vector(detector$pix_dead[, 2]))

  dead_modules <- data.frame(detector$pix_dead, NA, NA)
  colnames(dead_modules) <- c("pixcol", "pixrow", "modcol", "modrow")

  # TODO: more elegant with lapply or plyr etc
  for (i in 1:dead_n) {
    tmp <- which_module_idx(
      detector$pix_dead[i, 1], detector$pix_dead[i, 2],
      detector$module_edges_col, detector$module_edges_row
    )

    dead_modules[i, 3] <- tmp$col
    dead_modules[i, 4] <- tmp$row
  }

  return(dead_modules)
}

# Pixel analysis functions -----------------------------------------------------

#' Calculate euclidean distance from the centre of a module for each pixel
#'
#' @param detector Detector object
#' @return Matrix of euclidean distances
#' @examples
#' pixel_dist_ctr_eucl(PerkinElmerFull_Detector())
#' @export
pixel_dist_ctr_eucl <- function(detector) {
  dist <- matrix(NA, nrow = detector$detector_height, ncol = detector$detector_width)

  xx <- matrix(abs(1:detector$detector_width - detector$detector_width / 2),
    nrow = detector$detector_height, ncol = detector$detector_width,
    byrow = TRUE
  )

  yy <- matrix(abs(1:detector$detector_height - detector$detector_height / 2),
    nrow = detector$detector_height, ncol = detector$detector_width
  )

  dist <- sqrt(xx^2 + yy^2)

  return(dist)
}

#' Calculate parallel maxima from the centre for each pixel
#'
#' @param detector Detector object
#' @return Matrix of parallel maxima
#' @examples
#' pixel_dist_ctr_linf(PerkinElmerFull_Detector())
#' @export
pixel_dist_ctr_linf <- function(detector) {
  dist <- matrix(NA, nrow = detector$detector_height, ncol = detector$detector_width)

  # TODO: Should it be done only once per detector?
  xx <- matrix(abs(1:detector$detector_width - detector$detector_width / 2),
    nrow = detector$detector_height, ncol = detector$detector_width,
    byrow = TRUE
  )

  yy <- matrix(abs(1:detector$detector_height - detector$detector_height / 2),
    nrow = detector$detector_height, ncol = detector$detector_width
  )

  dist <- pmax(xx, yy, na.rm = TRUE)

  return(dist)
}


#' A function to calculate closest distance to an edge for a pixel
#'
#' @param x Coordinate of pixel
#' @param size Size of module
#' @return distance to closest edge
#' @keywords internal
.dist_closest_edge <- function(x, size) {
  # Why x-1? Because pixel locations start in 1, but we want both edges inside detector for symmetry
  return(min(x - 1, size - x))
}


#' A function to calculate pixel distances from the closest corner
#'
#' @param detector Detector object
#' @return Matrix containing pixel distances from closest corner
#' @export
#' @examples
#' detc <- Excalibur_exp_1
#' dist_corner(detc)
#' @export
dist_corner <- function(detector) {

  # dist <- matrix(NA, nrow = detector$detector_height, ncol = detector$detector_width)

  xx <- matrix(sapply(
    c(1:detector$detector_width),
    function(x) .dist_closest_edge(x, detector$detector_width)
  ),
  nrow = detector$detector_height,
  ncol = detector$detector_width, byrow = TRUE
  )

  yy <- matrix(sapply(
    c(1:detector$detector_height),
    function(x) .dist_closest_edge(x, detector$detector_height)
  ),
  nrow = detector$detector_height,
  ncol = detector$detector_width
  )

  dist <- sqrt(xx^2 + yy^2)

  return(dist)
}


#' Calculate horizontal distance from each pixel to nearest module edge
#'
#' @param detector Detector object
#' @return distance matrix
#' @examples
#' detc <- Excalibur_exp_1
#' dist_edge_col(detc)
#' @export
dist_edge_col <- function(detector) {
  dist <- matrix(NA, nrow = detector$detector_height, ncol = detector$detector_width)

  for (x in 1:detector$detector_width) {
    dist[1, x] <- .dist_edge(x, detector$module_edges_col)
  }

  # TODO: Address this issue?
  # Mystery: should work but does not! dist.matrix.col[2:detector.height, ] <- dist.matrix.col[1, ]    # all rows are like first row
  # Replace by pedestrain version:
  # dist[2:detector$detector_height, ] <- dist[1, ]

  # all rows are like first row
  for (y in 2:detector$detector_height) {
    dist[y, ] <- dist[1, ]
  }

  return(dist)
}


#' Calculate vertical distance from each pixel to nearest module edge
#'
#' @param detector Detector object
#' @return distance matrix
#' @export
#' @examples
#' detc <- Excalibur_exp_1
#' dist_edge_row(detc)
dist_edge_row <- function(detector) {
  dist <- matrix(NA, nrow = detector$detector_height, ncol = detector$detector_width)

  for (y in 1:detector$detector_height) {
    dist[y, 1] <- .dist_edge(y, detector$module_edges_row)
  }

  # all cols are like first col
  dist[, 2:detector$detector_width] <- dist[, 1]

  return(dist)
}


#' A function to calculate L-infinity distance to module edge
#'
#' @param detector Detector object
#' @return distance matrix
#' @export
dist_edge_min <- function(detector) {
  dist_col <- dist_edge_col(detector)
  dist_row <- dist_edge_row(detector)

  dist <- pmin(dist_col, dist_row)

  return(dist)
}

#' Plots pixel distance analysis
#'
#' @param data Matrix containing pixel analysis data
#' @param width Plot width
#' @param height Plot height
#' @param file_path Output path with an extension
#' @keywords internal
.plot_pixel <- function(data, width, height, file_path = NA) {
  if (!is.na(file_path)) {
    # starts the graphics device driver
    .ini_graphics(file_path = file_path)
  }

  iw <- c(1:width)
  ih <- c(1:height)

  # TODO: iw, ih?
  # note t() used to get orientations right in plots
  image(iw, ih, t(data))

  if (!is.na(file_path)) {
    dev.off()
  }
}

#' Calculates and plots pixel euclidean distance from the centre
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @export
plot_pixel_ctr_eucl <- function(detector, file_path = NA) {
  dist <- pixel_dist_ctr_eucl(detector)

  .plot_pixel(dist,
    width = detector$detector_width, height = detector$detector_height,
    file_path = file_path
  )
}

#' Calculates and plots pixel parallel maxima from the centre
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @export
plot_pixel_ctr_linf <- function(detector, file_path = NA) {
  dist <- pixel_dist_ctr_linf(detector)

  .plot_pixel(dist,
    width = detector$detector_width, height = detector$detector_height,
    file_path = file_path
  )
}

#' Calculates and plots pixel distances from corners
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @export
plot_pixel_dist_corner <- function(detector, file_path = NA) {
  dist <- dist_corner(detector)

  .plot_pixel(dist,
    width = detector$detector_width, height = detector$detector_height,
    file_path = file_path
  )
}

#' Calculates and plots horizontal distances from the module edges
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @export
plot_pixel_dist_edge_col <- function(detector, file_path = NA) {
  dist <- dist_edge_col(detector)

  .plot_pixel(dist,
    width = detector$detector_width, height = detector$detector_height,
    file_path = file_path
  )
}

#' Calculates and plots vetical distances from the module edges
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @export
plot_pixel_dist_edge_row <- function(detector, file_path = NA) {
  dist <- dist_edge_row(detector)

  .plot_pixel(dist,
    width = detector$detector_width, height = detector$detector_height,
    file_path = file_path
  )
}

#' Calculates and plots L-infinity distances from the module edges
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @export
plot_pixel_dist_edge <- function(detector, file_path = NA) {
  dist <- dist_edge_min(detector)

  .plot_pixel(dist,
    width = detector$detector_width, height = detector$detector_height,
    file_path = file_path
  )
}

# #' Counts dead pixels outside of detector and in gaps between modules and give warnings
# #'
# #' @param dead_data Dead pixel locations
# #' @param detector Detector object
# #' @return Inconsistency message
# #' @export
# inconsist_dead_detector <- function(dead_data, detector) {
#   error <- ""

#   outleft <- sum(dead_data[, 1] < 1)
#   outright <- sum(dead_data[, 1] > detector$detector_width)

#   outtop <- sum(dead_data[, 2] < 1)
#   outbottom <- sum(dead_data[, 2] > detector$detector_height)

#   colgaps <- c()
#   if (sum(detector$gap_col_sizes) != 0) {
#     for (i in 1:(detector$module_col_n - 1)) {
#       colgaps <- c(colgaps, (detector$module_edges_col[2, i] + 1):(detector$module_edges_col[1, i + 1] - 1))
#     }
#   }

#   rowgaps <- c()
#   if (sum(detector$gap_row_sizes) != 0) {
#     for (i in 1:(detector$module_row_n - 1)) {
#       rowgaps <- c(rowgaps, (detector$module_edges_row[2, i] + 1):(detector$module_edges_row[1, i + 1] - 1))
#     }
#   }

#   in_gaps_dead <- c()

#   in_gaps <- function(i, coo) {
#     return((coo[i, 1] %in% colgaps) | (coo[i, 2] %in% rowgaps))
#   }

#   in_gaps_dead <- vector(length = dim(dead_data)[1])

#   for (i in 1:length(in_gaps_dead)) {
#     in_gaps_dead[i] <- in_gaps(i, dead_data)
#   }

#   if (sum(in_gaps_dead) != 0) {
#     cat(paste("Warning: ", sum(in_gaps_dead),
#       " of the coordinates of damaged pixels correspond to locations in gaps between modules of the detector.\n",
#       sep = ""
#     ))
#   }

#   inconsistency <- list(outleft, outtop, outright, outbottom, sum(in_gaps_dead))
#   names(inconsistency) <- c("left", "top", "right", "bottom", "gaps")
#   return(inconsistency)
# }

#' Creates a mask matrix of dead pixels
#'
#' Converts the pix_dead attribute of a detector (NX2 list) to a matrix of 1 and 0 (1 for dead pixel)
#'
#' @param detector Detector object
#' @return dead pixel mask
#' @export
#' @examples
#' detc <- Excalibur_exp_1
get_dead_pix_mask <- function(detector) {

  # TODO: check because we needed to swap detector_width with detector_height.
  mask <- matrix(0, nrow = detector$detector_width, ncol = detector$detector_height)

  for (i in c(1:(dim(detector$pix_dead)[1]))) {
    x <- as.numeric(detector$pix_dead[i, 1])
    y <- as.numeric(detector$pix_dead[i, 2])

    mask[x, y] <- 1
  }

  return(mask)
}


#' Remove high density cluster of dead pixels
#' Recalculates dead statistics and clumps if they were present in the Detector object
#'
#' @param detector Detector object
#' @param min_pts minimum points argument of dbscan function
#' @param eps_adjust adjust eps
#' @return detector object with high density cluster of pixels removed
#' @export
remove_high_density_cluster <- function(detector, min_pts = 30, eps_adjust = 0.05) {
  ppp <- get_ppp_dead(detector)

  # Find clusters
  A <- cbind(ppp$x, ppp$y)
  epsLayout <- min(detector$detector_width, detector$detector_height) * eps_adjust
  res <- dbscan::dbscan(A, eps = epsLayout, min_pts) # border points are included, not noise in this context
  Nclusters <- length(unique(res$cluster))

  # Remove clusters
  A0 <- A[res$cluster == 0, ]

  # Create a new pix_matrix without clusters
  pix_matrix <- matrix(0, nrow = detector$detector_width, ncol = detector$detector_height)
  pix_matrix[ cbind(A0[, 1], A0[, 2]) ] <- 1

  # Reassign matrix to detector
  mod_detector <- .assign_pixel_matrix(detector, pix_matrix)

  # Check if any detector variables need updating

  if (!is.na(mod_detector$dead_stats)) {
    mod_detector <- get_dead_stats(mod_detector)
  }

  if (!is.na(mod_detector$clumps)) {
    mod_detector <- find_clumps(mod_detector)
  }

  mod_detector$pix_matrix_modified <- TRUE

  return(mod_detector)
}
