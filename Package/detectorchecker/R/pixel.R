#' @title Pixel module

#' Function assign a module to each dead pixel
#'
#' @param layout Layout object
#' @return dead_modules
.assign_module <- function(layout) {
  dead_n <- length(as.vector(layout$pix_dead[ , 2]))

  dead_modules <- data.frame(layout$pix_dead, NA, NA)
  colnames(dead_modules) <- c("pixcol", "pixrow", "modcol", "modrow")

  # TODO: more elegant with lapply or plyr etc
  for (i in 1:dead_n) {
    tmp <- .which_module_idx(layout$pix_dead[i, 1], layout$pix_dead[i, 2],
                             layout$module_edges_col, layout$module_edges_row)

    dead_modules[i, 3] <- tmp$col
    dead_modules[i, 4] <- tmp$row
  }

  return(dead_modules)
}

# Pixel analysis functions -----------------------------------------------------

#' A function to calculate euclidean distance from the centre
#'
#' @param layout Layout object
#' @return Matrix containing euclidean distances from the centre for each
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
#' @param layout Layout object
#' @return Matrix containing parallel maxima from the centre for each pixel
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
#' @param x something
#' @param size something else
#' @return what does this mean?
dist_closest_edge <- function(x, size) {
  # Why x-1? Because pixel locations start in 1, but we want both edges inside detector for symmetry
  return(min(x - 1, size - x))
}

# TODO: modify the description.
#' A function to calculate pixel distances from corners
#'
#' @param layout Layout object
#' @return Matrix containing parallel maxima from the centre for each pixel
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
#' @param layout Layout object
#' @return dist ?
dist_edge_col <- function(layout) {

  dist <- matrix(NA, nrow = layout$detector_height, ncol = layout$detector_width)

  for (x in 1:layout$detector_width) {
    dist[1, x] <- .dist_edge(x, layout$module_edges_col)
  }

  # TODO: Address this issue?
  # Mystery: should work but does not! dist.matrix.col[2:detector.height, ] <- dist.matrix.col[1, ]    # all rows are like first row
  # Replace by pedestrain version:
  # dist[2:layout$detector_height, ] <- dist[1, ]

  # all rows are like first row
  for (y in 2:layout$detector_height){
    dist[y, ] <- dist[1, ]
  }

  return(dist)
}

# TODO: modify the description.
#' A function to calculate pixel distances from edges by row
#'
#' @param layout Layout object
#' @return dist ?
dist_edge_row <- function(layout) {

  dist <- matrix(NA, nrow = layout$detector_height, ncol = layout$detector_width)

  for (y in 1:layout$detector_height) {
    dist[y, 1] <- .dist_edge(y, layout$module_edges_row)
  }

  # all cols are like first col
  dist[ , 2:layout$detector_width] <- dist[ , 1]

  return(dist)
}

# TODO: modify the description.
#' A function to calculate pixel distances from edges
#'
#' @param layout Layout object
#' @return dist ?
dist_edge_min <- function(layout) {

  dist_col <- dist_edge_col(layout)
  dist_row <- dist_edge_row(layout)

  dist <- pmin(dist_col, dist_row)

  return(dist)
}

#' Plots pixel analysis
#'
#' @param data Matrix containing pixel analysis data
#' @param width Plot width
#' @param height Plot height
#' @param file_path Output path with an extension
plot_pixel <- function(data, width, height, file_path = NA) {

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  iw <- c(1:width)
  ih <- c(1:height)

  # TODO: iw, ih?
  # note t() used to get orientations right in plots
  image(iw, ih, t(data))

  if(!is.na(file_path)) {
    dev.off()
  }
}

#' Calculates and plots pixel euclidean distance from the centre
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_pixel_ctr_eucl <- function(layout, file_path = NA) {

  dist <- pixel_dist_ctr_eucl(layout)

  plot_pixel(dist, width = layout$detector_width, height = layout$detector_height,
             file_path = file_path)
}

#' Calculates and plots pixel parallel maxima from the centre
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_pixel_ctr_linf <- function(layout, file_path = NA) {

  dist <- pixel_dist_ctr_linf(layout)

  plot_pixel(dist, width = layout$detector_width, height = layout$detector_height,
             file_path = file_path)
}

#' Calculates and plots pixel distances from corners
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_pixel_dist_corner <- function(layout, file_path = NA) {

  dist <- dist_corner(layout)

  plot_pixel(dist, width = layout$detector_width, height = layout$detector_height,
             file_path = file_path)
}

#' Calculates and plots distances from the module edges by column
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_pixel_dist_edge_col <- function(layout, file_path = NA) {

  dist <- dist_edge_col(layout)

  plot_pixel(dist, width = layout$detector_width, height = layout$detector_height,
             file_path = file_path)
}

#' Calculates and plots distances from the module edges by row
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_pixel_dist_edge_row <- function(layout, file_path = NA) {

  dist <- dist_edge_row(layout)

  plot_pixel(dist, width = layout$detector_width, height = layout$detector_height,
             file_path = file_path)
}

#' Calculates and plots minimum distances from the module edges
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_pixel_dist_edge <- function(layout, file_path = NA) {

  dist <- dist_edge_min(layout)

  plot_pixel(dist, width = layout$detector_width, height = layout$detector_height,
             file_path = file_path)
}

#' Counts damaged pixel locations (dead_data) outside detector (layout)
#'   and in gaps between modules and give warnings
#'
#' @param dead_data Dead pixel locations
#' @param layout Layout object
#' @return Inconsistency message
inconsist_dead_layout <- function(dead_data, layout) {

  error <- ""

  outleft <- sum(dead_data[ , 1] < 1)
  outright <- sum(dead_data[ , 1] > layout$detector_width)

  outtop <- sum(dead_data[ , 2] < 1)
  outbottom <- sum(dead_data[ , 2] > layout$detector_height)

  colgaps <- c()
  if (sum(layout$gap_col_sizes) != 0) {
    for (i in 1:(layout$module_col_n - 1)) {
      colgaps <- c(colgaps, (layout$module_edges_col[2, i] + 1):(layout$module_edges_col[1, i + 1] - 1) )
    }
  }

  rowgaps <- c()
  if (sum(layout$gap_row_sizes) != 0) {
    for (i in 1:(layout$module_row_n - 1)) {
      rowgaps <- c(rowgaps, (layout$module_edges_row[2, i] + 1):(layout$module_edges_row[1, i + 1] - 1) )
    }
  }

  in_gaps_dead <- c()

  in_gaps <- function(i, coo) {
    return((coo[i, 1] %in% colgaps) | (coo[i, 2] %in% rowgaps))
  }

  in_gaps_dead <- vector(length = dim(dead_data)[1])

  for (i in 1:length(in_gaps_dead)){
    in_gaps_dead[i] <- in_gaps(i, dead_data)
  }

  if (sum(in_gaps_dead) != 0){
    cat(paste("Warning: ", sum(in_gaps_dead),
              " of the coordinates of damaged pixels correspond to locations in gaps between modules of the detector.\n", sep=""))
  }

  inconsistency <- list(outleft, outtop, outright, outbottom, sum(in_gaps_dead))
  names(inconsistency) <- c("left", "top","right","bottom","gaps")
  return(inconsistency)
}
