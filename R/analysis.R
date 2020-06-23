#' @title Analysis module

#' A S3 class to represent dead pixels statistics summary
#'
#' @param dead_n Total number of damaged pixels:
#' @param module_n Total number of modules
#' @param module_count_arr Count of dead pixels in each quadrat
#' @param module_count Count of dead pixels in each quadrat
#' @param avg_dead_mod Average number of damaged pixels per module
#' @param Chisq_s The Chi-Squared test statistic value
#' @param Chisq_df Chi-Squared degrees of freedom
#' @param Chisq_p Chi-Squared p-value
#' @return Dead_Stats object
#' @export
Dead_Stats <- function(dead_n = NA, module_n = NA, module_count_arr = NA,
                       module_count = NA, avg_dead_mod = NA,
                       Chisq_s = NA, Chisq_df = NA, Chisq_p = NA) {
  dead_stats <- list(
    dead_n = dead_n,
    module_n = module_n,
    module_count_arr = module_count_arr,
    module_count = module_count,
    avg_dead_mod = avg_dead_mod,
    Chisq_s = Chisq_s,
    Chisq_df = Chisq_df,
    Chisq_p = Chisq_p
  )

  return(dead_stats)
}


#' A function to plot detector module with damaged pixels
#'
#' @param detector Detector object
#' @param col Module column number
#' @param row Module row number
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
plot_module_pixels <- function(detector, col, row, file_path = NA,
                                         caption = TRUE) {
  if (!caption) par(mar = c(0, 0, 0, 0))
  else par(mar=c(1, 1, 3, 1))

  # check whether the row and col numbers are correct
  .check_select(detector, row, col)

  if (!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  shift_left <- detector$module_edges_col[1, col] - 1
  shift_up <- detector$module_edges_row[1, row] - 1

  width <- detector$module_col_sizes[col]
  height <- detector$module_row_sizes[row]

  ppp_frame <- spatstat::ppp(1, 1, c(1, width), c(1, height))

  if (caption) {

    if (detector$pix_matrix_modified)
      caption_begining = paste(detector$name, "(modified) with damaged pixels")
    else
      caption_begining = paste(detector$name, "with damaged pixels")

    main_caption <- paste(caption_begining, "\n(black=module edges, red=damaged pixels)")

  } else {
    main_caption <- ""
  }

  plot(ppp_frame, pch = ".", cex.main = 0.7, main = main_caption)

  # selecting dead pixels for the particular module
  module_sel <- detector$pix_dead_modules[detector$pix_dead_modules[, 3] == col &
    detector$pix_dead_modules[, 4] == row, ]
  module_sel[, 1] <- module_sel[, 1] - shift_left
  module_sel[, 2] <- module_sel[, 2] - shift_up

  ppp_dead <- spatstat::ppp(
    module_sel[, 1], module_sel[, 2],
    c(1, width), c(1, height)
  )

  points(ppp_dead, pch = 22, col = "brown", cex = 0.7)

  if (!is.na(file_path)) {
    # shuts down the specified (by default the current) device
    dev.off()
  }
}



#' A function to plot detector with dead pixel counts per module
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @export
plot_pixels_count <- function(detector, file_path = NA, row = NA, col = NA,
                                  caption = TRUE) {
  main_caption <- ""

  if (all(is.na(detector$dead_stats))) {
    stop("Is the damaged statistics calculated?")
  }

  if (!is.na(row) && !is.na(col)) {
    if (!caption) par(mar = c(0, 0, 0, 0))
    else par(mar=c(1, 1, 3, 1))

    if (!is.na(file_path)) {
      # starts the graphics device driver
      ini_graphics(file_path = file_path)
    }

    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    if (caption) {
      main_caption <- paste(
        "Number of damaged pixels: ",
        detector$dead_stats$module_count_arr[col][row]
      )
    }

    width <- detector$module_col_sizes[col]
    height <- detector$module_row_sizes[row]

    ppp_frame <- spatstat::ppp(1, 1, c(1, width), c(1, height))

    plot(ppp_frame, pch = ".", cex.main = 0.7, main = main_caption)

    # This works only on rectangular detectors!!!
    module_idx <- (col - 1) * detector$module_row_n + row

    text(width / 2, height / 2, label = detector$dead_stats$module_count[module_idx])


    if (!is.na(file_path)) {
      dev.off()
    }
  } else {
    if (caption) {
      main_caption <- paste(
        "Number of damaged pixels in modules\n",
        "Total number damaged pixels: ", detector$dead_stats$dead_n,
        "\n (average per module: ", detector$dead_stats$avg_dead_mod, ")"
      )
    }

    plot_counts(detector$dead_stats$module_count_arr, caption = main_caption, file_path = file_path)
  }
}

#' A function to plot densities of dead pixels of detector or module
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param adjust Kernel density bandwidth
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @param color a list of colors
#' @export
plot_pixels_density <- function(detector, file_path = NA, adjust = 0.5,
                                  row = NA, col = NA, caption = TRUE, color = topo.colors(50)) {
  ppp_dead <- NA
  main_caption <- ""

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    # get the ppp for the selected module
    ppp_dead <- .get_ppp_dead_module(detector, row, col)

    if (caption) {
      main_caption <- paste("Dead pixel density (row =", row, "col =", col, "), adjust=", adjust)
    }
  } else {
    ppp_dead <- get_ppp_dead(detector)

    if (caption) {
      main_caption <- paste("Dead pixel density, adjust = ", adjust)
    }
  }

  plot_density(ppp_dead, main_caption, file_path = file_path, adjust = adjust, color = color)
}

#' A function to plot NN oriented arrrows of dead pixels of detector or module
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @importFrom graphics arrows plot
#' @export
plot_pixels_arrows <- function(detector, file_path = NA, row = NA, col = NA,
                                 caption = TRUE) {
  ppp_dead <- NA
  main_caption <- ""

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    # get the ppp for the selected module
    ppp_dead <- .get_ppp_dead_module(detector, row, col)

    if (caption) {
      main_caption <- paste("NN oriented arrows (row =", row, "col =", col, ")")
    }
  } else {
    ppp_dead <- get_ppp_dead(detector)

    if (caption) {
      main_caption <- "NN oriented arrows"
    }
  }

  plot_arrows(ppp_dead, main_caption, file_path = file_path)
}

#' Extracts a table of dead pixel coordinates from a pixel matrix
#'
#' @param pix_matrix pixel matrix with dead pixels flagged with 1
#' @return Table containing dead pixel coordinates
#' @export
dead_pix_coords <- function(pix_matrix) {

  # Matrix of damaged pixels coordinates
  # The first col of dead (dead[ , 1]) corresponds to the detector width dimension (col in Detector).
  # The second col of dead (dead[ , 2]) corresponds to the detector height dimension (row in Detector)

  dead_pix_coords <- which(pix_matrix == 1, arr.ind = TRUE)

  colnames(dead_pix_coords) <- c("col", "row")

  return(dead_pix_coords)
}

#' Fits pixel distance from the centre using glm
#'
#' @param detector Detector object
#' @return Fitted model
#' @export
glm_pixel_ctr_eucl <- function(detector) {
  dist <- pixel_dist_ctr_eucl(detector)
  pix_matrix <- detector$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel parallel maxima from the centre using glm
#'
#' @param detector Detector object
#' @return Fitted model
#' @export
glm_pixel_ctr_linf <- function(detector) {
  dist <- pixel_dist_ctr_linf(detector)
  pix_matrix <- detector$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel distances from the module edges by column using glm
#'
#' @param detector Detector object
#' @return Fitted model
#' @export
glm_pixel_dist_edge_col <- function(detector) {
  dist <- dist_edge_col(detector)
  pix_matrix <- detector$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel distances from module edges by row using glm
#'
#' @param detector Detector object
#' @return Fitted model
#' @export
glm_pixel_dist_edge_row <- function(detector) {
  dist <- dist_edge_row(detector)
  pix_matrix <- detector$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel distances to the nearest sub-panel edge using glm
#'
#' @param detector Detector object
#' @return Fitted model
#' @export
glm_pixel_dist_edge_min <- function(detector) {
  dist <- dist_edge_min(detector)

  pix_matrix <- detector$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel distances to the nearest corner using glm
#'
#' @param detector Detector object
#' @return Fitted model
#' @export
glm_pixel_dist_corner <- function(detector) {
  dist <- dist_corner(detector)

  pix_matrix <- detector$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Performs glm fitting on the specified symbolic expression
#'
#' @param symb_expr symbolic description of the linear predictor
#' @param family a description of the error distribution
#' @return Fitted model
#' @importFrom stats binomial glm
#' @export
perform_glm <- function(symb_expr, family = binomial(link = logit)) {

  #' @return glm_git fitted model
  glm_result <- glm(formula = symb_expr, family = family)

  return(glm_result)
}

#' Generate summary of damaged pixels
#'
#' @param detector Detector object
#' @return Dead_Stats object
#' @importFrom stats chisq.test
#' @export
get_dead_stats <- function(detector) {
  ppp_dead <- get_ppp_dead(detector)

  # count of points in each quadrat
  module_count_arr <- spatstat::quadratcount(
    X = ppp_dead,
    nx = detector$module_col_n,
    ny = detector$module_row_n
  )

  # count of points in each quadrat
  module_count <- as.vector(module_count_arr)

  dead_n <- length(as.vector(detector$pix_dead[, 2]))

  # This works only on rectangular detectors!!!
  module_n <- detector$module_col_n * detector$module_row_n

  avg_dead_mod <- round(dead_n / module_n, digits = 1)

  if (module_n <= 1) {
    Chisq_s <- NA
    Chisq_df <- NA
    Chisq_p <- NA
  } else {
    # Xi Squared Test
    Chisq <- chisq.test(x = module_count, p = rep(1 / module_n, module_n))
    Chisq_s <- Chisq$statistic
    Chisq_df <- Chisq$parameter
    Chisq_p <- Chisq$p.value
  }

  dead_stats <- Dead_Stats(
    dead_n = dead_n, module_n = module_n,
    module_count_arr = module_count_arr, module_count = module_count,
    avg_dead_mod = avg_dead_mod, Chisq_s = Chisq_s,
    Chisq_df = Chisq_df, Chisq_p = Chisq_p
  )

  detector$dead_stats <- dead_stats

  return(detector)
}

#' Summary of damaged pixels
#'
#' @param detector Detector object
#' @return A string with damaged pixels overall statitics
#' @export
dead_stats_summary <- function(detector) {
  detector <- get_dead_stats(detector)

  summary <- paste("Total number of damaged pixels: ", detector$dead_stats$dead_n, "\n", "")
  summary <- paste(summary, "Total number of modules: ", detector$dead_stats$module_n, "\n", "")
  summary <- paste(summary, "Average number of damaged pixels per module: ", detector$dead_stats$avg_dead_mod, "\n", "")
  summary <- paste(summary, "\n", "")
  summary <- paste(summary, "Chi-Squared Test results:\n", "")
  summary <- paste(summary, "Xsq = ", detector$dead_stats$Chisq_s, ", Xsq df = ", detector$dead_stats$Chisq_df, ", Xsq p = ", detector$dead_stats$Chisq_p, "\n", "")

  return(summary)
}

#' A function to plot NN angles of dead pixels of detector or module
#'
#' @param detector Detector object
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @export
plot_pixels_angles <- function(detector, file_path = NA, row = NA, col = NA,
                                 caption = TRUE) {
  ppp_dead <- NA
  main_caption <- ""

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    # get the ppp for the selected module
    ppp_dead <- .get_ppp_dead_module(detector, row, col)

    if (caption) {
      main_caption <- paste("NN to points orientations (row =", row, "col =", col, ")")
    }
  } else {
    ppp_dead <- get_ppp_dead(detector)

    if (caption) {
      main_caption <- paste("NN to points orientations ", ppp_dead$n, " dead pixels\n", sep = "")
    }
  }

  plot_angles(ppp_dead, main_caption, file_path = file_path)
}

# # TODO: define the function
# #' Get orient nn PP
# #' @param PPPdata ppp object
# #' @return describe
# #' @export
# orientnnPPP <- function(PPPdata) {
#   PPPnn <- PPPdata[spatstat::nnwhich(PPPdata)]
#   # now calculate our own thing for the orientations to compare
#   A <- matrix(c(PPPdata$x, PPPdata$y), nrow = 2, ncol = length(PPPdata$x), byrow = TRUE)
#   # x,y values of original point pattern
#   Ann <- matrix(c(PPPnn$x, PPPnn$y), nrow = 2, ncol = length(PPPnn$x), byrow = TRUE)
#   # x,y values of point pattern containing nn of each of the points in original
#   # Assigns a point patters (ppp object) a vector of the orientations of the arrows pointing from nearest neighbours to its points
#   return(round(apply(rbind(A, Ann), 2, orientcolfct), digits = 3))
# }

#' Estimates the norm of a vector
#'
#' @param v vector
#' @return norm of the vector v
.norm_vec <- function(v) {
  norm <- sqrt(sum(v^2))
  return(norm)
}

# #' Estimates the distance between vectors v and w
# #'
# #' @param v vector
# #' @param w vector
# #' @return distance between vectors v and w
# #' @export
# dist_vec <- function(v, w) {
#   .norm_vec(v - w)
# }

# #' Calculates distance and orientation of the oriented vector between two points
# #' in order of the second pointing to first (reflecting nearest neighbour (nn) framework)
# #' v, w point coordinates indicating vectors wrt to the origin.
# #' Values: distance and orientation (in [0,360) degrees) of w pointing towards v.
# #' @param v vector
# #' @param w vector
# #' @return distance and orientation of the oriented vector between two points
# #' @export
# orient_dist_vec <- function(v, w) {
#   v <- v - w
#   w <- c(0, 0)
#   tmp1 <- .norm_vec(v)
#   v <- v / tmp1
#   x <- v[1]
#   y <- v[2]
#   tmp2 <- asin(abs(y)) * 180 / pi

#   if (x >= 0) {
#     if (y < 0) {
#       tmp2 <- 360 - tmp2
#     }
#   } else {
#     tmp2 <- 180 - sign(y) * tmp2
#   }

#   orient_dist_vec <- list(tmp2, tmp1)
#   names(orient_dist_vec) <- c("orient", "dist")

#   return(orient_dist_vec)
# }


# #' Calculates orientation of the oriented vector between two points
# #' in order of the second pointing to first (reflecting nearest neighbour (nn) framework)
# #' @param b vector with elements 1:2 correspoding to first point and 3:4 corresponding to the second
# #' @return orientation of the oriented vector between two points
# #' @export
# orientcolfct <- function(b) orient_dist_vec(b[1:2], b[3:4])$orient

#' Generates ppp for the dead pixels
#'
#' @param detector Detector object
#' @return ppp of dead pixels
#' @export
get_ppp_dead <- function(detector) {
  if (suppressWarnings(any(is.na(detector$pix_dead)))) {
    stop("Is the damaged pixel data loaded?")
  }

  ppp_dead <- spatstat::ppp(
    detector$pix_dead[, 1], detector$pix_dead[, 2],
    c(1, detector$detector_width),
    c(1, detector$detector_height)
  )

  return(ppp_dead)
}

#' Generates ppp for the dead pixels for a selected module
#'
#' @param detector Detector object
#' @param row module row number
#' @param col module column number
#' @return ppp of dead pixels
.get_ppp_dead_module <- function(detector, row, col) {
  module_sel <- detector$pix_dead_modules[detector$pix_dead_modules[, 3] == col &
    detector$pix_dead_modules[, 4] == row, ]

  ppp_dead <- spatstat::ppp(
    module_sel[, 1],
    module_sel[, 2],
    c(
      detector$module_edges_col[1, col],
      detector$module_edges_col[2, col]
    ),
    c(
      detector$module_edges_row[1, row],
      detector$module_edges_row[2, row]
    )
  )

  return(ppp_dead)
}

#' Plots K, F, G functions
#'
#' @param detector Detector object
#' @param func Function name
#' @param file_path Output file path
#' @param row module row number
#' @param col module column number
#' @param caption Flag to turn on/off figure caption
#' @export
plot_pixels_kfg <- function(detector, func, file_path = NA, row = NA, col = NA,
                              caption = TRUE) {
  ppp_dead <- get_ppp_dead(detector)

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(detector, row, col)

    # get the ppp for the selected module
    ppp_dead <- .get_ppp_dead_module(detector, row, col)
  } else {
    ppp_dead <- get_ppp_dead(detector)
  }

  plot_kfg(ppp_dead, func, file_path = file_path, caption = caption)
}
