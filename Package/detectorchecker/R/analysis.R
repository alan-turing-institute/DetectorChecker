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

#' Plotting a module of a layout
#'
#' @param layout Layout object
#' @param mod_col Module column number
#' @param mod_row Module row number
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @export
plot_layout_module_damaged <- function(layout, col, row, file_path = NA,
                                       caption = TRUE) {

  if (!caption) par(mar = c(0, 0, 0, 0))

  # check whether the row and col numbers are correct
  .check_select(layout, row, col)

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  shift_left <- layout$module_edges_col[1, col] - 1
  shift_up <- layout$module_edges_row[1, row] - 1

  width <- layout$module_col_sizes[col]
  height <- layout$module_row_sizes[row]

  ppp_frame <- spatstat::ppp(1, 1, c(1, width), c(1, height))

  if(caption) {
    main_caption <- paste(layout$name, "with damaged pixels\n (black=module edges)")

  } else {
    main_caption <- ""
  }

  plot(ppp_frame, pch = ".", cex.main = 0.7, main = main_caption)

  # selecting dead pixels for the particular module
  module_sel <- layout$pix_dead_modules[layout$pix_dead_modules[ , 3] == col &
                                          layout$pix_dead_modules[ , 4] == row,]
  module_sel[ , 1] <- module_sel[ , 1] - shift_left
  module_sel[ , 2] <- module_sel[ , 2] - shift_up

  ppp_dead <- spatstat::ppp(module_sel[ , 1], module_sel[ , 2],
                            c(1, width), c(1, height))

  points(ppp_dead, pch = 22, col = "brown", cex = 0.7)

  if(!is.na(file_path)) {
    # shuts down the specified (by default the current) device
    dev.off()
  }
}

#' A function to plot layout with damaged pixels
#'
#' @param layout Layout object
#' @param file_path Output file path
#' @param caption Flag to turn on/off figure caption
#' @importFrom graphics points
#' @export
plot_layout_damaged <- function(layout, file_path = NA, caption = TRUE) {

  main_caption <- ""
  if (!caption) par(mar = c(0, 0, 0, 0))

  ppp_dead <- get_ppp_dead(layout)

  ppp_edges_col <- create_ppp_edges_col(layout)
  ppp_edges_row <- create_ppp_edges_row(layout)

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if (sum(layout$gap_col_sizes) + sum(layout$gap_row_sizes) == 0) {

    if(caption) {
      main_caption <- paste(layout$name, "with damaged pixels\n (black=module edges)")
    }

    # vertical lines in x-positions given by xlines
    plot(ppp_edges_col, pch = ".", cex.main = 0.7, main = main_caption)

    # horizontal lines in y-positions given by ylines
    points(ppp_edges_row, pch = ".")

  } else {

    # Define point patterns (spatstat) capturing gaps
    ppp_gaps_col <- create_ppp_gaps_col(layout)
    ppp_gaps_row <- create_ppp_gaps_row(layout)

    if(caption) {
      main_caption <- paste(layout$name, "with damaged pixels\n (black=module edges, grey=gaps)")
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
  # because of the titles and because they layout should be printed under the
  # damaged pixels rather than cover them up.

  points(ppp_dead, pch = 22, col = "brown", cex = 0.7)

  if(!is.na(file_path)) {
    dev.off()
  }
}

#' A function to plot layout with counts per module
#'
#' @param layout Layout object
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @export
plot_layout_cnt_mod <- function(layout, file_path = NA, row = NA, col = NA,
                                caption = TRUE) {

  if (!caption) par(mar = c(0, 0, 0, 0))

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if(any(is.na(layout$dead_stats))) {
    stop("Is the damaged statistics calculated?")
  }

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(layout, row, col)

    if(caption) {
      main_caption <- paste("Number of damaged pixels: ",
                            layout$dead_stats$module_count_arr[col][row])
    } else {
      main_caption <- ""
    }

    width <- layout$module_col_sizes[col]
    height <- layout$module_row_sizes[row]

    ppp_frame <- spatstat::ppp(1, 1, c(1, width), c(1, height))

    plot(ppp_frame, pch = ".", cex.main = 0.7, main = main_caption)

    # This works only on rectangular layouts!!!
    module_idx <- (col - 1) * layout$module_row_n + row

    text(width/2, height/2, label = layout$dead_stats$module_count[module_idx])

  } else {
    if(caption) {
      main_caption <- paste("Number of damaged pixels in modules\n",
                            "Total number damaged pixels: ", layout$dead_stats$dead_n,
                             "\n (average per module: ", layout$dead_stats$avg_dead_mod, ")")
    } else {
      main_caption <- ""
    }

    plot(layout$dead_stats$module_count_arr, main = main_caption)
  }

  if(!is.na(file_path)) {
    dev.off()
  }
}

#' A function to plot layout with dead pixel densities
#'
#' @param layout Layout object
#' @param file_path Output file path
#' @param adjust Kernel density bandwidth
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @export
plot_layout_density <- function(layout, file_path = NA, adjust = 0.25,
                                row = NA, col = NA, caption = TRUE) {

  ppp_dead <- NA
  main_caption <- ""

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(layout, row, col)

    # get the ppp for the selected module
    ppp_dead <- .get_ppp_dead_module(layout, row, col)

    if (caption) {
      main_caption <- paste("Dead pixel density (row=", row, "col=", col, "), adjust=", adjust)
    }

  } else {
    ppp_dead <- get_ppp_dead(layout)

    if (caption) {
      main_caption <- paste("Dead pixel density, adjust = ", adjust)
    }
  }

  plot_density(ppp_dead, main_caption, file_path = file_path, adjust = adjust)
}


#' A function to plot NN oriented arrrows
#'
#' @param layout Layout object
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @importFrom graphics arrows plot
#' @export
plot_layout_arrows <- function(layout, file_path = NA, row = NA, col = NA,
                               caption = TRUE) {

  main_caption <- ""
  if (!caption) par(mar = c(0, 0, 0, 0))
  else par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(layout, row, col)

    # get the ppp for the selected module
    ppp_dead <- .get_ppp_dead_module(layout, row, col)

    if(caption) {
      main_caption <- paste("NN oriented arrows (row=", row, "col=", col, ")")
    }

  } else {
    ppp_dead <- get_ppp_dead(layout)

    if(caption) {
      main_caption <- "NN oriented arrows"
    }
  }

  PPPnn <- ppp_dead[spatstat::nnwhich(ppp_dead)]

  plot(ppp_dead, main = main_caption)

  arrows(PPPnn$x, PPPnn$y, ppp_dead$x, ppp_dead$y,
         angle = 15, length = 0.07, col = "red")

  if(!is.na(file_path)) {
    dev.off()
  }
}

#' Extracts a table of dead pixel coordinates from a pixel matrix
#'
#' @param pix_matrix pixel matrix with dead pixels flagged with 1
#' @return Table containing dead pixel coordinates
#' @export
dead_pix_coords <- function(pix_matrix) {

  # Matrix of damaged pixels coordinates
  # The first col of dead (dead[ , 1]) corresponds to the detector width dimension (col in Layout).
  # The second col of dead (dead[ , 2]) corresponds to the detector height dimension (row in Layout)

  dead_pix_coords <- which(pix_matrix == 1, arr.ind = TRUE)

  colnames(dead_pix_coords) <- c("col", "row")

  return(dead_pix_coords)
}

#' Fits pixel distance from the centre to
#'
#' @param layout Layout object
#' @return Fitted model
#' @export
glm_pixel_ctr_eucl <- function(layout) {

  dist <- pixel_dist_ctr_eucl(layout)
  pix_matrix <- layout$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel parallel maxima from the centre
#'
#' @param layout Layout object
#' @return Fitted model
#' @export
glm_pixel_ctr_linf <- function(layout) {

  dist <- pixel_dist_ctr_linf(layout)
  pix_matrix <- layout$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel istances from the module edges by column
#'
#' @param layout Layout object
#' @return Fitted model
#' @export
glm_pixel_dist_edge_col <- function(layout) {

  dist <- dist_edge_col(layout)
  pix_matrix <- layout$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel istances from the module edges by row
#'
#' @param layout Layout object
#' @return Fitted model
#' @export
glm_pixel_dist_edge_row <- function(layout) {

  dist <- dist_edge_row(layout)
  pix_matrix <- layout$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Performs model fitting on the specified symbolic expression
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

#' Count number of damaged pixels overall and in different modules
#'
#' @param layout Layout object
#' @return Dead_Stats object
#' @importFrom stats chisq.test
#' @export
get_dead_stats <- function(layout) {

  ppp_dead <- get_ppp_dead(layout)

  # count of points in each quadrat
  module_count_arr <- spatstat::quadratcount(X = ppp_dead,
                                             nx = layout$module_col_n,
                                             ny = layout$module_row_n)

  # count of points in each quadrat
  module_count <- as.vector(module_count_arr)

  dead_n <- length(as.vector(layout$pix_dead[ , 2]))

  # This works only on rectangular layouts!!!
  module_n <- layout$module_col_n * layout$module_row_n

  avg_dead_mod <- round(dead_n / module_n, digits = 1)

  # Xi Squared Test
  Chisq <- chisq.test(x = module_count, p = rep( 1 / module_n, module_n))
  Chisq_s <- Chisq$statistic
  Chisq_df <- Chisq$parameter
  Chisq_p <- Chisq$p.value

  dead_stats <- Dead_Stats(dead_n = dead_n, module_n = module_n,
                           module_count_arr = module_count_arr, module_count = module_count,
                           avg_dead_mod = avg_dead_mod, Chisq_s = Chisq_s,
                           Chisq_df = Chisq_df, Chisq_p = Chisq_p)

  layout$dead_stats <- dead_stats

  return(layout)
}

#' Count number of damaged pixels overall and in different modules
#'
#' @param layout Layout object
#' @return A string with damaged pixels overall statitics
#' @export
dead_stats_summary <- function(layout) {

  layout <- get_dead_stats(layout)

  summary <- paste("Total number of damaged pixels: ", layout$dead_stats$dead_n, "\n", "")
  summary <- paste(summary, "Total number of modules: ", layout$dead_stats$module_n, "\n", "")
  summary <- paste(summary, "Average number of damaged pixels per module: ", layout$dead_stats$avg_dead_mod, "\n", "")
  summary <- paste(summary, "\n", "")
  summary <- paste(summary, "Chi-Squared Test results:\n", "")
  summary <- paste(summary, "Xsq = ", layout$dead_stats$Chisq_s, ", Xsq df = ", layout$dead_stats$Chisq_df, ", Xsq p = ", layout$dead_stats$Chisq_p, "\n", "")

  return(summary)
}

#' ANGLES using nnorient() from spatstat package
#'
#' @param layout Layout object
#' @param file_path Output file path
#' @param row Module row number
#' @param col Module column number
#' @param caption Flag to turn on/off figure caption
#' @export
plot_layout_angles <- function(layout, file_path = NA, row = NA, col = NA,
                               caption = TRUE) {

  main_caption <- ""
  if (!caption) par(mar = c(0, 0, 0, 0))
  else par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))

  ppp_dead <- get_ppp_dead(layout)

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(layout, row, col)

    # get the ppp for the selected module
    ppp_dead <- .get_ppp_dead_module(layout, row, col)

    if(caption) {
      main_caption <- paste("NN to points orientations (row=", row, "col=", col, ")")
    }

  } else {
    ppp_dead <- get_ppp_dead(layout)

    if(caption) {
      main_caption <- paste("NN to points orientations ", ppp_dead$n, " dead pixels\n", sep="")
    }
  }

  spatstat::rose(spatstat::nnorient(ppp_dead, sigma = 4),
                 col = "grey", main = main_caption)

  if(!is.na(file_path)) {
    dev.off()
  }
}

# TODO: define the function
#' Get orient nn PP
#' @param PPPdata describe
#' @return describe
#' @export
orientnnPPP <- function(PPPdata) {
  PPPnn <- PPPdata[spatstat::nnwhich(PPPdata)]
  #now calculate our own thing for the orientations to compare
  A <- matrix(c(PPPdata$x,PPPdata$y), nrow=2, ncol=length(PPPdata$x), byrow=TRUE)
  # x,y values of original point pattern
  Ann <- matrix(c(PPPnn$x,PPPnn$y), nrow=2, ncol=length(PPPnn$x), byrow=TRUE)
  # x,y values of point pattern containing nn of each of the points in original
  # Assigns a point patters (ppp object) a vector of the orientations of the arrows pointing from nearest neighbours to its points
  return(round(apply(rbind(A,Ann), 2, orientcolfct), digits=3))
}

#' Estimates the norm of a vector
#'
#' @param v vector
#' @return norm of the vector v
#' @export
norm_vec <- function(v) {
  norm <- sqrt(sum(v^2))
  return(norm)
}

#' Estimates the distance between vectors v and w
#'
#' @param v vector
#' @param w vector
#' @return distance between vectors v and w
#' @export
dist_vec <- function(v,w) {
  norm_vec(v - w)
}

#' Calculates distance and orientation of the oriented vector between two points
#' in order of the second pointing to first (reflecting nearest neighbour (nn) framework)
#' v, w point coordinates indicating vectors wrt to the origin.
#' Values: distance and orientation (in [0,360) degrees) of w pointing towards v.
#' @param v vector
#' @param w vector
#' @return distance and orientation of the oriented vector between two points
#' @export
orient_dist_vec <- function(v, w){
  v <- v - w
  w <- c(0, 0)
  tmp1 <- norm_vec(v)
  v <- v / tmp1
  x <- v[1]
  y <- v[2]
  tmp2 <- asin(abs(y)) * 180 / pi

  if (x >= 0){
    if (y < 0) {
      tmp2 <- 360 - tmp2
    }
  } else {
    tmp2 <- 180 - sign(y) * tmp2
  }

  orient_dist_vec <- list(tmp2, tmp1)
  names(orient_dist_vec) <- c("orient", "dist")

  return(orient_dist_vec)
}

# TODO: define the function
#'
#' @export
orientcolfct <- function(b) orient_dist_vec(b[1:2],b[3:4])$orient

#' Generates ppp for the dead pixels
#'
#' @param layout Layout object
#' @return ppp of dead pixels
#' @export
get_ppp_dead <- function(layout) {

  if(suppressWarnings(any(is.na(layout$pix_dead)))) {
    stop("Is the damaged pixel data loaded?")
  }

  ppp_dead <- spatstat::ppp(layout$pix_dead[ , 1], layout$pix_dead[ , 2],
                            c(1, layout$detector_width),
                            c(1, layout$detector_height))

  return(ppp_dead)
}

#' Generates ppp for the dead pixels for a selected module
#'
#' @param layout Layout object
#' @param row module row number
#' @param col module column number
#' @return ppp of dead pixels
.get_ppp_dead_module <- function(layout, row, col) {

  module_sel <- layout$pix_dead_modules[layout$pix_dead_modules[ , 3] == col &
                                          layout$pix_dead_modules[ , 4] == row,]

  ppp_dead <- spatstat::ppp(module_sel[ , 1],
                            module_sel[ , 2],
                            c(layout$module_edges_col[1, col],
                              layout$module_edges_col[2, col]),
                            c(layout$module_edges_row[1, row],
                              layout$module_edges_row[2, row]))

  return(ppp_dead)
}

#' Plots K, F, G functions
#'
#' @param layout Layout object
#' @param func Function name
#' @param file_path Output file path
#' @param row module row number
#' @param col module column number
#' @param caption Flag to turn on/off figure caption
#' @importFrom stats density
#' @importFrom grDevices dev.off
#' @export
plot_kfg <- function(layout, func, file_path = NA, row = NA, col = NA,
                     caption = TRUE) {

  if (!caption) par(mar = c(0, 0, 0, 0))

  if (missing(func) || is.null(func)) {
    stop(c("Analysis function name is not specified.\n",
           "Available functions: K, F, G, Kinhom, Finhom, Ginhom"))
  }

  main_caption <- ""
  ppp_dead <- get_ppp_dead(layout)

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if (!is.na(row) && !is.na(col)) {
    # check whether the row and col numbers are correct
    .check_select(layout, row, col)

    # get the ppp for the selected module
    ppp_dead <- .get_ppp_dead_module(layout, row, col)

  } else {
    ppp_dead <- get_ppp_dead(layout)
  }

  if(func == "K") {

    if(caption) main_caption <- "K-function"

    plot(spatstat::Kest(ppp_dead), main = main_caption)

  } else if (func == "F") {

    if(caption) main_caption <- "F-function"

    plot(spatstat::Fest(ppp_dead), main = main_caption)

  } else if (func == "G") {

    if(caption) main_caption <- "G-function"

    plot(spatstat::Gest(ppp_dead), main = main_caption)

  } else if (func == "Kinhom") {
    lambda <- density(ppp_dead)
    if(caption) main_caption <- "Inhomogeneous K-Function"

    plot(spatstat::Kinhom(ppp_dead, lambda, correction = "all"),
         cex = 0.5, main = main_caption)

  } else if (func == "Finhom") {
    lambda <- density(ppp_dead)
    if(caption) main_caption <- "Inhomogeneous F-Function"

    plot(spatstat::Finhom(ppp_dead, lambda, correction = "all"),
         cex = 0.5, main = main_caption)

  } else if (func == "Ginhom") {
    lambda <- density(ppp_dead)
    if(caption) main_caption <- "Inhomogeneous G-Function"

    plot(spatstat::Ginhom(ppp_dead, lambda, correction = "all"),
         cex = 0.5, main = main_caption)

  } else {
    stop(c("Cannot identify analysis function.\n",
           "Available functions: K, F, G, Kinhom, Finhom, Ginhom"))
  }

  if(!is.na(file_path)) {
    dev.off()
  }
}
