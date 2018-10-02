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
#'
#' @return Dead_Stats object
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

#' A function to plot layout with damaged pixels
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_layout_damaged <- function(layout, file_path = NA) {

  ppp_dead <- get_ppp_dead(layout)

  ppp_edges_col <- create_ppp_edges_col(layout)
  ppp_edges_row <- create_ppp_edges_row(layout)

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if (sum(layout$gap_col_sizes) + sum(layout$gap_row_sizes) == 0) {

    # vertical lines in x-positions given by xlines
    plot(ppp_edges_col, pch = ".", cex.main = 0.7,
         main = paste(layout$name, "with damaged pixels\n (black=module edges)"))

    # horizontal lines in y-positions given by ylines
    points(ppp_edges_row, pch = ".")

  } else {

    # Define point patterns (spatstat) capturing gaps
    ppp_gaps_col <- create_ppp_gaps_col(layout)
    ppp_gaps_row <- create_ppp_gaps_row(layout)

    # vertical lines in x-positions given by xlines
    plot(ppp_edges_col, pch = ".", cex.main = 0.7,
         main = paste(layout$name, "with damaged pixels\n (black=module edges, grey=gaps)"))

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
plot_layout_cnt_mod <- function(layout, file_path = NA) {

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  if(any(is.na(layout$dead_stats))) {
    stop("Is the damaged statistics is performed?")
  }

  plot(layout$dead_stats$module_count_arr,
       main = paste("Number of damaged pixels in modules\n", "Total number damaged pixels: ", layout$dead_stats$dead_n,
                                        "\n (average per module: ", layout$dead_stats$avg_dead_mod, ")"))

  if(!is.na(file_path)) {
    dev.off()
  }
}

#' A function to plot layout with dead pixel densities
#'
#' @param layout Layout object
#' @param file_path Output file path
#' @param adjust Kernel density bandwidth
plot_layout_density <- function(layout, file_path = NA, adjust = 0.25) {

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  title <- paste("Dead pixel density, adjust = ", adjust)

  ppp_dead <- get_ppp_dead(layout)

  image(density(ppp_dead, adjust = adjust), main = title)

  if(!is.na(file_path)) {
    dev.off()
  }
}

#' A function to plot NN oriented arrrows
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_layout_arrows <- function(layout, file_path = NA) {

  title <- "NN oriented arrows"

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))

  ppp_dead <- get_ppp_dead(layout)

  PPPnn <- ppp_dead[spatstat::nnwhich(ppp_dead)]

  plot(ppp_dead, main = title)

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
perform_glm <- function(symb_expr, family = binomial(link = logit)) {

  #' @return glm_git fitted model
  glm_result <- glm(formula = symb_expr, family = family)

  return(glm_result)
}

#' Count number of damaged pixels overall and in different modules
#'
#' @param layout Layout object
#' @return Dead_Stats object
get_dead_stats <- function(layout) {

  ppp_dead <- get_ppp_dead(layout)

  # count of points in each quadrat
  module_count_arr <- spatstat::quadratcount(X = ppp_dead,
                                             nx = layout$module_col_n,
                                             ny = layout$module_row_n)

  # count of points in each quadrat
  module_count <- as.vector(module_count_arr)

  dead_n <- length(as.vector(layout$pix_dead[ , 2]))
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
plot_layout_angles <- function(layout, file_path = NA) {

  ppp_dead <- get_ppp_dead(layout)

  title <- paste("NN to points orientations ", ppp_dead$n, " dead pixels\n", sep="")

  if(!is.na(file_path)) {
    # starts the graphics device driver
    ini_graphics(file_path = file_path)
  }

  par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))

  spatstat::rose(spatstat::nnorient(ppp_dead, sigma = 4),
                 col = "grey", main = title)

  if(!is.na(file_path)) {
    dev.off()
  }
}

# TODO: define the function
#' Get orient nn PP
#' @param PPPdata describe
#' @return describe
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

# TODO: Fix this
#' Testing analysis functions
#'
#' @param layout Layout object
module_indiv_rose <- function(layout) {

  dirOut <- getwd()

  ppp_dead <- get_ppp_dead(layout)

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

  # Make angle histograms (rose plots) and pixel plots with arrows
  for (i in 1:layout$module_col_n) {
    for (j in 1:layout$module_row_n) {

      # goes through modules in the order: start bottom left, move up, start over 2nd col, move up...
      dead_module_select <- dead_modules[dead_modules[ , 3] == i & dead_modules[ , 4] == j, ]

      ppp_dead_module_select <- spatstat::ppp(dead_module_select[ , 1],
                                    dead_module_select[ , 2],
                                    c(layout$module_edges_col[1, i],
                                      layout$module_edges_col[2, i]),
                                    c(layout$module_edges_row[1, j],
                                      layout$module_edges_row[2, j]))

      # need at least 2 points to calculate neighbours!
      if (ppp_dead_module_select$n >= 2){

        PPPnn <- ppp_dead_module_select[spatstat::nnwhich(ppp_dead_module_select)]

        pdf(paste(dirOut, "/", "anglesOrientNNindivModules_DIY_",i,"_",j,"_",
                  layout$name, ".pdf", sep = ""), bg = "transparent")

        par(mfrow = c(1, 2), mar = c(1, 1, 5, 1), oma = c(1, 1, 5, 1))

        plot(ppp_dead_module_select, main = "NN oriented arrows")

        arrows(PPPnn$x, PPPnn$y,
               ppp_dead_module_select$x, ppp_dead_module_select$y,
               angle = 15, length = 0.07, col = "red")

        spatstat::rose(orientnnPPP(ppp_dead_module_select), breaks=72,
             main="NN oriented angles")

        title(paste("Module in layout position col=", i,
                    " row=", j, "\n", ppp_dead_module_select$n,
                    " dead pixels\n", sep = ""), outer = TRUE)

        dev.off()
      }
    }
  }
}

#' Estimates the norm of a vector
#'
#' @param v vector
#' @return norm of the vector v
norm_vec <- function(v) {
  norm <- sqrt(sum(v^2))
  return(norm)
}

#' Estimates the distance between vectors v and w
#'
#' @param v vector
#' @param w vector
#' @return distance between vectors v and w
dist_vec <- function(v,w) {
  norm_vec(v - w)
}

# Calculates distance and orientation of the oriented vector between two points
# in order of the second pointing to first (reflecting nearest neighbour (nn) framework)
# v, w point coordinates indicating vectors wrt to the origin.
# Values: distance and orientation (in [0,360) degrees) of w pointing towards v.
#' @param v vector
#' @param w vector
#' @return distance and orientation of the oriented vector between two points
orientdist_vec <- function(v, w){
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

  orientdist_vec <- list(tmp2, tmp1)
  names(orientdist_vec) <- c("orient", "dist")

  return(orientdist_vec)
}

# TODO: define the function
#'
orientcolfct <- function(b) orientdist_vec(b[1:2],b[3:4])$orient

#' Generates ppp for the dead pixels
#'
#' @param layout Layout object
#' @return ppp of dead pixels
get_ppp_dead <- function(layout) {

  if(suppressWarnings(any(is.na(layout$pix_dead)))) {
    stop("Is the damaged pixel data loaded?")
  }

  ppp_dead <- spatstat::ppp(layout$pix_dead[ , 1], layout$pix_dead[ , 2],
                            c(1, layout$detector_width),
                            c(1, layout$detector_height))

  return(ppp_dead)
}

