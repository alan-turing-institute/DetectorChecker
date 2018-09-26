#' A S3 class to represent dead pixels statistics summary
#'
#' @slot dead_n Total number of damaged pixels:
#' @slot module_n Total number of modules
#' @slot module_count_arr Count of dead pixels in each quadrat
#' @slot module_count Count of dead pixels in each quadrat
#' @slot avg_dead_mod Average number of damaged pixels per module
#' @slot Chisq_s The Chi-Squared test statistic value
#' @slot Chisq_df Chi-Squared degrees of freedom
#' @slot Chisq_p Chi-Squared p-value
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
plot_layout_damaged <- function(layout, file_path) {

  dirOut <- getwd()

  pix_dead <- layout$pix_dead

  ppp_dead <- spatstat::ppp(pix_dead[ , 1], pix_dead[ , 2],
                            c(1, layout$detector_width),
                            c(1, layout$detector_height))

  ppp_edges_col <- create_ppp_edges_col(layout)
  ppp_edges_row <- create_ppp_edges_row(layout)

  # starts the graphics device driver
  ini_graphics(file_path = file_path)

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
  dev.off()
}

#' A function to plot layout with counts per module
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_layout_cnt_mod <- function(layout, file_path) {

  # starts the graphics device driver
  ini_graphics(file_path = file_path)

  plot(layout$dead_stats$module_count_arr,
       main = paste("Number of damaged pixels in modules\n", "Total number damaged pixels: ", layout$dead_stats$dead_n,
                                        "\n (average per module: ", layout$dead_stats$avg_dead_mod, ")"))

  dev.off()
}

#' A function to plot layout with dead pixel densities
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_layout_density <- function(layout, file_path, adjust = 0.25) {

  # starts the graphics device driver
  ini_graphics(file_path = file_path)

  title <- paste("Dead pixel density, adjust = ", adjust)

  ppp_dead <- spatstat::ppp(layout$pix_dead[ , 1], layout$pix_dead[ , 2],
                            c(1, layout$detector_width),
                            c(1, layout$detector_height))

  image(density(ppp_dead, adjust = adjust), main = title)

  dev.off()
}

#' A function to plot NN oriented arrrows
#'
#' @param layout Layout object
#' @param file_path Output file path
plot_layout_arrows <- function(layout, file_path) {

  title <- "NN oriented arrows"

  # starts the graphics device driver
  ini_graphics(file_path = file_path)

  par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))

  ppp_dead <- spatstat::ppp(layout$pix_dead[ , 1], layout$pix_dead[ , 2],
                            c(1, layout$detector_width),
                            c(1, layout$detector_height))

  PPPnn <- ppp_dead[spatstat::nnwhich(ppp_dead)]

  plot(ppp_dead, main = title)

  arrows(PPPnn$x, PPPnn$y, ppp_dead$x, ppp_dead$y,
         angle = 15, length = 0.07, col = "red")

  dev.off()
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

  ppp_dead <- spatstat::ppp(layout$pix_dead[ , 1], layout$pix_dead[ , 2],
                            c(1, layout$detector_width),
                            c(1, layout$detector_height))

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

