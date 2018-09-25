#' A S3 class to represent dead pixels statistics summary
#'
#' @slot dead_n Total number of damaged pixels:
#' @slot module_n Total number of modules
#' @slot avg_dead_mod Average number of damaged pixels per module
#' @slot Chisq_s The Chi-Squared test statistic value
#' @slot Chisq_df Chi-Squared degrees of freedom
#' @slot Chisq_p Chi-Squared p-value
#'
#' @return dead_stats Dead_Stats object
Dead_Stats <- function(dead_n = NA, module_n = NA, avg_dead_mod = NA,
                       Chisq_s = NA, Chisq_df = NA, Chisq_p = NA) {

  dead_stats <- list(
    dead_n = dead_n,
    module_n = module_n,
    avg_dead_mod = avg_dead_mod,
    Chisq_s = Chisq_s,
    Chisq_df = Chisq_df,
    Chisq_p = Chisq_p
  )

  return(dead_stats)
}

#' A function to plot layout with damaged pixels
#'
#' @slot layout Layout object
#' @slot file_path Output file path
plot_layout_damaged <- function(layout, file_path) {

  dirOut <- getwd()

  pix_dead <- layout$pix_dead

  ppp_dead <- ppp(pix_dead[ , 1], pix_dead[ , 2],
                  c(1, layout$detector_width), c(1, layout$detector_height))

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

#' Extracts a table of dead pixel coordinates from a pixel matrix
#'
#' @slot pix_matrix pixel matrix with dead pixels flagged with 1
#' @return dead_pix_coords a table containing dead pixel coordinates
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
#' @slot layout Layout object
#' @return glm_fit Fitted model
glm_pixel_ctr_eucl <- function(layout) {

  dist <- pixel_dist_ctr_eucl(layout)
  pix_matrix <- layout$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel parallel maxima from the centre
#'
#' @slot layout Layout object
#' @return glm_fit Fitted model
glm_pixel_ctr_linf <- function(layout) {

  dist <- pixel_dist_ctr_linf(layout)
  pix_matrix <- layout$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel istances from the module edges by column
#'
#' @slot layout Layout object
#' @return glm_fit Fitted model
glm_pixel_dist_edge_col <- function(layout) {

  dist <- dist_edge_col(layout)
  pix_matrix <- layout$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Fits pixel istances from the module edges by row
#'
#' @slot layout Layout object
#' @return glm_fit Fitted model
glm_pixel_dist_edge_row <- function(layout) {

  dist <- dist_edge_row(layout)
  pix_matrix <- layout$pix_matrix

  glm_fit <- perform_glm(as.vector(pix_matrix) ~ as.vector(dist))

  return(glm_fit)
}

#' Performs model fitting on the specified symbolic expression
#'
#' @slot symb_expr symbolic description of the linear predictor
perform_glm <- function(symb_expr, family = binomial(link = logit)) {

  #' @return glm_git fitted model
  glm_result <- glm(formula = symb_expr, family = family)

  return(glm_result)
}

#' Count number of damaged pixels overall and in different modules
#'
#' @slot layout Layout object
#' @return dead_stats Dead_Stats object
get_dead_stats <- function(layout) {

  ppp_dead <- ppp(layout$pix_dead[ , 1], layout$pix_dead[ , 2],
                  c(1, layout$detector_width), c(1, layout$detector_height))

  # returns the count of points in each quadrat
  quadrat_count_arr <- quadratcount(X = ppp_dead, nx = layout$module_col_n, ny=layout$module_row_n)

  # returns the count of points in each quadrat
  quadrat_count <- as.vector(quadrat_count_arr)

  dead_n <- length(as.vector(layout$pix_dead[ , 2]))
  module_n <- layout$module_col_n * layout$module_row_n
  avg_dead_mod <- round(dead_n / module_n, digits = 1)

  # Xi Squared Test
  Chisq <- chisq.test(x = quadrat_count, p = rep( 1 / module_n, module_n))
  Chisq_s <- Chisq$statistic
  Chisq_df <- Chisq$parameter
  Chisq_p <- Chisq$p.value

  dead_stats <- Dead_Stats(dead_n = dead_n, module_n = module_n,
                           avg_dead_mod = avg_dead_mod, Chisq_s = Chisq_s,
                           Chisq_df = Chisq_df, Chisq_p = Chisq_p)

  return(dead_stats)
}

#' Count number of damaged pixels overall and in different modules
#'
#' @slot layout Layout object
#' @return summary A string with damaged pixels overall statitics
dead_stats_summary <- function(layout) {

  dead_stats <- get_dead_stats(layout)

  summary <- paste("Total number of damaged pixels: ", dead_stats$dead_n, "\n", "")
  summary <- paste(summary, "Total number of modules: ", dead_stats$module_n, "\n", "")
  summary <- paste(summary, "Average number of damaged pixels per module: ", dead_stats$avg_dead_mod, "\n", "")
  summary <- paste(summary, "\n", "")
  summary <- paste(summary, "Chi-Squared Test results:\n", "")
  summary <- paste(summary, "Xsq = ", dead_stats$Chisq_s, ", Xsq df = ", dead_stats$Chisq_df, ", Xsq p = ", dead_stats$Chisq_p, "\n", "")

  return(summary)
}

