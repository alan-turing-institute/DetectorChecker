#' A function to plot layout with damaged pixels
#'
#' @slot layout Layout object
#' @slot pix_dead Matrix of damaged pixels coordinates
#' @slot file_path Output file path
plot_layout_damaged <- function(layout, pix_dead, file_path) {

  dirOut <- getwd()

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

#' Performs model fitting on the specified symbolic expression
#'
#' @slot symb_expr symbolic description of the linear predictor
perform_glm <- function(symb_expr, family = binomial(link = logit)) {

  #' @return glm_git fitted model
  glm_result <- glm(formula = symb_expr, family = family)

  print(summary(glm_result))
}
