#' A function to calculate pixel distances from edges
#'
#' @slot layout The name of the layout to be used
#' @slot file Path(s) to the file(s) containing dead pixel information
load_module <- function(layout_name = NA, file = NA) {

  # Initial checks
  if (is.na(layout_name)) {
    stop("Layout has not been specified.")
  }

  # if (is.na(file)) {
  #   stop("File(s) regarding dead pixels have(s) not been specified.")
  # }

  # create the named layout
  layout <- create_module(layout_name)

  # check whether we accept the file(s)
  file_cnt <- length(file)

  if (file_cnt == 1) {
    file_extansion <- file_ext(file)

    if (file_extansion == "tif") {
      dead_data <- read_tiff(file_path = file, layout = layout)

    } else if (file_extansion == "xml") {
      dead_data <- read_xml(file_path = file, layout = layout)

    } else if (file_extansion == "hdf") {
      dead_data <- read_hdf(file_path = file, layout = layout)

    } else {
      stop(c("Undefined file extension: ", file_extansion, " [", file, "]"))

    }
  } else {
    # if we have a list of files, at the moment we assume that  they are in the
    #   hdf format.
    dead_data <- read_hdf(file_path = file, layout = layout)
  }

  return(dead_data)
}

#' A function to plot layout with damaged pixels
#'
#' @slot layout Layout object
#' @slot dead_data Matrix of damaged pixels coordinates
#' @slot file Output file path
plot_layout_damaged <- function(layout = NA, dead_data = NA, file = NA) {

  dirOut <- getwd()

  ppp_dead <- ppp(dead_data[ , 1], dead_data[ , 2],
                  c(1, layout$detector_width), c(1, layout$detector_height))

  pdf(file)

  ppp_edges_col <- create_ppp_edges_col(layout)
  ppp_edges_row <- create_ppp_edges_row(layout)

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
