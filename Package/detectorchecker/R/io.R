library(tiff)
library(h5)

#' Reads in dead pixels from a tiff file and checks against the layout
#'
#' @slot file Path to the tiff file
#' @slot layout Layout object
read_tiff <- function(file = NA, layout = NA) {

  # reading in the data
  tiff_data <- readTIFF(file, as.is = TRUE)

  # first consistency check: Detector dimensions okay?
  if (layout$detector_height != dim(tiff_data)[2]) {
    stop("Error: Number of rows in row data file (tif) incorrect.
         Please check the file and check if your Layout parameters match your damaged pixel data.")
  }

  if (layout$detector_width != dim(tiff_data)[1]) {
    stop("Error: Number of columns in row data file (tif) incorrect.
         Please check the file and check if your Layout parameters match your damaged pixel data.")
  }

  # Check range of T.
  # Assumption: max values in T are dead pixels, others are fine (only two categories).
  # If not binary, transform into a binary matrix Tbin.
  table(as.vector(tiff_data))
  bins <- round(tiff_data / max(tiff_data))
  table(as.vector(bins))

  # Matrix of damaged pixels coordinates
  dead <- which(bins == 1, arr.ind = tiff_data)

  # The first col of dead (dead[ , 1]) corresponds to the detector width dimension (col in Layout).
  # The second col of dead (dead[ , 2]) corresponds to the detector height dimension (row in Layout)

  colnames(dead) <- c("col", "row")

  return(dead)
}

#' Reads in multiple hdf files
#'
#' @slot files A list of paths to hdf files. Must be in the correct order.
#' @return data combined dataset
read_hdf <- function(file_list = NA) {
  data <- NA

  # Reading in multiple hdf files at the same time
  if (is.vector(file_list)) {

    file_cnt <- 0
    for(file in file_list) {

      data_file <- h5file(file, mode = 'a')

      if (file_cnt >= 1) {
        data <- c(data, data_file)
      } else {
        data <- data_file
      }

      file_cnt <- file_cnt + 1
    }

  # Reading single hdf file at the same time
  } else if (!is.na(file_list)) {
    data <- h5file(file, mode = 'a')
  }

  return(data)
}

