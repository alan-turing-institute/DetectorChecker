library(tiff)
library(h5)
library(tools)

#' Reads in dead pixels from a tiff file and checks against the layout
#'
#' @slot file Path to the tiff file
#' @slot layout Layout object
read_tiff <- function(file_path = NA, layout = NA) {

  # reading in the data
  tiff_data <- readTIFF(file_path, as.is = TRUE)

  # first consistency check: Detector dimensions okay?
  if (layout$detector_height != dim(tiff_data)[2]) {
    stop("Error: Number of rows in row data file (tif) incorrect.
         Please check the file and check if your Layout parameters match your damaged pixel data.")
  }

  if (layout$detector_width != dim(tiff_data)[1]) {
    stop("Error: Number of columns in row data file (tif) incorrect.
         Please check the file and check if your Layout parameters match your damaged pixel data.")
  }

  # Check range of tiff_data
  # Assumption: max values in tiff_data are dead pixels, others are fine (only two categories).
  # If not binary, transform into a binary matrix bins
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

#' Reads in dead pixels from an xml file and checks against the layout
#'
#' @slot file Path to the xml file
#' @slot layout Layout object
read_xml <- function(file_path = NA, layout = NA) {

  # decode bad pixel map list from xml file (pedestrian way...)
  xml_data <- suppressWarnings(matrix(scan(file_path,
                                           what = c("", "", "", "", "", ""),
                                           skip = 3), ncol = 6, byrow = TRUE))

  # Applying data treatment
  # But(!), warning is just because of end of file, hence just remove last two lines and it works.
  nrows <- nrow(xml_data) - 2

  xml_data_modi <- xml_data[1:nrows, ncol = c(2, 3)]

  # these are coordinates of dead pixels, with
  # xml_data_modi[, 1] for detector cols (width) and xml_data_modi[, 2] for detector rows (height)
  xml_data_modi <- apply(xml_data_modi, 2, extract_number)

  ## Convert into dead pixel matrix
  dead_data <- matrix(NA, nrow = dim(xml_data_modi)[1], ncol = 2)

  colnames(dead_data) <- c("col", "row")
  dead_data[ , 1] <- xml_data_modi[ , 1] + 1
  dead_data[ , 2] <- xml_data_modi[ , 2] + 1

  return(dead_data)
}

# TODO: Define the function
#' Function that does?
#'
#' @slot s String expression?
#' @return x Numeric value
extract_number <- function(s) {

  v <- substring(s, 4, 4 + nchar(s) - 5)
  v <- as.numeric(v)

  return(v)
}

#' Creates a dead pixel mask
#'
#' @slot layout Layout object
#' @slot dead_data Dead pixel data
#' @return mask Dead pixel mask
dead_pix_mask <- function(layout, dead_data) {

  mask <- matrix(0, nrow = layout$detector_height, ncol = layout$detector_width)

  for (i in c(1:dim(dead_data)[1])) {
    mask[dead_data[i, ]] <- 1
  }

  return(mask)
}
