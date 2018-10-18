#' Reads in tiff file and returns a pixel matrix
#'
#' @param layout Layout object
#' @param file_path Path to the tiff file
#' @return Pixel matrix with dead pixels flagged with 1
matrix_from_tiff <- function(layout, file_path) {

  # reading in the data
  tiff_data <- tiff::readTIFF(file_path, as.is = TRUE)

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
  # Transform into a binary matrix dead_matrix
  table(as.vector(tiff_data))

  pix_matrix <- round(tiff_data / max(tiff_data))

  table(as.vector(pix_matrix))

  return(pix_matrix)
}

#' Reads in hdf file(s) and returns a pixel matrix
#'
#' @param layout Layout object
#' @param file_path A list of paths to hdf files. Must be in the correct order.
#' @return Data of a combined dataset from hdf files
matrix_from_hdf <- function(layout, file_path) {
  data <- NA
  hdf_data <- NA

  # Reading in multiple hdf files at the same time
  if (is.vector(file_path)) {

    file_cnt <- 0
    for(file in file_path) {

      data_file <- h5::h5file(file, mode = 'r')

      hdf_data_file <- h5::readDataSet(data_file[h5::list.datasets(data_file)])

      if (file_cnt > 0) {
        # The data is combined by rows!
        hdf_data <- rbind(hdf_data, hdf_data_file)

      } else {
        hdf_data <- hdf_data_file
      }

      h5::h5close(data_file)

      file_cnt <- file_cnt + 1
    }
  }

  # transposing the matrix
  pix_matrix <- t(hdf_data)

  if (layout$detector_width != dim(pix_matrix)[1]) {
    stop("Error: Number of columns in data file (hdf) incorrect.
         Please check the file and check if your Layout parameters match your damaged pixel data.")
  }

  # first consistency check: Detector dimensions okay?
  if (layout$detector_height != dim(pix_matrix)[2]) {
    stop("Error: Number of rows in data file (hdf) incorrect.
         Please check the file and check if your Layout parameters match your damaged pixel data.")
  }

  return(pix_matrix)
}

#' Reads in xml file and returns a pixel matrix
#'
#' @param layout Layout object
#' @param file_path Path to the xml file
#' @return Data from an xml file
matrix_from_xml <- function(layout, file_path) {

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
  xml_data_modi <- apply(xml_data_modi, 2, .extract_number)

  # Convert into dead pixel matrix
  pix_dead <- matrix(NA, nrow = dim(xml_data_modi)[1], ncol = 2)
  pix_dead[, 1] <- xml_data_modi[, 1] + 1
  pix_dead[, 2] <- xml_data_modi[, 2] + 1

  # TODO: This is not a good approach as we first read in pix_dead and convert it to
  #  pix_matrix. Later on pix_dead is recunstructed again from pix_matrix.
  pix_matrix <- matrix(0, nrow = layout$detector_width,
                       ncol = layout$detector_height)

  for (i in c(1:dim(pix_dead)[1])) {

    coord_x <- pix_dead[i, 1]
    coord_y <- pix_dead[i, 2]

    pix_matrix[coord_x, coord_y] <- 1
  }

  return(pix_matrix)
}

#' Internal function to convert string values to numbers
#'
#' @param s String expression?
#' @return Numeric value
.extract_number <- function(s) {

  v <- substring(s, 4, 4 + nchar(s) - 5)
  v <- as.numeric(v)

  return(v)
}

#' Creates a dead pixel mask
#'
#' @param layout Layout object
#' @param dead_data Dead pixel data
#' @return Dead pixel mask
dead_pix_mask <- function(layout, dead_data) {

  mask <- matrix(0, nrow = layout$detector_height, ncol = layout$detector_width)

  for (i in c(1:dim(dead_data)[1])) {
    mask[dead_data[i, ]] <- 1
  }

  return(mask)
}

#' A function to load pixel data
#'
#' @param layout The name of the layout to be used
#' @param file_path Path(s) to the file(s) containing dead pixel information
#' @return Layout object
load_pix_matrix <- function(layout, file_path) {

  pix_matrix <- NA

  # check the number of files
  file_cnt <- length(file_path)

  if (file_cnt == 1) {
    file_extansion <- tools::file_ext(file_path)

    if (file_extansion == "tif") {
      pix_matrix <- matrix_from_tiff(layout = layout, file_path = file_path)

    } else if (file_extansion == "xml") {
      pix_matrix <- matrix_from_xml(layout = layout, file_path = file_path)

    } else if (file_extansion == "hdf") {
      pix_matrix <- matrix_from_hdf(layout = layout, file_path = file_path)

    } else {
      stop(c("Undefined file extension: ", file_extansion, " [", file_path, "]"))
    }
  } else {
    # if we have a list of files, at the moment we assume that they are in the
    #   hdf format.
    pix_matrix <- matrix_from_hdf(file_path = file_path, layout = layout)
  }

  pix_dead <- dead_pix_coords(pix_matrix)

  layout$pix_matrix <- pix_matrix
  layout$pix_dead <- pix_dead

  # assigning a module to each dead pixels
  dead_modules <- .assign_module(layout)
  layout$pix_dead_modules <- dead_modules

  return(layout)
}

#' Starts the graphics device driver for producing graphics with respect to a
#'   chosen format
#'
#' @param file_path Output path with an extension
ini_graphics <- function(file_path) {
  # choosing output format
  file_extansion <- tools::file_ext(file_path)

  if ((file_extansion == "jpeg") || (file_extansion == "jpg")) {
    jpeg(file_path)

  } else if (file_extansion == "pdf") {
    pdf(file_path)

  } else {
    stop(c("Unknown output format: ", file_extansion, " [", file_path, "]\n",
           "Supported formats: jpeg, pdf"))
  }
}
