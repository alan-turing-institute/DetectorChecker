# These tests are duplicates. The functionality is tested in test_analysis.R too.

context("I/O functions")

test_that("TIFF", {
  test_dir <- getwd()

  # Pilatus detector unit test
  detector <- Pilatus_Detector()

  # TIFF file
  test_path <- system.file(  "extdata", "Pilatus", "badpixel_mask.tif", package = "detectorchecker")  

  # pixel matrix, with dead pixels flagged
  detector <- load_pix_matrix(detector = detector, file_path = test_path)

  expect_equal(dim(detector$pix_matrix)[1], 2527)
  expect_equal(dim(detector$pix_matrix)[2], 2463)

  expect_equal(dim(detector$pix_dead)[1], 1161)
  expect_equal(dim(detector$pix_dead)[2], 2)
})

test_that("HDF", {
  test_dir <- getwd()
  
  # Excalibur detector unit test
  detector <- Excalibur_Detector()

  test_path1 <- system.file(  "extdata", "Excalibur", "pixelmask.fem1.hdf", package = "detectorchecker")  
  test_path2 <- system.file(  "extdata", "Excalibur", "pixelmask.fem2.hdf", package = "detectorchecker")
  test_path3 <- system.file(  "extdata", "Excalibur", "pixelmask.fem3.hdf", package = "detectorchecker")
  test_path4 <- system.file(  "extdata", "Excalibur", "pixelmask.fem4.hdf", package = "detectorchecker")
  test_path5 <- system.file(  "extdata", "Excalibur", "pixelmask.fem5.hdf", package = "detectorchecker")
  test_path6 <- system.file(  "extdata", "Excalibur", "pixelmask.fem6.hdf", package = "detectorchecker")

  test_list <- c(test_path1, test_path2, test_path3, test_path4, test_path5, test_path6)

  detector <- load_pix_matrix(detector = detector, file_path = test_list)

  # width
  expect_equal(dim(detector$pix_matrix)[1], 2048)

  # height
  expect_equal(dim(detector$pix_matrix)[2], 1536)
})

test_that("XML", {
  # Pilatus detector unit test
  detector <- PerkinElmerFull_Detector()

  test_dir <- getwd()

  test_path <- system.file(  "extdata", "PerkinElmer_Full", "BadPixelMap.bpm", "BadPixelMap_t1.bpm.xml", package = "detectorchecker")
  
  # file.path(
  #   test_dir, "dead_pix", "PerkinElmer",
  #   "BadPixelMap_0.bpm",
  #   "BadPixelMap.bpm.xml"
  # )

  detector <- load_pix_matrix(detector = detector, file_path = test_path)

  # width
  expect_equal(dim(detector$pix_matrix)[1], 2000)

  # height
  expect_equal(dim(detector$pix_matrix)[2], 2000)
})
