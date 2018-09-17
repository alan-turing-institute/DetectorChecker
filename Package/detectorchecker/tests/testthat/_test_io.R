context("I/O functions")

test_that("TIFF", {

  test_dir <- getwd()

  # Pilatus layout unit test
  layout <- Pilatus_Layout()

  # TIFF file
  test_path <- file.path(test_dir, "dead_pix", "Pilatus", "badpixel_mask.tif")

  dead <- read_tiff(file = test_path, layout = layout)

  expect_equal(dim(dead)[1], 1161)
  expect_equal(dim(dead)[2], 2)
})

test_that("HDF", {

  test_dir <- getwd()

  test_path1 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem1.hdf")
  test_path2 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem2.hdf")
  test_path3 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem3.hdf")
  test_path4 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem4.hdf")
  test_path5 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem5.hdf")
  test_path6 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem6.hdf")

  test_list <- c(test_path1, test_path2, test_path3, test_path4, test_path5, test_path6)

  data <- read_hdf(file_list = test_list)

  # Expect 6 files to be read
  expect_equal(length(data), 6)

  data <- read_hdf(file_list = test_path1)

  # Expect 1 file to be read
  expect_equal(length(data), 1)
})

test_that("XML", {

  test_dir <- getwd()

  test_path <- file.path(test_dir, "dead_pix", "PerkinElmer",
                         "BadPixelMap_0.bpm",
                         "BadPixelMap.bpm.xml")

  data <- read_xml(file_path = test_path)

  expect_equal(length(data), 18902)
})

