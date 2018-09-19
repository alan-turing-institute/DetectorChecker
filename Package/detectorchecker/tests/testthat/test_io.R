context("I/O functions")

test_that("TIFF", {

  test_dir <- getwd()

  # Pilatus layout unit test
  layout <- Pilatus_Layout()

  # TIFF file
  test_path <- file.path(test_dir, "dead_pix", "Pilatus", "badpixel_mask.tif")

  # pixel matrix, with dead pixels flagged
  pix_matrix <- matrix_from_tiff(file = test_path, layout = layout)

  expect_equal(dim(pix_matrix)[1], 2527)
  expect_equal(dim(pix_matrix)[2], 2463)

  # dead pixel coordinate table
  pix_dead <- dead_pix_coords(pix_matrix)

  expect_equal(dim(pix_dead)[1], 1161)
  expect_equal(dim(pix_dead)[2], 2)
})

test_that("HDF", {

  test_dir <- getwd()

  # Pilatus layout unit test
  layout <- Excalibur_Layout()

  test_path1 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem1.hdf")
  test_path2 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem2.hdf")
  test_path3 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem3.hdf")
  test_path4 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem4.hdf")
  test_path5 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem5.hdf")
  test_path6 <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem6.hdf")

  test_list <- c(test_path1, test_path2, test_path3, test_path4, test_path5, test_path6)

  pix_matrix <- read_hdf(layout, file_path = test_list)

  # width
  expect_equal(dim(pix_matrix)[1], 2048)

  # height
  expect_equal(dim(pix_matrix)[2], 1536)

  # pix_matrix <- read_hdf(layout, file_path = test_path1)
  #
  # # Expect 1 file to be read
  # expect_equal(length(pix_matrix), 1)
})

# test_that("XML", {
#
#   test_dir <- getwd()
#
#   test_path <- file.path(test_dir, "dead_pix", "PerkinElmer",
#                          "BadPixelMap_0.bpm",
#                          "BadPixelMap.bpm.xml")
#
#   data <- read_xml(file_path = test_path)
#
#   expect_equal(length(data), 18902)
# })
#
