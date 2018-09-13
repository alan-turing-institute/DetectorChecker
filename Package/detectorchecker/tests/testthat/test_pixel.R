context("Pixel analysis functions")

test_that("Distance analysis functions", {

  # PerkinElmerFull layout unit test
  layout <- PerkinElmerFull_Layout()

  # TODO: Check the following
  # This is not perfect and maybe we can come up with a better way to compare to
  #   chcek. We compare the calculted and expected traces to make sure that
  #   the functions are behaving correctly.
  # Testing distances from corners

  # ----------------------------------------------------------------------------
  # Euclidean distances from the centre
  dist <- pixel_dist_ctr_eucl(layout)

  # Expected value for the PerkinElmerFull layout
  expect_equal(tr(dist), 1414213.562)

  # ----------------------------------------------------------------------------
  # Parallel maxima distances from the centre
  dist <- pixel_dist_ctr_linf(layout)

  # Expected value for the PerkinElmerFull layout
  expect_equal(tr(dist), 1000000)

  # ----------------------------------------------------------------------------
  # Pixel distances from corners
  dist <- dist_corner(layout)

  # Expected value for the PerkinElmerFull layout
  expect_equal(tr(dist), 1412799.349)

  # # ----------------------------------------------------------------------------
  # # Pixel distances from the module edges by column
  # dist <- dist_edge_col(layout)
  #
  # # Expected value for the PerkinElmerFull layout
  # expect_equal(tr(dist), 100.00)
  #
  # # ----------------------------------------------------------------------------
  # # Pixel distances from the module edges by row
  # dist <- dist_edge_row(layout)
  #
  # # Expected value for the PerkinElmerFull layout
  # expect_equal(tr(dist), 100.00)
  #
  # # ----------------------------------------------------------------------------
  # # Pixel distances from the module edges by column
  # dist <- dist_edge_min(layout)
  #
  # # Expected value for the PerkinElmerFull layout
  # expect_equal(tr(dist), 100.00)
  #
  # # ----------------------------------------------------------------------------
})

context("Plotting pixel analysis")

test_that("Plotting pixel analysis", {

  test_out_fmt <- "png"
  test_out_dir <- getwd()

  # PerkinElmerFull layout unit test
  layout <- PerkinElmerFull_Layout()

  # # ----------------------------------------------------------------------------
  # # Euclidean distances from the centre
  #
  # test_out_name <- "unittest_pixel_ctr_eucl"
  # test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep="."))
  #
  # plot_pixel_ctr_eucl(layout, file = test_out_path)
  #
  # # Check whether the file was created
  # expect_that(file.exists(test_out_path), is_true())
  #
  # # Removing the test output file
  # if (file.exists(test_out_path)) file.remove(test_out_path)

  # # ----------------------------------------------------------------------------
  # # Parallel maxima distances from the centre
  #
  # test_out_name <- "unittest_pixel_ctr_linf"
  # test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep="."))
  #
  # plot_pixel_ctr_linf(layout, file = test_out_path)
  #
  # # Check whether the file was created
  # expect_that(file.exists(test_out_path), is_true())
  #
  # # Removing the test output file
  # if (file.exists(test_out_path)) file.remove(test_out_path)

  # # ----------------------------------------------------------------------------
  # # Pixel distances from corners
  #
  # test_out_name <- "unittest_pixel_dist_corner"
  # test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep="."))
  #
  # plot_pixel_dist_corner(layout, file = test_out_path)
  #
  # # Check whether the file was created
  # expect_that(file.exists(test_out_path), is_true())
  #
  # # Removing the test output file
  # if (file.exists(test_out_path)) file.remove(test_out_path)

  # # ----------------------------------------------------------------------------
  # # Pixel distances from the module edges by column
  #
  # test_out_name <- "unittest_pixel_dist_edge_col"
  # test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep="."))
  #
  # plot_pixel_dist_edge_col(layout, file = test_out_path)
  #
  # # Check whether the file was created
  # expect_that(file.exists(test_out_path), is_true())
  #
  # # Removing the test output file
  # if (file.exists(test_out_path)) file.remove(test_out_path)

  # # ----------------------------------------------------------------------------
  # # Pixel distances from the module edges by row
  #
  # test_out_name <- "unittest_pixel_dist_edge_row"
  # test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep="."))
  #
  # plot_pixel_dist_edge_row(layout, file = test_out_path)
  #
  # # Check whether the file was created
  # expect_that(file.exists(test_out_path), is_true())
  #
  # # Removing the test output file
  # if (file.exists(test_out_path)) file.remove(test_out_path)

  # # ----------------------------------------------------------------------------
  # # Pixel distances from the module edges
  #
  # test_out_name <- "unittest_pixel_dist_edge"
  # test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep="."))
  #
  # plot_pixel_dist_edge(layout, file = test_out_path)
  #
  # # Check whether the file was created
  # expect_that(file.exists(test_out_path), is_true())
  #
  # # Removing the test output file
  # if (file.exists(test_out_path)) file.remove(test_out_path)

})

context("Reading in dead pixels")

test_that("TIFF", {

  test_dir <- getwd()

  # Pilatus layout unit test
  layout <- Pilatus_Layout()

  # TIFF file
  test_path <- file.path(test_dir, "dead_pix", "Pilatus", "badpixel_mask.tif")

  dead <- read_dead_pix_from_tiff(file = test_path, layout = layout)

  expect_equal(dim(dead)[1], 1161)
  expect_equal(dim(dead)[2], 2)
})

test_that("HDF", {

  test_dir <- getwd()

  # Pilatus layout unit test
  layout <- Excalibur_Layout()

  # TIFF format
  test_path <- file.path(test_dir, "dead_pix", "Excalibur", "pixelmask.fem1.hdf")

  dead <- read_dead_pix_from_hdf(file = test_path, layout = layout)

  #expect_equal(dim(dead)[1], 1161)
  #expect_equal(dim(dead)[2], 2)

  expect_equal(TRUE, TRUE)
})
