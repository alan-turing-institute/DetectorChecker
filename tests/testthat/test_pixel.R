context("Pixel analysis functions")

test_that("Distance analysis functions", {

  # PerkinElmerFull detector unit test
  detector <- PerkinElmerFull_Detector()

  # TODO: Check the following
  # This is not perfect and maybe we can come up with a better way to compare to
  #   chcek. We compare the calculted and expected traces to make sure that
  #   the functions are behaving correctly.
  # Testing distances from corners

  # ----------------------------------------------------------------------------
  # Euclidean distances from the centre
  dist <- pixel_dist_ctr_eucl(detector)

  # Expected value for the PerkinElmerFull detector
  expect_equal(.tr(dist), 1414213.562)

  # ----------------------------------------------------------------------------
  # Parallel maxima distances from the centre
  dist <- pixel_dist_ctr_linf(detector)

  # Expected value for the PerkinElmerFull detector
  expect_equal(.tr(dist), 1000000)

  # ----------------------------------------------------------------------------
  # Pixel distances from corners
  dist <- dist_corner(detector)

  # Expected value for the PerkinElmerFull detector
  expect_equal(.tr(dist), 1412799.349)

  # # ----------------------------------------------------------------------------
  # # Pixel distances from the module edges by column
  # dist <- dist_edge_col(detector)
  #
  # # Expected value for the PerkinElmerFull detector
  # expect_equal(.tr(dist), 100.00)
  #
  # # ----------------------------------------------------------------------------
  # # Pixel distances from the module edges by row
  # dist <- dist_edge_row(detector)
  #
  # # Expected value for the PerkinElmerFull detector
  # expect_equal(.tr(dist), 100.00)
  #
  # # ----------------------------------------------------------------------------
  # # Pixel distances from the module edges by column
  # dist <- dist_edge_min(detector)
  #
  # # Expected value for the PerkinElmerFull detector
  # expect_equal(.tr(dist), 100.00)
  #
  # # ----------------------------------------------------------------------------
})

test_that("Removing high density clusters", {

  detector_name <- "Pilatus"
  pilatus_detector <- create_module(detector_name)
  
  dead_path <- system.file(  "extdata", "Pilatus", "badpixel_mask.tif", package = "detectorchecker")
  
  # Load pixel matrix
  pilatus_detector <- load_pix_matrix(detector = pilatus_detector, file_path = dead_path)

  # Remove high density clusters
  pilatus_detector_modified <- remove_high_density_cluster(pilatus_detector)


  pilatus_detector_detector_labs <- labels(pilatus_detector)

  # Attributes of the detector that may have changed
  potential_change_labs <- c("pix_matrix", "pix_dead", "dead_stats", "pix_dead_modules", "clumps" )
               
  # Attributes that should be identical
  matching_labs <- pilatus_detector_detector_labs[!(pilatus_detector_detector_labs %in% potential_change_labs)]
  
  # Check identical attributes
  for (lab in matching_labs){
    expect_equal(pilatus_detector[lab], pilatus_detector_modified[lab])
  }

})

context("Plotting pixel analysis")

test_that("Plotting pixel analysis", {
  test_out_fmt <- "jpg"
  test_out_dir <- getwd()

  # PerkinElmerFull detector unit test
  detector <- PerkinElmerFull_Detector()

  # ----------------------------------------------------------------------------
  # Euclidean distances from the centre

  test_out_name <- "unittest_pixel_ctr_eucl"
  test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep = "."))

  plot_pixel_ctr_eucl(detector, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # ----------------------------------------------------------------------------
  # Parallel maxima distances from the centre

  test_out_name <- "unittest_pixel_ctr_linf"
  test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep = "."))

  plot_pixel_ctr_linf(detector, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # ----------------------------------------------------------------------------
  # Pixel distances from corners

  test_out_name <- "unittest_pixel_dist_corner"
  test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep = "."))

  plot_pixel_dist_corner(detector, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # ----------------------------------------------------------------------------
  # Pixel distances from the module edges by column

  test_out_name <- "unittest_pixel_dist_edge_col"
  test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep = "."))

  plot_pixel_dist_edge_col(detector, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # ----------------------------------------------------------------------------
  # Pixel distances from the module edges by row

  test_out_name <- "unittest_pixel_dist_edge_row"
  test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep = "."))

  plot_pixel_dist_edge_row(detector, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # ----------------------------------------------------------------------------
  # Pixel distances from the module edges

  test_out_name <- "unittest_pixel_dist_edge"
  test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep = "."))

  plot_pixel_dist_edge(detector, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)
})
