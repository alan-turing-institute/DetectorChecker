context("Plotting detector")

test_that("Availability", {
  expect_equal(check_detector_avail("Pilatus"), TRUE)
})

context("Detector object creation")

test_that("Initialization of detector objects", {

  # We do not need to test the default detector
  # detector <- Default_Detector()
  # expect_equal(detector$name, "Default")

  # TODO: fix the inconsistancy
  # Excalibur detector unit test
  detector <- Excalibur_Detector()
  expect_equal(detector$name, "Excalibur")
  detector_consist_check(detector)

  # Perkinelmer detector unit test
  detector <- PerkinElmerFull_Detector()
  expect_equal(detector$name, "PerkinElmerFull")
  detector_consist_check(detector)

  # PerkinElmerCropped1600 detector unit test
  detector <- PerkinElmerCropped1600_Detector()
  expect_equal(detector$name, "PerkinElmerCropped1600")
  detector_consist_check(detector)

  # PerkinElmerRefurbished detector unit test
  detector <- PerkinElmerRefurbished_Detector()
  expect_equal(detector$name, "PerkinElmerRefurbished")
  detector_consist_check(detector)

  # Pilatus detector unit test
  detector <- Pilatus_Detector()
  expect_equal(detector$name, "Pilatus")
  detector_consist_check(detector)
})

context("Deriving additional detector elements")

test_that("derive_plot", {

  # PerkinElmerFull_Detector detector as a test case
  detector <- PerkinElmerFull_Detector()

  # Test values for the PerkinElmerFull detector
  test_horiz_values <- matrix(c(
    1, 105, 233, 361, 489, 617, 745, 873, 1001,
    1129, 1257, 1385, 1513, 1641, 1769, 1897, 104,
    232, 360, 488, 616, 744, 872, 1000, 1128, 1256,
    1384, 1512, 1640, 1768, 1896, 2000
  ),
  nrow = 2, ncol = 16, byrow = TRUE
  )

  dimnames(test_horiz_values)[[1]] <- c("left", "right")

  test_vert_values <- matrix(c(1, 1001, 1000, 2000),
    nrow = 2, ncol = 2, byrow = TRUE
  )

  dimnames(test_vert_values)[[1]] <- c("top", "bottom")

  expect_equal(detector$module_edges_col, test_horiz_values)
  expect_equal(detector$module_edges_row, test_vert_values)
})

context("Plotting detector")

test_that("plot detector", {
  test_out_name <- "unittest_detector"
  test_out_fmt <- "jpeg"
  test_out_dir <- getwd()
  test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep = "."))

  # PerkinElmerFull detector unit test
  detector <- PerkinElmerFull_Detector()

  # Plotting detector in the cwd
  plot_detector(detector = detector, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)
})
