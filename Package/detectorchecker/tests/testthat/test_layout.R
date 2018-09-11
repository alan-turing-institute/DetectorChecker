context("Layout object creation")

test_that("Initialization of layout objects", {

  # We do not need to test the default layout
  # layout <- Default_Layout()
  # expect_equal(layout$name, "Default")

  # TODO: fix the inconsistancy
  # Excalibur layout unit test
  # layout <- Excalibur_Layout()
  # expect_equal(layout$name, "Excalibur")
  # Layout.consistancyCheck(excalibur_layout)

  # Perkinelmer layout unit test
  layout <- PerkinElmerFull_Layout()
  expect_equal(layout$name, "PerkinElmerFull")
  layout_consist_check(layout)

  # PerkinElmerCropped1600 layout unit test
  layout <- PerkinElmerCropped1600_Layout()
  expect_equal(layout$name, "PerkinElmerCropped1600")
  layout_consist_check(layout)

  # PerkinElmerRefurbished layout unit test
  layout <- PerkinElmerRefurbished_Layout()
  expect_equal(layout$name, "PerkinElmerRefurbished")
  layout_consist_check(layout)
})

context("Deriving additional layout elements")

test_that("derive_plot", {

  # PerkinElmerFull_Layout layout as a test case
  layout <- PerkinElmerFull_Layout()

  # Deriving additional elements
  layout <- derive_layout(layout)

  # Test values for the PerkinElmerFull layout
  test_horiz_values <- matrix(c(1, 105, 233, 361, 489, 617, 745, 873, 1001,
                                1129, 1257, 1385, 1513, 1641, 1769, 1897, 104,
                                232, 360, 488, 616, 744, 872, 1000, 1128, 1256,
                                1384, 1512, 1640, 1768, 1896, 2000),
                              nrow = 2, ncol = 16, byrow = TRUE)

  dimnames(test_horiz_values)[[1]] <- c("left", "right")

  test_vert_values <- matrix(c(1, 1001, 1000, 2000),
                             nrow = 2, ncol = 2, byrow = TRUE)

  dimnames(test_vert_values)[[1]] <- c("top", "bottom")

  expect_equal(layout$module_edges_col, test_horiz_values)
  expect_equal(layout$module_edges_row, test_vert_values)
})

context("Plotting layout")

test_that("plot layout", {

  test_out_name <- "unittest_layout"
  test_out_fmt <- "pdf"
  test_out_dir <- getwd()
  test_out_path <- file.path(test_out_dir, paste(test_out_name, test_out_fmt, sep="."))

  # PerkinElmerFull layout unit test
  layout <- PerkinElmerFull_Layout()

  # Plotting layout in the cwd
  plot_layout(layout, file=test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

})
