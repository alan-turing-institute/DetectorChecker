context("Testing reading in the dead pixel data and visualizing the layout: Pilatus")

test_that("Pilatus", {

  test_dir <- getwd()

  layout_name <- "Pilatus"

  pilatus_layout <- create_module(layout_name)

  # getting the dead (damaged) pixel data
  dead_path <- file.path(test_dir, "dead_pix", "Pilatus",
                         "badpixel_mask.tif")

  pilatus_layout <- load_pix_matrix(layout = pilatus_layout, file_path = dead_path)

  # output file
  test_out_path <- "pilatus_damaged.jpg"

  # Visualizing damaged pixels
  plot_layout_damaged(layout = pilatus_layout, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)
})

context("Testing reading in the dead pixel data and visualizing the layout: Perkin Elmer")

test_that("Perkin Elmer", {
  test_dir <- getwd()

  layout_name <- "PerkinElmerFull"

  perkinelmerfull_layout <- create_module(layout_name)

  # getting the dead (damaged) pixel data
  dead_path <- file.path(test_dir, "dead_pix", "PerkinElmer",
                         "BadPixelMap_0.bpm", "BadPixelMap.bpm.xml")

  perkinelmerfull_layout <- load_pix_matrix(layout = perkinelmerfull_layout, file_path = dead_path)

  # output file
  test_out_path <- "perkinelmerfull_damaged.pdf"

  # Visualizing damaged pixels
  plot_layout_damaged(layout = perkinelmerfull_layout, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)
})

context("Testing reading in the dead pixel data and visualizing the layout: Excalibur")

test_that("Excalibur - mutiple files", {
  test_dir <- getwd()

  layout_name <- "Excalibur"

  excalibur_layout <- create_module(layout_name)

  # getting the dead (damaged) pixel data
  dead_paths <- file.path(test_dir, "dead_pix", "Excalibur",
                          "pixelmask.fem1.hdf")

  dead_paths <- c(dead_paths, file.path(test_dir, "dead_pix", "Excalibur",
                                        "pixelmask.fem2.hdf"))

  dead_paths <- c(dead_paths, file.path(test_dir, "dead_pix", "Excalibur",
                                        "pixelmask.fem3.hdf"))

  dead_paths <- c(dead_paths, file.path(test_dir, "dead_pix", "Excalibur",
                                        "pixelmask.fem4.hdf"))

  dead_paths <- c(dead_paths, file.path(test_dir, "dead_pix", "Excalibur",
                                        "pixelmask.fem5.hdf"))

  dead_paths <- c(dead_paths, file.path(test_dir, "dead_pix", "Excalibur",
                                        "pixelmask.fem6.hdf"))

  excalibur_layout <- load_pix_matrix(layout = excalibur_layout, file_path = dead_paths)

  # output file
  test_out_path <- "Excalibur_damaged.pdf"

  # Visualizing damaged pixels
  plot_layout_damaged(layout = excalibur_layout, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)
})

context("Model fitting")

# test_that("Pilatus", {
#   test_dir <- getwd()
#
#   layout <- create_module("Pilatus")
#
#   # getting the dead (damaged) pixel data
#   dead_path <- file.path(test_dir, "dead_pix", "Pilatus", "badpixel_mask.tif")
#
#   layout <- load_pix_matrix(layout = layout, file_path = dead_path)
#
#   # Euclidean distances from the centre
#   glm_fit <- glm_pixel_ctr_eucl(layout)
#
#   print(summary(glm_fit))
#
#   expect_that(TRUE, is_true())
# })
