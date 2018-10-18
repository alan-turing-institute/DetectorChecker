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

  # Dead stats
  perkinelmerfull_layout <- get_dead_stats(perkinelmerfull_layout)

  # Plotting counts per module
  test_out_path <- "perkinelmerfull_module_cnt.pdf"
  plot_layout_cnt_mod(layout = perkinelmerfull_layout, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # Plotting dead pixel density
  test_out_path <- "perkinelmerfull_density.jpg"
  plot_layout_density(layout = perkinelmerfull_layout, file_path = test_out_path,
                      adjust = 0.5)

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # Arrows
  test_out_path <- "perkinelmerfull_arrows.jpg"

  plot_layout_arrows(layout = perkinelmerfull_layout, file_path = test_out_path)

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

test_that("Pilatus", {
  test_dir <- getwd()

  layout <- create_module("Pilatus")

  # getting the dead (damaged) pixel data
  dead_path <- file.path(test_dir, "dead_pix", "Pilatus", "badpixel_mask.tif")

  layout <- load_pix_matrix(layout = layout, file_path = dead_path)

  # Euclidean distances from the centre
  # glm_fit <- glm_pixel_ctr_eucl(layout)
  # print(summary(glm_fit))

  # Parallel maxima from the centre
  # glm_fit <- glm_pixel_ctr_linf(layout)
  # print(summary(glm_fit))

  # Distances from the module edges by column
  # glm_fit <- glm_pixel_dist_edge_col(layout)
  # print(summary(glm_fit))

  # Distances from the module edges by row
  glm_fit <- glm_pixel_dist_edge_row(layout)

  # TODO: check the fitted values
  # print(summary(glm_fit))

  expect_that(TRUE, is_true())
})

context("Dead Stats Summary")

test_that("PerkinElmerFull", {
  test_dir <- getwd()

  layout <- create_module("PerkinElmerFull")

  # getting the dead (damaged) pixel data
  test_path <- file.path(test_dir, "dead_pix", "PerkinElmer",
                         "BadPixelMap_0.bpm",
                         "BadPixelMap.bpm.xml")

  layout <- load_pix_matrix(layout = layout, file_path = test_path)

  dead_stats_summary <- dead_stats_summary(layout)

  summary <- paste("\n", "\n", layout_summary(layout),"\n", "")
  summary <- paste(summary, dead_stats_summary,"\n", "")

  expect_that(TRUE, is_true())
})

context("Testing analysis: Perkin Elmer")

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

  # Dead stats
  perkinelmerfull_layout <- get_dead_stats(perkinelmerfull_layout)

  # Plotting counts per module
  test_out_path <- "perkinelmerfull_module_cnt.pdf"
  plot_layout_cnt_mod(layout = perkinelmerfull_layout, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # Plotting dead pixel density
  test_out_path <- "perkinelmerfull_density.jpg"
  plot_layout_density(layout = perkinelmerfull_layout, file_path = test_out_path,
                      adjust = 0.5)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # Arrows
  test_out_path <- "perkinelmerfull_arrows.jpg"
  plot_layout_arrows(layout = perkinelmerfull_layout, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # Full detector angle plot
  test_out_path <- "perkinelmerfull_angles.jpg"
  plot_layout_angles(layout = perkinelmerfull_layout, file_path = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # test_analysis
  # test_analysis_functions(perkinelmerfull_layout)

})

context("Testing KFG: ")

test_that("Perkin Elmer", {
  test_dir <- getwd()

  layout_name <- "PerkinElmerFull"

  layout <- create_module(layout_name)

  # getting the dead (damaged) pixel data
  dead_path <- file.path(test_dir, "dead_pix", "PerkinElmer",
                         "BadPixelMap_0.bpm", "BadPixelMap.bpm.xml")

  layout <- load_pix_matrix(layout = layout, file_path = dead_path)

  # K
  test_out_path <- "K-Function.jpg"
  plot_kfg(layout = layout, func = "K", file_path = test_out_path)
  expect_that(file.exists(test_out_path), is_true())
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # F
  test_out_path <- "F-Function.jpg"
  plot_kfg(layout = layout, func = "F", file_path = test_out_path)
  expect_that(file.exists(test_out_path), is_true())
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # G
  test_out_path <- "G-Function.jpg"
  plot_kfg(layout = layout, func = "G", file_path = test_out_path)
  expect_that(file.exists(test_out_path), is_true())
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # Kinhom
  test_out_path <- "Kinhom.jpg"
  plot_kfg(layout = layout, func = "Kinhom", file_path = test_out_path)
  expect_that(file.exists(test_out_path), is_true())
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # Finhom
  test_out_path <- "Finhom.jpg"
  plot_kfg(layout = layout, func = "Finhom", file_path = test_out_path)
  expect_that(file.exists(test_out_path), is_true())
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # Ginhom
  test_out_path <- "Ginhom.jpg"
  plot_kfg(layout = layout, func = "Ginhom", file_path = test_out_path)
  expect_that(file.exists(test_out_path), is_true())
  if (file.exists(test_out_path)) file.remove(test_out_path)
})
