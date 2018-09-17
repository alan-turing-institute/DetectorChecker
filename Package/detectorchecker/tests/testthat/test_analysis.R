context("Testing reading in the dead pixel data and visualizing the layout")

test_that("Pilatus", {

  test_dir <- getwd()

  layout_name <- "Pilatus"

  # This shoudl be a successful tif upload
  dead_path <- file.path(test_dir, "dead_pix", "Pilatus",
                         "badpixel_mask.tif")

  pilatus_layout <- create_module(layout_name)

  # geeting the dead (damaged) pixel data
  dead_data <- load_module(layout = layout_name, file = dead_path)

  # output file
  test_out_path <- "pilatus_damaged.pdf"

  # Plotting layout with damaged pixels
  plot_layout_damaged(layout = pilatus_layout, dead_data = dead_data,
                      file = test_out_path)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  # if (file.exists(test_out_path)) file.remove(test_out_path)

})
