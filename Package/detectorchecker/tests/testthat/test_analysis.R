context("Analysis")

test_that("Testing read in data", {

  test_dir <- getwd()

  # This shoudl be a successful upload
  test_path <- file.path(test_dir, "dead_pix", "Pilatus", "badpixel_mask.tif")

  load_module(layout="Pilatus", file=test_path)

  expect_equal(TRUE, TRUE)
})
