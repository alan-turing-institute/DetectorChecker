context("Pixel object creation")

test_that("Pixel analysis functions", {

  # PerkinElmerFull layout unit test
  layout <- PerkinElmerFull_Layout()

  # This is not perfect and maybe we can come up with a better way to check the
  #   matrices. We compare the calculted and expected traces to make sure that
  #   the functions behaving correctly.

  # Testing distances from corners
  dist <- pixel_dist_corner(layout)

  # Expected value for the PerkinElmerFull layout
  expect_equal(tr(dist), 1412799.349)

})

