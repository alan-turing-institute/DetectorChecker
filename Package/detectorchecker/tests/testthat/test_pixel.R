context("Pixel analysis functions")

test_that("Pixel parallel maxima distances from the centre", {

  # PerkinElmerFull layout unit test
  layout <- PerkinElmerFull_Layout()

  # This is not perfect and maybe we can come up with a better way to check the
  #   matrices. We compare the calculted and expected traces to make sure that
  #   the functions behaving correctly.

  # Testing distances from corners
  dist <- pixel_dist_ctr_linf(layout)

  print(paste(format(tr(dist), digits=20)))

  # Expected value for the PerkinElmerFull layout
  #expect_equal(tr(dist), 1412799)
})

test_that("Pixel distances from corners", {

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

# print(dist, digits=10)


