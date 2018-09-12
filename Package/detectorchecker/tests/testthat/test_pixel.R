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

  # ----------------------------------------------------------------------------
  # Pixel distances from the module edges by column
  dist <- dist_edge_col(layout)

  # Expected value for the PerkinElmerFull layout
  expect_equal(tr(dist), 100.00)

  # ----------------------------------------------------------------------------
  # Pixel distances from the module edges by row
  dist <- dist_edge_row(layout)

  # Expected value for the PerkinElmerFull layout
  expect_equal(tr(dist), 100.00)

  # ----------------------------------------------------------------------------
  # Pixel distances from the module edges by column
  dist <- dist_edge_min(layout)

  # Expected value for the PerkinElmerFull layout
  expect_equal(tr(dist), 100.00)

  # ----------------------------------------------------------------------------
})
