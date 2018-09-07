context("Layout object creation")

test_that("Initialization of layout objects", {

  layout <- Default_Layout()
  expect_equal(layout$name, "Default")

  # Excalibur layout unit test
  layout <- Excalibur_Layout()
  expect_equal(layout$name, "Excalibur")
  # TODO: fix the inconsistancy
  #Layout.consistancyCheck(excalibur_layout)

  # Perkinelmer layout unit test
  layout <- PerkinElmerFull_Layout()
  expect_equal(layout$name, "PerkinElmerFull")
  Layout.consistancyCheck(layout)

  # PerkinElmerCropped1600 layout unit test
  layout <- PerkinElmerCropped1600_Layout()
  expect_equal(layout$name, "PerkinElmerCropped1600")
  Layout.consistancyCheck(layout)

  # PerkinElmerRefurbished layout unit test
  layout <- PerkinElmerRefurbished_Layout()
  expect_equal(layout$name, "PerkinElmerRefurbished")
  Layout.consistancyCheck(layout)
})
