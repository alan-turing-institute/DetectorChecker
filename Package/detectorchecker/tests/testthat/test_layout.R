context("Layout object creation")

test_that("Initialization of layout objects", {

  default_layout <- Default_Layout()
  expect_equal(default_layout$name, "Default")

  # Excalibur layout unit test
  excalibur_layout <- Excalibur_Layout()
  expect_equal(excalibur_layout$name, "Excalibur")
  Layout.consistancyCheck(excalibur_layout)

  # Perkinelmer layout unit test
  perkinelmerfull_layout <- PerkinElmerFull_Layout()
  expect_equal(perkinelmerfull_layout$name, "PerkinElmerFull")
  Layout.consistancyCheck(perkinelmerfull_layout)
})
