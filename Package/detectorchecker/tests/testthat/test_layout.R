context("Layout object creation")

test_that("Initialization of layout objects", {

  default.layout <- Layout.Default()
  expect_equal(default.layout$name, "Default")

  # Excalibur layout unit test
  excalibur.layout <- Layout.Excalibur()
  expect_equal(excalibur.layout$name, "Excalibur")
  Layout.consistancyCheck(excalibur.layout)

  # Perkinelmer layout unit test
  perkinelmerfull.layout <- Layout.PerkinElmerFull()
  expect_equal(perkinelmerfull.layout$name, "PerkinElmerFull")
  Layout.consistancyCheck(perkinelmerfull.layout)
})
