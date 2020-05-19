context("Testing clumps")

test_that("plot events", {
  test_dir <- getwd()

  detector_name <- "PerkinElmerFull"

  perkin_detector <- create_detector(detector_name)

  # getting the dead (damaged) pixel data
  dead_path <- system.file("extdata", "PerkinElmer_Full", "BadPixelMap.bpm", "BadPixelMap_t1.bpm.xml", package = "detectorchecker")

  perkin_detector <- load_pix_matrix(detector = perkin_detector, file_path = dead_path)

  # output file
  test_out_path <- "perkin_clump_events.jpg"

  incl_event_list <- list(1, 2, 3, 4, 5, 6, 7, 8)

  detector_events <- find_clumps(perkin_detector)

  plot_events(detector_events, file_path = test_out_path, caption = FALSE, incl_event_list = incl_event_list)

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)
})

test_that("plot module events", {
  test_dir <- getwd()

  detector_name <- "PerkinElmerFull"

  perkin_detector <- create_detector(detector_name)

  # getting the dead (damaged) pixel data
  dead_path <- system.file("extdata", "PerkinElmer_Full", "BadPixelMap.bpm", "BadPixelMap_t1.bpm.xml", package = "detectorchecker")

  perkin_detector <- load_pix_matrix(detector = perkin_detector, file_path = dead_path)

  # output file
  test_out_path <- "perkin_clump_module_events.jpg"

  incl_event_list <- list(1, 2, 3, 4, 5, 6, 7, 8)

  row_ <- 2
  col_ <- 3

  detector_events <- find_clumps(perkin_detector, row = row_, col = col_)

  plot_module_events(detector_events,
    file_path = test_out_path, row = row_, col = col_, caption = FALSE,
    incl_event_list = incl_event_list
  )

  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())

  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)
})

test_that("plot module events by type", {
  test_dir <- getwd()

  detector_name <- "PerkinElmerFull"

  perkin_detector <- create_detector(detector_name)

  # getting the dead (damaged) pixel data
  dead_path <- system.file("extdata", "PerkinElmer_Full", "BadPixelMap.bpm", "BadPixelMap_t1.bpm.xml", package = "detectorchecker")

  perkin_detector <- load_pix_matrix(detector = perkin_detector, file_path = dead_path)


  incl_event_list <- list(1, 2, 3, 4, 5, 6, 7, 8)

  row_ <- 2
  col_ <- 3

  detector_events <- find_clumps(perkin_detector, row = row_, col = col_)


  # output file
  test_out_path <- "perkin_events_density.jpg"
  plot_events_density(detector_events, file_path = test_out_path, row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)
  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())
  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # output file
  test_out_path <- "perkin_events_arrows.jpg"
  plot_events_arrows(detector_events, file_path = test_out_path, row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)
  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())
  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # output file
  test_out_path <- "perkin_events_angles.jpg"
  plot_events_angles(detector_events, file_path = test_out_path, row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)
  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())
  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)

  # output file
  test_out_path <- "perkin_events_kfg.jpg"
  plot_events_kfg(detector_events, file_path = test_out_path, func = "K", row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)
  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())
  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)


  # output file
  test_out_path <- "perkin_events_kfg_Kinhom.jpg"
  plot_events_kfg(detector_events, file_path = test_out_path, func = "Kinhom", row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)
  # Check whether the file was created
  expect_that(file.exists(test_out_path), is_true())
  # Removing the test output file
  if (file.exists(test_out_path)) file.remove(test_out_path)
})

test_that("correct clumps", {

  detector_name <- "PerkinElmerFull"

  perkin_detector <- create_detector(detector_name)

  # getting the dead (damaged) pixel data
  dead_path <- system.file("extdata", "PerkinElmer_Full", "BadPixelMap.bpm", "BadPixelMap_t1.bpm.xml", package = "detectorchecker")

  perkin_detector <- load_pix_matrix(detector = perkin_detector, file_path = dead_path)

  # output file
  test_out_path <- "perkin_clump_events.jpg"

  incl_event_list <- list(1, 2, 3, 4, 5, 6, 7, 8)

  plot_events(perkin_detector, file_path = test_out_path, caption = FALSE, incl_event_list = incl_event_list)
  expect_that(file.exists(test_out_path), is_true())
  if (file.exists(test_out_path)) file.remove(test_out_path)

  detector_events <- detectorchecker::find_clumps(perkin_detector)
  detectorchecker::plot_events(detector_events, file_path = test_out_path, caption = FALSE, incl_event_list = incl_event_list)
  expect_that(file.exists(test_out_path), is_true())
  if (file.exists(test_out_path)) file.remove(test_out_path)

  detector_events <- detectorchecker::find_clumps(perkin_detector, row = 1, col = 1)
  detectorchecker::plot_events(detector_events, file_path = test_out_path, caption = FALSE, incl_event_list = incl_event_list)
  expect_that(file.exists(test_out_path), is_true())
  if (file.exists(test_out_path)) file.remove(test_out_path)

})
