## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(detectorchecker)

## ------------------------------------------------------------------------
available_detectors

## ------------------------------------------------------------------------
pilatus_detector <- create_module("Pilatus")

## ------------------------------------------------------------------------
plot_detector(pilatus_detector)

## ------------------------------------------------------------------------
plot_pixel_ctr_eucl(pilatus_detector)

## ------------------------------------------------------------------------
plot_pixel_ctr_linf(pilatus_detector)

## ------------------------------------------------------------------------
plot_pixel_dist_corner(pilatus_detector)

## ------------------------------------------------------------------------
plot_pixel_dist_edge_col(pilatus_detector)

## ------------------------------------------------------------------------
plot_pixel_dist_edge_row(pilatus_detector)

## ------------------------------------------------------------------------
plot_pixel_dist_edge(pilatus_detector)

## ------------------------------------------------------------------------
user_def_detector <- readin_detector("../inst/extdata/user-defined.dc")

plot_detector(user_def_detector)


## ------------------------------------------------------------------------
pilatus_detector <- create_module("Pilatus")

file_path <- "../inst/extdata/Pilatus/badpixel_mask.tif"

pilatus_detector <- load_pix_matrix(detector = pilatus_detector, file_path = file_path)

plot_detector_damaged(detector = pilatus_detector, caption = FALSE)

plot_detector_module_damaged(detector = pilatus_detector, col = 4, row = 5, caption = FALSE)

## ------------------------------------------------------------------------
detector_perkin <- create_module("PerkinElmerFull")

file_path <- "../inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml"

detector_perkin <- load_pix_matrix(detector = detector_perkin, file_path = file_path)

plot_detector_damaged(detector = detector_perkin)

## ------------------------------------------------------------------------
detector_exc <- create_module("Excalibur")

file_path <- c("../inst/extdata/Excalibur/pixelmask.fem1.hdf", 
               "../inst/extdata/Excalibur/pixelmask.fem2.hdf",
               "../inst/extdata/Excalibur/pixelmask.fem3.hdf",
               "../inst/extdata/Excalibur/pixelmask.fem4.hdf",
               "../inst/extdata/Excalibur/pixelmask.fem5.hdf",
               "../inst/extdata/Excalibur/pixelmask.fem6.hdf")

detector_exc <- load_pix_matrix(detector = detector_exc, file_path = file_path)

plot_detector_damaged(detector = detector_exc)

## ------------------------------------------------------------------------
detector_perkin <- create_module("PerkinElmerFull")

file_path <- "../inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml"

detector_perkin <- load_pix_matrix(detector = detector_perkin, file_path = file_path)

plot_detector_density(detector = detector_perkin, adjust = 0.5)

plot_detector_density(detector = detector_perkin, adjust = 0.5, row = 1, col = 2)


## ------------------------------------------------------------------------
detector_perkin <- create_module("PerkinElmerFull")

file_path <- "../inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml"

detector_perkin <- load_pix_matrix(detector = detector_perkin, file_path = file_path)

cat(detector_summary(detector_perkin))
cat(dead_stats_summary(detector_perkin))

## ------------------------------------------------------------------------
detector_perkin <- create_module("PerkinElmerFull")

file_path <- "../inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml"

detector_perkin <- load_pix_matrix(detector = detector_perkin, file_path = file_path)

detector_perkin <- get_dead_stats(detector_perkin)

plot_detector_cnt_mod(detector = detector_perkin, caption = FALSE)

plot_detector_cnt_mod(detector = detector_perkin, row = 1, col = 5, caption = FALSE)

## ------------------------------------------------------------------------
detector_perkin <- create_module("PerkinElmerFull")

file_path <- "../inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml"

detector_perkin <- load_pix_matrix(detector = detector_perkin, file_path = file_path)

plot_detector_arrows(detector = detector_perkin)

plot_detector_arrows(detector = detector_perkin, row = 1, col = 5)


## ------------------------------------------------------------------------
detector_perkin <- create_module("PerkinElmerFull")

file_path <- "../inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml"

detector_perkin <- load_pix_matrix(detector = detector_perkin, file_path = file_path)

plot_detector_angles(detector = detector_perkin)

plot_detector_angles(detector = detector_perkin, row = 1, col = 5)


## ------------------------------------------------------------------------
detector_perkin <- create_module("PerkinElmerFull")

file_path <- "../inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml"

detector_perkin <- load_pix_matrix(detector = detector_perkin, file_path = file_path)

plot_detector_kfg(detector = detector_perkin, func = "K", caption = FALSE)
plot_detector_kfg(detector = detector_perkin, func = "F")
plot_detector_kfg(detector = detector_perkin, func = "G")

plot_detector_kfg(detector = detector_perkin, func = "K", row = 1, col = 5, caption = FALSE)
plot_detector_kfg(detector = detector_perkin, func = "F", row = 1, col = 5, caption = FALSE)
plot_detector_kfg(detector = detector_perkin, func = "G", row = 1, col = 5, caption = FALSE)

## ------------------------------------------------------------------------
detector_pilatus <- create_module("Pilatus")

file_path <- "../inst/extdata/Pilatus/badpixel_mask.tif"

detector_pilatus <- load_pix_matrix(detector = detector_pilatus, file_path = file_path)

plot_detector_kfg(detector = detector_pilatus, func = "Kinhom")
plot_detector_kfg(detector = detector_pilatus, func = "Finhom")
plot_detector_kfg(detector = detector_pilatus, func = "Ginhom")

plot_detector_kfg(detector = detector_pilatus, func = "Kinhom", row = 1, col = 5, caption = FALSE)
plot_detector_kfg(detector = detector_pilatus, func = "Finhom", row = 1, col = 5, caption = FALSE)
plot_detector_kfg(detector = detector_pilatus, func = "Ginhom", row = 1, col = 5, caption = FALSE)

## ------------------------------------------------------------------------
detector_perkin <- create_module("PerkinElmerFull")

file_path <- "../inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml"

detector_perkin <- load_pix_matrix(detector = detector_perkin, file_path = file_path)

plot_detector_damaged(detector = detector_perkin)

## ------------------------------------------------------------------------

incl_event_list <- list(1,2,3,4,5,6,7,8)
      
detector_events <- detectorchecker::find_clumps(detector_perkin)
      
detectorchecker::plot_events(detector_events, caption = FALSE, incl_event_list = incl_event_list)

## ------------------------------------------------------------------------

row_ = 2
col_ = 3

detector_events <- detectorchecker::find_clumps(detector_perkin, row = row_, col = col_)

plot_module_events(detector_events, row = row_, col = col_, caption = FALSE, 
                   incl_event_list = incl_event_list)


## ------------------------------------------------------------------------

detector_events <- detectorchecker::find_clumps(detector_perkin)
detectorchecker::plot_events_arrows(detector_events, caption = FALSE, incl_event_list = incl_event_list)


## ------------------------------------------------------------------------
detector_events <- detectorchecker::find_clumps(detector_perkin, row = row_, col = col_)

detectorchecker::plot_events_density(detector_events, row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)
detectorchecker::plot_events_arrows(detector_events, row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)
detectorchecker::plot_events_angles(detector_events, row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)
detectorchecker::plot_events_kfg(detector_events, func = "K", row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)
detectorchecker::plot_events_kfg(detector_events, func = "Kinhom", row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)


## ------------------------------------------------------------------------
detectorchecker::plot_events_count(detector_events, row = row_, col = col_, caption = FALSE, incl_event_list = incl_event_list)

## ------------------------------------------------------------------------
detector_perkin <- create_module("PerkinElmerFull")

file_path <- "../inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml"

detector_perkin <- load_pix_matrix(detector = detector_perkin, file_path = file_path)

plot_detector_damaged(detector = detector_perkin)


detector_perkin_modified <- remove_high_density_cluster(detector_perkin, min_pts = 30, eps_adjust = 0.05)

plot_detector_damaged(detector = detector_perkin_modified)



