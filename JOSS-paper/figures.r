# Create figures for JOSS paper

library(detectorchecker)
pilatus_detector <- create_module("Pilatus")

file_path <- "../inst/extdata/Pilatus/badpixel_mask.tif"
pilatus_detector <- load_pix_matrix(detector = pilatus_detector, file_path = file_path)

plot_detector_damaged(detector = pilatus_detector, caption = FALSE, file_path='image/f2a.jpg')
# How to get topocolor? and is adjust correct?
plot_detector_density(detector = pilatus_detector, adjust = 5, caption = FALSE, file_path='image/f2b.jpg')


plot_detector_kfg(detector = pilatus_detector, func = "F", caption = FALSE, file_path='image/f3a.jpg')
plot_detector_kfg(detector = pilatus_detector, func = "G", caption = FALSE, file_path='image/f3b.jpg')

plot_detector_kfg(detector = pilatus_detector, func = "K", caption = FALSE, file_path='image/f4a.jpg')
plot_detector_kfg(detector = pilatus_detector, func = "Kinhom", caption = FALSE, file_path='image/f4b.jpg')
