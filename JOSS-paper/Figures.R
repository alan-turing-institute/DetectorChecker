# Create figures for JOSS paper

library(detectorchecker)

pilatus_detector <- create_detector("Pilatus")

file_path <- "../inst/extdata/Pilatus/badpixel_mask.tif"
pilatus_detector <- load_pix_matrix(detector = pilatus_detector, file_path = file_path)

png("image/fig2-a.png")
plot_pixels(detector = pilatus_detector, caption = FALSE)
dev.off()

png("image/fig2-b.png")
# In the next line, is adjust correct?
plot_pixels_density(detector = pilatus_detector, adjust = 5, caption = FALSE)
dev.off()

png("image/fig3-a.png")
plot_pixels_kfg(detector = pilatus_detector, func = "F", caption = FALSE)
dev.off()

png("image/fig3-b.png")
plot_pixels_kfg(detector = pilatus_detector, func = "G", caption = FALSE)
dev.off()

png("image/fig4-a.png")
plot_pixels_kfg(detector = pilatus_detector, func = "K", caption = FALSE)
dev.off()

png("image/fig4-b.png")
plot_pixels_kfg(detector = pilatus_detector, func = "Kinhom", caption = FALSE)
dev.off()
