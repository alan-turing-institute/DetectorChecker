# Create figures for JOSS paper

library(detectorchecker)

pilatus_detector <- create_detector("Pilatus")

file_path <- "../inst/extdata/Pilatus/badpixel_mask.tif"
pilatus_detector <- load_pix_matrix(detector = pilatus_detector, file_path = file_path)

pdf("image/fig2-a.pdf")
plot_pixels(detector = pilatus_detector, caption = FALSE)
dev.off()

pdf("image/fig2-b.pdf")
# In the next line, is adjust correct?
plot_pixels_density(detector = pilatus_detector, adjust = 5, caption = FALSE)
dev.off()

pdf("image/fig3-a.pdf")
plot_pixels_kfg(detector = pilatus_detector, func = "F", caption = FALSE)
dev.off()

pdf("image/fig3-b.pdf")
plot_pixels_kfg(detector = pilatus_detector, func = "G", caption = FALSE)
dev.off()

pdf("image/fig4-a.pdf")
plot_pixels_kfg(detector = pilatus_detector, func = "K", caption = FALSE)
dev.off()

pdf("image/fig4-b.pdf")
plot_pixels_kfg(detector = pilatus_detector, func = "Kinhom", caption = FALSE)
dev.off()
