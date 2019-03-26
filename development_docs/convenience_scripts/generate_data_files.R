devtools::load_all()

#detector_type: The type of detector
#file_path: The path of the pixel data
#data_name: Name to save the data under (so you can load from package as data(data_name))
create_dataset <- function(detector_type, file_path){

  detector <-  create_module(detector_type)
  detector <- load_pix_matrix(detector = detector, file_path = file_path)

  return(detector)

}

# PerkinElmer_Full
PerkinElmerFull_exp_1 <- create_dataset("PerkinElmerFull", "inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t1.bpm.xml")
devtools::use_data(PerkinElmerFull_exp_1, detectorchecker)

PerkinElmerFull_exp_2 <- create_dataset("PerkinElmerFull", "inst/extdata/PerkinElmer_Full/BadPixelMap.bpm/BadPixelMap_t2.bpm.xml")
devtools::use_data(PerkinElmerFull_exp_2, detectorchecker)

# PerkinElmer_Cropped
PerkinElmerCropped_exp_1 <- create_dataset("PerkinElmerCropped1600", "inst/extdata/PerkinElmer_Cropped/BadPixelMap.bmp/BadPixelMap_t1.bpm.xml")
devtools::use_data(PerkinElmerCropped_exp_1, detectorchecker)

PerkinElmerCropped_exp_2 <- create_dataset("PerkinElmerCropped1600", "inst/extdata/PerkinElmer_Cropped/BadPixelMap.bmp/BadPixelMap_t2.bpm.xml")
devtools::use_data(PerkinElmerCropped_exp_2, detectorchecker)

# PerkinElmer_Refurbished
PerkinElmerRefurbished_exp_1 <- create_dataset("PerkinElmerRefurbished", "inst/extdata/PerkinElmer_Refurbished/BadPixelMap_0.bpm/BadPixelMap_t1.bpm.xml")
devtools::use_data(PerkinElmerRefurbished_exp_1, detectorchecker)

PerkinElmerRefurbished_exp_2 <- create_dataset("PerkinElmerRefurbished", "inst/extdata/PerkinElmer_Cropped/BadPixelMap.bmp/BadPixelMap_t2.bpm.xml")
devtools::use_data(PerkinElmerRefurbished_exp_2, detectorchecker)


# Excaliber
Excalibur_exp_1 <- create_dataset("Excalibur", 
                                  c("inst/extdata/Excalibur/pixelmask.fem1.hdf", 
                                    "inst/extdata/Excalibur/pixelmask.fem2.hdf",
                                    "inst/extdata/Excalibur/pixelmask.fem3.hdf",
                                    "inst/extdata/Excalibur/pixelmask.fem4.hdf",
                                    "inst/extdata/Excalibur/pixelmask.fem5.hdf",
                                    "inst/extdata/Excalibur/pixelmask.fem6.hdf"))
                                  
devtools::use_data(Excalibur_exp_1, detectorchecker)


# Pilatus

Pilatus_exp_1 <- create_dataset("Pilatus", "inst/extdata/Pilatus/badpixel_mask.tif")

devtools::use_data(Pilatus_exp_1, detectorchecker)
