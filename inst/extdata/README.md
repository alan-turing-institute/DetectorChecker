# Example files

This folder contains a set of example datasets, which can be used with the DetectorCheckerWebApp and are used internally by DetectorChecker.

## How to download

Please download the full repository by going to https://github.com/alan-turing-institute/DetectorChecker and clicking `clone or download`. Download as a zip file and then navigate to the `inst/extdata` folder.

You can then use these files with the DectorCheckerWebApp (https://github.com/alan-turing-institute/DetectorCheckerWebApp).

## Files by detector type

Examples are provided for six detector types. It is possible to load datasets as a single file, or split across multiple files. All examples, except Excalibur, use a single file. For Excalibur you will need to load all examples into the WebApp simultaneously:

1. PerkinElmer_Full
    - Two example datasets:
        - BadPixelMap_t1.bmp.xml
        - BadPixelMap_t2.bmp.xml

2. PerkinElmer_Refurbished
    - Two example datasets:
        - BadPixelMap_t1.bmp.xml
        - BadPixelMap_t2.bmp.xml

3. PerkinElmer_Cropped
    - Two example datasets:
        - BadPixelMap_t1.bmp.xml
        - BadPixelMap_t2.bmp.xml

4. Excalibur
    - One example dataset, split across 5 files (load them all in to the DetectorChecker WebApp):
        - pixelmask.fem1.hdf
        - pixelmask.fem2.hdf
        - pixelmask.fem3.hdf
        - pixelmask.fem4.hdf
        - pixelmask.fem5.hdf
        - pixelmask.fem6.hdf

5. Pilatus
    - One example dataset:
        - badpixel_mask.tif

6. user-defined
	- irregular
		- layout_par_irregular.txt - layout file
		- badpixelmap_irregular - example of a dead pixels file
		
	- photographic (photographic aspect ratio without any submodes and gaps)
   		- layout_par_photographic.txt - layout file
		- examples of dead pixels files:
  			- photographic.tif
  			- badpixelmap_photographic_hom.xml
  			- badpixelmap_photographic_inhom.xml
  			
	- simple (example of a user defined detector type having 2 rows and 16 columns)
		- user-defined.txt - layout file
		- user-defined.bpm.xml - dead pixels file
	
