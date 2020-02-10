figures := $(wildcard figure*.png)

main: JOSSpaper.pdf

JOSSpaper.pdf: JOSSpaper.md JOSSpaper.bib $(figures)
	pandoc +RTS -K512m -RTS JOSSpaper.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output JOSSpaper.pdf --template /home/wilfrid/R/x86_64-pc-linux-gnu-library/3.6/rmarkdown/rmd/latex/default-1.17.0.2.tex --highlight-style tango --variable graphics=yes --variable 'geometry:margin=1in' --variable 'compact-title:yes' --filter /usr/lib/rstudio/bin/pandoc/pandoc-citeproc 
