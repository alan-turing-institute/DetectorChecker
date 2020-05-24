main: JOSSpaper.pdf

JOSSpaper.pdf: figures JOSSpaper.md JOSSpaper.bib
	/usr/bin/pandoc +RTS -K512m -RTS JOSSpaper.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output JOSSpaper.pdf \
	-V colorlinks --highlight-style tango --variable graphics=yes --variable 'geometry:margin=1in' --variable 'compact-title:yes' --filter pandoc-citeproc --metadata link-citations=true

figures:
	/usr/bin/Rscript Figures.R
	for i in 2 3 4 ; do /usr/bin/convert image/fig$$i-a.png image/fig$$i-b.png +append figure$$i.png ; done
