images = image/fig2-a.pdf  image/fig2-b-trim.png  image/fig3-b.pdf  image/fig4-b.pdf image/fig2-b.png  image/fig3-a.pdf image/fig4-a.pdf image/flowchartDCshort.pdf



main: paper.pdf

paper.pdf: paper.md paper.bib
	pandoc +RTS -K512m -RTS paper.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output paper.pdf \
	-V colorlinks --highlight-style tango --variable graphics=yes --variable 'geometry:margin=1in' --variable 'compact-title:yes' --filter pandoc-citeproc --metadata link-citations=true

figures: Figures.R
	/usr/bin/Rscript $<
	/usr/bin/convert image/fig2-b.png -trim -bordercolor \#FFFFFF -border x16 image/fig2-b-trim.png

test:
	/usr/bin/pandoc +RTS -K512m -RTS test.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output test.pdf \
	-V colorlinks --highlight-style tango --variable graphics=yes --variable 'geometry:margin=1in' --variable 'compact-title:yes' --filter pandoc-citeproc --metadata link-citations=true
