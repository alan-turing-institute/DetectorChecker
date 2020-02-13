figures := $(wildcard figure*.png)

main: JOSSpaper.pdf

JOSSpaper.pdf: JOSSpaper.md JOSSpaper.bib $(figures)
	pandoc +RTS -K512m -RTS JOSSpaper.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output JOSSpaper.pdf --highlight-style tango --variable graphics=yes --variable 'geometry:margin=1in' --variable 'compact-title:yes' --filter pandoc-citeproc
