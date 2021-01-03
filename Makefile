all: tutorial.html tutorial.tex wrapper.pdf

tutorial.html: tutorial.md
	cmark --to html tutorial.md > tutorial.html

tutorial.tex: tutorial.md
	cmark --to latex tutorial.md > tutorial.tex

wrapper.pdf: wrapper.tex tutorial.tex
	pdflatex wrapper
	pdflatex wrapper
