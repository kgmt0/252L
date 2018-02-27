BASE=writeup

all: ${BASE}.pdf

%.tex: %.Rnw
	Rscript -e "library(knitr); knit('$^')"

%.pdf: %.tex
	pdflatex $^

.PHONY: clean

clean:
	rm -fv	${BASE}.pdf ${BASE}.tex ${BASE}.aux ${BASE}.log texput.log
