# doc/GNUmakefile

PANDOC_FLAGS := --smart -V papersize:"a4paper"

.PHONY: all clean

all: hal8.pdf hal8.html hal8-folien.pdf

%.tex : %.md
	pandoc -s $(PANDOC_FLAGS) -f markdown -t latex $< -o $@

%.html : %.md
	pandoc -s $(PANDOC_FLAGS) -f markdown -t html $< -o $@

%.pdf : %.tex
	pdflatex $<
	while grep -i >/dev/null "rerun to.*get.*right" $*.log; do \
	  pdflatex $<; \
	done

hal8-folien.pdf : hal8-folien.md
	pandoc -t beamer $< -o $@

clean:
	@rm -f hal8.aux hal8.html hal8.log hal8.out hal8.pdf hal8.toc
