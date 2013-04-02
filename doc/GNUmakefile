# doc/GNUmakefile

PANDOC_FLAGS := --smart -V papersize:"a4paper"

.PHONY: all clean

all: hal8.pdf hal8.html

%.tex : %.md
	pandoc -s $(PANDOC_FLAGS) -f markdown -t latex $< >$@

%.html : %.md
	pandoc -s $(PANDOC_FLAGS) -f markdown -t html $< >$@

%.pdf : %.tex
	pdflatex $<
	while grep -i >/dev/null "rerun to.*get.*right" $*.log; do \
	  pdflatex $<; \
	done

clean:
	@rm -f hal8.aux hal8.html hal8.log hal8.out hal8.pdf hal8.toc
