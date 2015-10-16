# doc/GNUmakefile

PANDOC_FLAGS := --smart -V papersize:"a4paper"

.PHONY: all clean

all:: hal8.pdf hal8.html hal8-folien.pdf nixos-meetup-3-slides.pdf
all:: 00-future-of-haskell-packaging-in-nixpkgs.pdf
all:: 01-use-function-application-to-escape-override-hell.pdf
all:: 02-guarantee-consistent-builds-and-obsolete-overridescope.pdf
all:: 03-map-cabal-files-to-nix-without-information-loss.md
all:: 04-set-up-a-sufficiently-powerful-build-farm.md


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

nixos-meetup-3-slides.pdf : nixos-meetup-3-slides.md
	pandoc -t beamer $< -o $@

clean:
	@rm -f hal8.aux hal8.html hal8.log hal8.out hal8.pdf hal8.toc
