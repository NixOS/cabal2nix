# doc/GNUmakefile

PANDOC_FLAGS := --smart -V papersize:"a4paper"

.PHONY: all clean

all:: hal8.pdf hal8.html hal8-folien.pdf nixos-meetup-3-slides.pdf
all:: 00-future-of-haskell-packaging-in-nixpkgs.pdf
all:: 01-use-function-application-to-escape-override-hell.pdf
all:: 02-guarantee-consistent-builds-and-obsolete-overridescope.pdf
all:: 03-map-cabal-files-to-nix-without-information-loss.pdf
all:: 04-set-up-a-sufficiently-powerful-build-farm.pdf
all:: 05-publish-all-of-hackage.pdf
all:: nixcon-2015-slides.pdf

%.tex : %.md
	pandoc -s $(PANDOC_FLAGS) -f markdown -t latex $< -o $@

%.html : %.md
	pandoc -s $(PANDOC_FLAGS) -f markdown -t html $< -o $@

%.pdf : %.tex
	pdflatex $<
	while grep -i >/dev/null "rerun to.*get.*right" $*.log; do \
	  pdflatex $<; \
	done

%.eps : %.dia
	dia -e $@ -t eps $<

%.pdf : %.eps
	ps2pdf -dEPSCrop $< $@

hal8-folien.pdf : hal8-folien.md
	pandoc -t beamer $< -o $@

nixos-meetup-3-slides.pdf : nixos-meetup-3-slides.md
	pandoc -t beamer $< -o $@

nixcon-2015-slides.pdf : nixcon-2015-slides.md pipeline-overview.pdf pipeline-details.pdf \
			 hackage-version-boxplot.pdf
	pandoc -t beamer $< -o $@

hackage-version-boxplot.pdf : analyze-hackage.r hackage.csv
	Rscript $<

hackage.csv:
	{ set -eu -o pipefail; \
	  cd ../hackage; \
	  echo package,version; \
	  for n in *; do \
	    test -d $$n || continue; \
	    for v in $$n/*; do \
	      echo $$n,$$(basename $$v); \
	    done; \
	  done \
	} >$@.tmp
	mv $@.tmp $@

clean:
	@rm -f *.aux *.html *.log *.out *.pdf *.toc
	@rm -f .Rhistory builds.csv hackage.csv
