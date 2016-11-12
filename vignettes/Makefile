RSCRIPT = $(R_HOME)/bin/Rscript
RPROG=  $(R_HOME)/bin/R
 
all: RHRV-quickstart.pdf

clean:
	rm -f index.html
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl
 

RHRV-quickstart.pdf: clean RHRV-quickstart.Rnw
	# Generate the concordance.tex
	$(RPROG) CMD Sweave RHRV-quickstart.Rnw
	$(RSCRIPT) --vanilla -e "require(highlight); driver <- HighlightWeaveLatex(boxes = TRUE); Sweave( 'RHRV-quickstart.Rnw', driver = driver );"
	$(RSCRIPT) -e "tools::texi2dvi( 'RHRV-quickstart.tex', pdf = TRUE, clean = TRUE )"
	rm -fv Rplots.pdf
