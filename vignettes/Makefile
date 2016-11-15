RPROG=  $(R_HOME)/bin/R
 
all: RHRV-quickstart.pdf

clean:
	rm -f index.html
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl
 

RHRV-quickstart.pdf: clean RHRV-quickstart.Rnw
	# Generate the concordance.tex
	$(RPROG) CMD Sweave --clean --pdf RHRV-quickstart.Rnw
