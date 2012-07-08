# Makefile to use knitr for package vignettes

# put all PDF targets here, separated by spaces
HTMLS= corrplot-intro.html

all: $(HTMLS) 


%.html: %.Rnw
	$(R_HOME)/bin/Rscript -e "library(knitr); knit2html('$*.Rmd')"
