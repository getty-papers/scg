render: 
	latexmk -pdf -output-directory=tmp -pdflatex="pdflatex -interaction=nonstopmode" -use-make main.tex
	mv tmp/main.pdf .

clean: 
	rm -rf tmp/*

analysis:
	Rscript main.r


