render: 
	latexmk -pdf -output-directory=tmp -pdflatex="pdflatex -interaction=nonstopmode" -use-make main.tex
	mv tmp/main.pdf .

clean: 
	rm -rf tmp/*

analysis:
	Rscript main.r

commit:
	git add -A
	git commit -m "update"
	git push origin main

prepare:
	make render
	mv tmp/main.bbl .
	zip -r arxiv.zip latex_styles/ figure-pdf/ tables/ main.tex main.bbl
