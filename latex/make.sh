pdflatex paper.tex > /dev/null  
bibtex paper > /dev/null
pdflatex paper.tex > /dev/null 
pdflatex -synctex=1 paper.tex > /dev/null 
date
