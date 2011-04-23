.SUFFIXES: .lhs .pdf .tex .html

.lhs.tex:
	pandoc -f markdown+lhs -t latex+lhs -o $@ $< -s --template=../latex.template

.tex.pdf:
	pdflatex $<

.lhs.html:
	pandoc -f markdown+lhs -t html -o $@ $< -s --template=../html.template
