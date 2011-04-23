.SUFFIXES: .lhs .pdf .tex

.lhs.tex:
	pandoc -f markdown+lhs -t latex+lhs -o $@ $< -s --template=../latex.template

.tex.pdf:
	pdflatex $<
