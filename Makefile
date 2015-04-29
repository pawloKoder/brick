all:
	happy -gca Parbrick.y
	alex -g Lexbrick.x
	latex Docbrick.tex; dvips Docbrick.dvi -o Docbrick.ps
	ghc --make Testbrick.hs -o Testbrick
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docbrick.ps
distclean: clean
	-rm -f Docbrick.* Lexbrick.* Parbrick.* Layoutbrick.* Skelbrick.* Printbrick.* Testbrick.* Absbrick.* Testbrick ErrM.* SharedString.* brick.dtd XMLbrick.* Makefile*

