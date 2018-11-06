all:
	happy -gca ParRose.y
	alex -g LexRose.x
	ghc --make Interpret.hs -o Rose

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi Rose *.dyn_hi *.dyn.ho *.bak *.dyn_o
	

