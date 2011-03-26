%.exe: %.hs
	ghc --make -o $@ $<
	hlint $<

ALL: operatorparser.exe
