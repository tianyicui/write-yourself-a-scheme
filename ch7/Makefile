%.exe: %.hs
	ghc --make -o $@ $<
	hlint $<

ALL: variableparser.exe
