%.exe: %.hs
	ghc --make -o $@ $<
	hlint $<

ALL: functionparser.exe
