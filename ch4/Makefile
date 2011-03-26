%.exe: %.hs
	ghc --make -o $@ $<
	hlint $<

ALL: errorcheck.exe
