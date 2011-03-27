%.exe: %.hs
	ghc --make -o $@ $<
	hlint $<

ALL: replparser.exe
