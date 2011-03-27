%.exe: %.hs
	ghc --make -o $@ $<
	hlint $<

ALL: ioparser.exe
