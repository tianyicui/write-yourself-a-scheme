%.exe: %.hs
	ghc --make -o $@ $<
	hlint $<

ALL: hello.exe
