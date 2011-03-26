%.exe: %.hs
	ghc --make -o $@ $<
	hlint $<

ALL: evaluator1.exe evaluator2.exe
