%.exe: %.hs
	ghc --make -o $@ $<
	hlint $<

ALL: simpleparser1.exe simpleparser2.exe datatypeparser.exe recursiveparser.exe
