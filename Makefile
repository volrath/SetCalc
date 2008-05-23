DOCDIR   = doc
BINARIES = SetCalc
OBJECTS  = *.o *.hi
SOURCES  = SetCalc.hs
MODULES  = Lexer.x

all:	$(BINARIES) doc

SetCalc:$(SOURCES) $(MODULES)
	hmake SetCalc

clean:
	rm -rf Lex*.hs $(OBJECTS) $(BINARIES) doc

doc:	$(SOURCES) $(MODULES)
	test -d $(DOCDIR) || mkdir $(DOCDIR)
	rm -rf $(DOCDIR)/*
	ghc -cpp -E -optP-P -D__HADDOCK__ Lexer.hs -o Lex.hs
	haddock --html --ignore-all-exports --odir=$(DOCDIR) $(SOURCES) Lex.hs
	rm -f Lex.hs