all:
	happy -gca ParInstant.y
	alex -g LexInstant.x
	ghc -o insc_llvm InscLlvm.hs
	ghc -o insc_jvm InscJVM.hs

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.bc *.class

distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* ComposOp.* Instant.dtd XMLInstant.* Makefile*
	

