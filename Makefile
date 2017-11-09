all:
	cd src && make
	mv src/insc_llvm src/insc_jvm ./

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.bc *.class *.j *.ll
	-rm insc_llvm insc_jvm
	cd src && make clean

distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* ComposOp.* Instant.dtd XMLInstant.* Makefile*
	

