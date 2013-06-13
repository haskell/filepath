# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: ghc-filepath-tests
ghc-filepath-tests:
	$(RM) -f GenTests.hi GenTests.o GenTests GenTests.exe
	$(RM) -f AutoTest.hi AutoTest.o
	$(RM) -f FilePath_Test.hs
	$(RM) -f FilePath_Test.hi FilePath_Test.o FilePath_Test FilePath_Test.exe
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make GenTests
	./GenTests
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make FilePath_Test
	./FilePath_Test

