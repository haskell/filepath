# This Makefile runs the tests using GHC's testsuite framework.  It
# assumes the package is part of a GHC build tree with the testsuite
# installed in ../../../testsuite.

TOP=../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: ghc-filepath-tests
ghc-filepath-tests:
	$(RM) -f GenTests.hi GenTests.o GenTests
	$(RM) -f AutoTest.hi AutoTest.o
	$(RM) -f FilePath_Test.hi FilePath_Test.o FilePath_Test
	$(TEST_HC) -v0 --make GenTests
	./GenTests
	$(TEST_HC) -v0 --make FilePath_Test
	./FilePath_Test

