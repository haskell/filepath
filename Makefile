# -----------------------------------------------------------------------------

TOP=..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

ifeq "$(IncludeExampleDirsInBuild)" "YES"
SUBDIRS += examples
endif

ALL_DIRS = \
	System \
	System/FilePath

PACKAGE = filepath
VERSION = 1.0
PACKAGE_DEPS = base

EXCLUDED_SRCS += Setup.hs
EXCLUDED_SRCS += System/FilePath/Internal.hs

SRC_HC_OPTS += -cpp

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
