{-# LANGUAGE CPP #-}

#undef  POSIX
#define WINDOWS
#define ABSTRACT_FILEPATH
#define IS_WINDOWS True
#define MODULE_NAME Windows

#include "../../FilePath/Internal.hs"
