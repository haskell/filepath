{-# LANGUAGE CPP #-}

#undef WINDOWS
#define ABSTRACT_FILEPATH
#define IS_WINDOWS False
#define MODULE_NAME Posix

#include "../../FilePath/Internal.hs"
