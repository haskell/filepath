{-# LANGUAGE CPP #-}

#undef  WINDOWS
#define POSIX
#define IS_WINDOWS False
#define FILEPATH_NAME PosixPath
#define OSSTRING_NAME PosixString
#define WORD_NAME PosixChar

#include "Common.hs"
