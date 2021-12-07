{-# LANGUAGE CPP #-}

#undef  WINDOWS
#define POSIX
#define IS_WINDOWS False
#define FILEPATH_NAME PosixFilePath
#define OSSTRING_NAME PosixString
#define WORD_NAME PosixChar
#define CTOR PS
#define WTOR PW

#include "Common.hs"
