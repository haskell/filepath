{-# LANGUAGE CPP #-}

#undef  POSIX
#define IS_WINDOWS True
#define WINDOWS
#define FILEPATH_NAME WindowsFilePath
#define OSSTRING_NAME WindowsString
#define WORD_NAME WindowsChar

#include "Common.hs"
