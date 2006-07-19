{-# OPTIONS_GHC -cpp #-}

#ifdef __HADDOCK__

{- |
    This module is a version of "System.FilePath" which uses Posix separators,
    even when run under Windows. For most purposes "System.FilePath" is a better
    choice.
-}

#endif

module System.FilePath.Posix

#ifdef __HADDOCK__
    where
#else

#define forceEffect ForcePosix
#define module --
#ifdef TESTING
# define DRIVE_SECTION -}
# define END_DRIVE_SECTION {-
#endif

#include "../FilePath.hs"

#endif

