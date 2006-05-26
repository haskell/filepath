{-# OPTIONS_GHC -cpp #-}

#ifdef __HADDOCK__

{- |
    This module is a version of "System.FilePath" which uses Windows separators,
    even when run under Posix. For most purposes "System.FilePath" is a better
    choice.
-}

#endif

module System.FilePath.Windows

#ifdef __HADDOCK__
    where
#else

#define forceEffect ForceWindows
#define module --
#include "../FilePath.hs"

#endif

