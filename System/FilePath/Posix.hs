{-# OPTIONS_GHC -cpp #-}

module System.FilePath.Posix

#define forceEffect ForcePosix

#define module --
#include "../FilePath.hs"


