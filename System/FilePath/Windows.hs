{-# OPTIONS_GHC -cpp #-}

module System.FilePath.Windows

#define forceEffect ForceWindows

#define module --
#include "../FilePath.hs"


