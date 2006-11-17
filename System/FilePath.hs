{- |
Module      :  System.FilePath
Copyright   :  (c) Neil Mitchell 2005-2006
License     :  BSD3

Maintainer  :  http://www.cs.york.ac.uk/~ndm/
Stability   :  in-progress
Portability :  portable

A library for FilePath manipulations, this is a wrapper to pick the latest version
of the libary. For the moment I suggest importing "System.FilePath.Version_0_10"
directly, to avoid any future compatability concerns.

I am currently creating Version_0_11, which is substantially similar to Version_0_10
but will serve as the next release.
-}

module System.FilePath(module System.FilePath.Version_0_11) where

import System.FilePath.Version_0_11
