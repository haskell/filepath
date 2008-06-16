{-# LANGUAGE CPP #-}
#define MODULE_NAME     Windows
#define IS_WINDOWS      True

-----------------------------------------------------------------------------
-- |
-- Module      :  System.FilePath.Windows
-- Copyright   :  (c) Neil Mitchell 2005-2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- A library for FilePath manipulations, using Windows style paths on
-- all platforms. Importing "System.FilePath" is usually better.

-- Unfortunately, this #include breaks when haddocking with Cabal
#ifdef __HADDOCK__
module System.FilePath.Windows where
#else
#include "Internal.hs"
#endif
