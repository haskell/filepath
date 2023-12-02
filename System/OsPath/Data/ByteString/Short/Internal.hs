-- |
-- Module      :  System.OsPath.Data.ByteString.Short.Internal
-- Copyright   :  © 2022 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal low-level utilities mostly for 'System.OsPath.Data.ByteString.Short.Word16',
-- such as byte-array operations and other stuff not meant to be exported from Word16 module.
module System.OsPath.Data.ByteString.Short.Internal {-# DEPRECATED "Use System.OsString.Data.ByteString.Short.Internal from os-string >= 2.0.0 package instead. This module will be removed in filepath >= 1.5." #-}
  ( module System.OsPath.Data.ByteString.Short.Internal.Hidden
  )
  where

import System.OsPath.Data.ByteString.Short.Internal.Hidden
