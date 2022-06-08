-- |
-- Module      :  OsString
-- Copyright   :  © 2021 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of platform specific short 'OsString', which is:
--
-- 1. on windows UTF16 data
-- 2. on unix UTF8 data
--
-- It captures the notion of syscall specific encoding to avoid roundtrip issues
-- and memory fragmentation by using unpinned byte arrays.
module System.OsString
  (
  -- * String types
    OsString

  -- * String construction
  , toOsStringUtf
  , toOsStringEnc
  , toOsStringFS
  , osstr
  , packOsString

  -- * String deconstruction
  , fromOsStringUtf
  , fromOsStringEnc
  , fromOsStringFS
  , unpackOsString

  -- * Word types
  , OsChar

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar
  )
where

import System.OsString.Internal
    ( unsafeFromChar
    , toChar
    , fromOsStringUtf
    , fromOsStringEnc
    , fromOsStringFS
    , osstr
    , packOsString
    , toOsStringUtf
    , toOsStringEnc
    , toOsStringFS
    , unpackOsString
    )
import System.OsString.Internal.Types
    ( OsString, OsChar )
