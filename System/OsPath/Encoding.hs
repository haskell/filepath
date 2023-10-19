-- |
-- Module      :  System.OsPath.Encoding
-- Copyright   :  © 2023 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Encoding helpers for 'OsPath'. It's advised to import this module qualified to
-- avoid name clashes with @System.OsString.Encoding@:
--
-- > import qualified System.OsPath.Encoding as OSE
module System.OsPath.Encoding
  (
  -- * Types
    EncodingException(..)
  , showEncodingException

  -- * UCS-2
  , ucs2le
  , mkUcs2le
  , ucs2le_DF
  , ucs2le_EF
  , ucs2le_decode
  , ucs2le_encode

  -- * UTF-16LE_b
  , utf16le_b
  , mkUTF16le_b
  , utf16le_b_DF
  , utf16le_b_EF
  , utf16le_b_decode
  , utf16le_b_encode

  -- * base encoding
  , encodeWithBasePosix
  , decodeWithBasePosix
  , encodeWithBaseWindows
  , decodeWithBaseWindows

  -- * low-level functions
  , withFilePathWin
  , peekFilePathWin
  , withFilePathPosix
  , peekFilePathPosix
  )
  where

import Foreign.C (CStringLen)
import Foreign

import System.OsString.Encoding.Internal

-- | Suitable for executing ffi calls on a windows filepath. The callback
-- takes the length of the filepath and a 'Word16' pointer as arguments.
--
-- @since 1.4.200.0
withFilePathWin :: FilePath -> (Int -> Ptr Word16 -> IO a) -> IO a
withFilePathWin = withWindowsString

-- | Read a windows filepath from a given memory location, assuming
-- the given length.
--
-- @since 1.4.200.0
peekFilePathWin :: (Ptr Word16, Int) -> IO FilePath
peekFilePathWin = peekWindowsString

-- | Suitable for executing ffi calls on a unix filepath.
--
-- @since 1.4.200.0
withFilePathPosix :: FilePath -> (CStringLen -> IO a) -> IO a
withFilePathPosix = withPosixString

-- | Read a windows filepath from a given memory location, assuming
-- the given length.
--
-- @since 1.4.200.0
peekFilePathPosix :: CStringLen -> IO FilePath
peekFilePathPosix = peekPosixString

