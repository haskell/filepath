{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module System.AbstractFilePath.Types
  (
  -- * FilePath types
    AbstractFilePath
  , WindowsFilePath
  , PosixFilePath
  , PlatformFilePath

  -- * OsString reexports
  , WindowsString
  , PosixString
  , WindowsChar
  , PosixChar
  , OsString
  , OsChar
  )
where

import System.OsString.Internal.Types


-- | Filepaths are @wchar_t*@ data on windows as passed to syscalls.
type WindowsFilePath = WindowsString

-- | Filepaths are @char[]@ data on unix as passed to syscalls.
type PosixFilePath = PosixString

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
-- | Ifdef around current platform (either 'WindowsFilePath' or 'PosixFilePath').
type PlatformFilePath = WindowsFilePath
#else
-- | Ifdef around current platform (either 'WindowsFilePath' or 'PosixFilePath').
type PlatformFilePath = PosixFilePath
#endif


-- | Type representing filenames\/pathnames.
--
-- This type doesn't add any guarantees over 'OsString'.
type AbstractFilePath = OsString
