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

  -- * base encoding
  , encodeWithTE
  , decodeWithTE
  , encodeWithBasePosix
  , decodeWithBasePosix
  , encodeWithBaseWindows
  , decodeWithBaseWindows
  )
  where

import System.OsPath.Encoding.Internal
