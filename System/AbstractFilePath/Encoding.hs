module System.AbstractFilePath.Encoding
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
  , encodeWith
  , decodeWith
  , encodeWithBasePosix
  , decodeWithBasePosix
  , encodeWithBaseWindows
  , decodeWithBaseWindows
  )
  where

import System.AbstractFilePath.Encoding.Internal
