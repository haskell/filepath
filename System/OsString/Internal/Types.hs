{-# LANGUAGE PatternSynonyms #-}

module System.OsString.Internal.Types {-# DEPRECATED "Use System.OsString.Internal.Types from os-string >= 2.0.0 package instead. This module will be removed in filepath >= 1.5." #-}
  (
    WindowsString(..)
  , pattern WS
  , unWS
  , PosixString(..)
  , unPS
  , pattern PS
  , PlatformString
  , WindowsChar(..)
  , unWW
  , pattern WW
  , PosixChar(..)
  , unPW
  , pattern PW
  , PlatformChar
  , OsString(..)
  , OsChar(..)
  )
where

import System.OsString.Internal.Types.Hidden