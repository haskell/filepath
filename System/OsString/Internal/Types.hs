{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module System.OsString.Internal.Types
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


import Control.DeepSeq
import Data.Data
import Data.Word
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import GHC.Generics (Generic)

import qualified System.AbstractFilePath.Data.ByteString.Short as BS
#if MIN_VERSION_template_haskell(2,16,0)
import qualified Language.Haskell.TH.Syntax as TH
#endif

-- Using unpinned bytearrays to avoid Heap fragmentation and
-- which are reasonably cheap to pass to FFI calls
-- wrapped with typeclass-friendly types allowing to avoid CPP
--
-- Note that, while unpinned bytearrays incur a memcpy on each
-- FFI call, this overhead is generally much preferable to
-- the memory fragmentation of pinned bytearrays

-- | Commonly used windows string as UTF16 bytes.
newtype WindowsString = WindowsString { getWindowsString :: BS.ShortByteString }
  deriving (Eq, Ord, Show, Semigroup, Monoid, Typeable, Generic, NFData)

-- | Just a short bidirectional synonym for 'WindowsString' constructor.
pattern WS :: BS.ShortByteString -> WindowsString
pattern WS { unWS } <- WindowsString unWS where
  WS a = WindowsString a
{-# COMPLETE WS #-}


instance Lift WindowsString where
  lift (WindowsString bs)
    = [| WindowsString (BS.pack $(lift $ BS.unpack bs)) :: WindowsString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- | Commonly used Posix string as uninterpreted @char[]@
-- array.
newtype PosixString = PosixString { getPosixString :: BS.ShortByteString }
  deriving (Eq, Ord, Show, Semigroup, Monoid, Typeable, Generic, NFData)

-- | Just a short bidirectional synonym for 'PosixString' constructor.
pattern PS :: BS.ShortByteString -> PosixString
pattern PS { unPS } <- PosixString unPS where
  PS a = PosixString a
{-# COMPLETE PS #-}

instance Lift PosixString where
  lift (PosixString bs)
    = [| PosixString (BS.pack $(lift $ BS.unpack bs)) :: PosixString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformString = WindowsString
#else
type PlatformString = PosixString
#endif

newtype WindowsChar = WindowsChar { getWindowsChar :: Word16 }
  deriving (Eq, Ord, Show, Typeable, Generic, NFData)
newtype PosixChar   = PosixChar { getPosixChar :: Word8 }
  deriving (Eq, Ord, Show, Typeable, Generic, NFData)

-- | Just a short bidirectional synonym for 'WindowsChar' constructor.
pattern WW :: Word16 -> WindowsChar
pattern WW { unWW } <- WindowsChar unWW where
  WW a = WindowsChar a
{-# COMPLETE WW #-}

-- | Just a short bidirectional synonym for 'WindowsChar' constructor.
pattern PW :: Word8 -> PosixChar
pattern PW { unPW } <- PosixChar unPW where
  PW a = PosixChar a
{-# COMPLETE PW #-}

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformChar = WindowsChar
#else
type PlatformChar = PosixChar
#endif


-- | Newtype representing short operating system specific strings.
--
-- Internally this is either 'WindowsString' or 'PosixString',
-- depending on the platform. Both use unpinned
-- 'ShortByteString' for efficiency.
--
-- The constructor is only exported via "System.OsString.Internal.Types", since
-- dealing with the internals isn't generally recommended, but supported
-- in case you need to write platform specific code.
newtype OsString = OsString { getOsString :: PlatformString }
  deriving (Show, Typeable, Generic, NFData)

-- | Byte equality of the internal representation.
instance Eq OsString where
  (OsString a) == (OsString b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsString where
  compare (OsString a) (OsString b) = compare a b


-- | \"String-Concatenation\" for 'OsString. This is __not__ the same
-- as '(</>)'.
instance Monoid OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    mempty      = OsString (WindowsString BS.empty)
#if MIN_VERSION_base(4,16,0)
    mappend = (<>)
#else
    mappend (OsString (WindowsString a)) (OsString (WindowsString b))
      = OsString (WindowsString (mappend a b))
#endif
#else
    mempty      = OsString (PosixString BS.empty)
#if MIN_VERSION_base(4,16,0)
    mappend = (<>)
#else
    mappend (OsString (PosixString a)) (OsString (PosixString b))
      = OsString (PosixString (mappend a b))
#endif
#endif
#if MIN_VERSION_base(4,11,0)
instance Semigroup OsString where
#if MIN_VERSION_base(4,16,0)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    (<>) (OsString (WindowsString a)) (OsString (WindowsString b))
      = OsString (WindowsString (mappend a b))
#else
    (<>) (OsString (PosixString a)) (OsString (PosixString b))
      = OsString (PosixString (mappend a b))
#endif
#else
    (<>) = mappend
#endif
#endif


instance Lift OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  lift (OsString (WindowsString bs))
    = [| OsString (WindowsString (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#else
  lift (OsString (PosixString bs))
    = [| OsString (PosixString (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif


-- | Newtype representing a code unit.
--
-- On Windows, this is restricted to two-octet codepoints 'Word16',
-- on POSIX one-octet ('Word8').
newtype OsChar = OsChar { getOsChar :: PlatformChar }
  deriving (Show, Typeable, Generic, NFData)

-- | Byte equality of the internal representation.
instance Eq OsChar where
  (OsChar a) == (OsChar b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsChar where
  compare (OsChar a) (OsChar b) = compare a b

