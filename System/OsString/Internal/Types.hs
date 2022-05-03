{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module System.OsString.Internal.Types
  (
    WindowsString(..)
  , PosixString(..)
  , PlatformString
  , WindowsChar(..)
  , PosixChar(..)
  , PlatformChar
  , OsString(..)
  , OsChar(..)
  )
where


import Control.DeepSeq
import Data.Data
import Data.Word
import GHC.Exts
    ( IsString (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import GHC.Generics (Generic)

import System.AbstractFilePath.Data.ByteString.Short.Decode
import System.AbstractFilePath.Data.ByteString.Short.Encode

import qualified Data.ByteString.Short as BS
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
newtype WindowsString = WS { unWFP :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid, Typeable, Generic, NFData)

instance Lift WindowsString where
  lift (WS bs)
    = [| WS (BS.pack $(lift $ BS.unpack bs)) :: WindowsString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- | Commonly used Posix string as uninterpreted @char[]@
-- array.
newtype PosixString   = PS { unPFP :: BS.ShortByteString }
  deriving (Eq, Ord, Semigroup, Monoid, Typeable, Generic, NFData)

instance Lift PosixString where
  lift (PS bs)
    = [| PS (BS.pack $(lift $ BS.unpack bs)) :: PosixString |]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- | Decodes as UTF-16LE.
instance Show WindowsString where
  show (WS bs) = ('\"': decodeUtf16LEWith lenientDecode bs) <> "\""

-- | Encodes as UTF-16LE.
instance Read WindowsString where
  readsPrec p str = [ (WS $ encodeUtf16LE x, y) | (x, y) <- readsPrec p str ]

-- | Decodes as UTF-8 and replaces invalid chars with unicode replacement
-- char U+FFFD.
instance Show PosixString where
  show (PS bs) = ('\"': decodeUtf8With lenientDecode bs) <> "\""

-- | Encodes as UTF-8.
instance Read PosixString where
  readsPrec p str = [ (PS $ encodeUtf8 x, y) | (x, y) <- readsPrec p str ]

instance IsString WindowsString where 
    fromString = WS . encodeUtf16LE

instance IsString PosixString where 
    fromString = PS . encodeUtf8

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type PlatformString = WindowsString
#else
type PlatformString = PosixString
#endif

newtype WindowsChar = WW { unWW :: Word16 }
  deriving (Eq, Ord, Show, Typeable, Generic, NFData)
newtype PosixChar   = PW { unPW :: Word8 }
  deriving (Eq, Ord, Show, Typeable, Generic, NFData)

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
newtype OsString = OsString PlatformString
  deriving (Typeable, Generic, NFData)

-- | Byte equality of the internal representation.
instance Eq OsString where
  (OsString a) == (OsString b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsString where
  compare (OsString a) (OsString b) = compare a b

-- | Encodes as UTF16 on windows and UTF8 on unix.
instance IsString OsString where 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    fromString = OsString . WS . encodeUtf16LE
#else
    fromString = OsString . PS . encodeUtf8
#endif


-- | \"String-Concatenation\" for 'OsString. This is __not__ the same
-- as '(</>)'.
instance Monoid OsString where 
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    mempty      = OsString (WS BS.empty)
#if MIN_VERSION_base(4,16,0)
    mappend = (<>)
#else
    mappend (OsString (WS a)) (OsString (WS b))
      = OsString (WS (mappend a b))
#endif
#else
    mempty      = OsString (PS BS.empty)
#if MIN_VERSION_base(4,16,0)
    mappend = (<>)
#else
    mappend (OsString (PS a)) (OsString (PS b))
      = OsString (PS (mappend a b))
#endif
#endif
#if MIN_VERSION_base(4,11,0)
instance Semigroup OsString where 
#if MIN_VERSION_base(4,16,0)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    (<>) (OsString (WS a)) (OsString (WS b))
      = OsString (WS (mappend a b))
#else
    (<>) (OsString (PS a)) (OsString (PS b))
      = OsString (PS (mappend a b))
#endif
#else
    (<>) = mappend
#endif
#endif


instance Lift OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  lift (OsString (WS bs))
    = [| OsString (WS (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#else
  lift (OsString (PS bs))
    = [| OsString (PS (BS.pack $(lift $ BS.unpack bs))) :: OsString |]
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- | Decodes as UTF-16 on windows.
--
-- Decodes as UTF-8 on unix and replaces invalid chars with unicode replacement
-- char U+FFFD.
instance Show OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  show (OsString (WS bs)) = ('\"': decodeUtf16LEWith lenientDecode bs) <> "\""
#else
  show (OsString (PS bs)) = ('\"': decodeUtf8With lenientDecode bs) <> "\""
#endif

-- | Encodes as UTF-8 on unix and UTF-16LE on windows.
instance Read OsString where
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  readsPrec p str = [ (OsString $ WS $ encodeUtf16LE x, y) | (x, y) <- readsPrec p str ]
#else
  readsPrec p str = [ (OsString $ PS $ encodeUtf8 x, y) | (x, y) <- readsPrec p str ]
#endif


-- | Newtype representing a code unit.
--
-- On Windows, this is restricted to two-octet codepoints 'Word16',
-- on POSIX one-octet ('Word8').
newtype OsChar = OsChar PlatformChar
  deriving (Show, Typeable, Generic, NFData)

-- | Byte equality of the internal representation.
instance Eq OsChar where
  (OsChar a) == (OsChar b) = a == b

-- | Byte ordering of the internal representation.
instance Ord OsChar where
  compare (OsChar a) (OsChar b) = compare a b



