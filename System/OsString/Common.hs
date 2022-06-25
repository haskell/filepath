{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
-- This template expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True
--
#if defined(WINDOWS)
#define WINDOWS_DOC
#else
#define POSIX_DOC
#endif

module System.OsString.MODULE_NAME
  (
  -- * Types
#ifdef WINDOWS
    WindowsString
  , WindowsChar
#else
    PosixString
  , PosixChar
#endif

  -- * String construction
  , toPlatformStringUtf
  , toPlatformStringEnc
  , toPlatformStringFS
  , bytesToPlatformString
  , pstr
  , packPlatformString

  -- * String deconstruction
  , fromPlatformStringUtf
  , fromPlatformStringEnc
  , fromPlatformStringFS
  , unpackPlatformString

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar
  )
where



import System.OsString.Internal.Types (
#ifdef WINDOWS
  WindowsString(..), WindowsChar(..)
#else
  PosixString(..), PosixChar(..)
#endif
  )

import Data.Char
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Data.ByteString.Internal
    ( ByteString )
import Control.Exception
    ( SomeException, try, displayException )
import Control.DeepSeq ( force )
import Data.Bifunctor ( first )
import GHC.IO
    ( evaluate, unsafePerformIO )
import qualified GHC.Foreign as GHC
import Language.Haskell.TH
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )


import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
#ifdef WINDOWS
import System.OsPath.Encoding
import System.IO
    ( TextEncoding, utf16le )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import System.OsPath.Data.ByteString.Short.Word16 as BS
import qualified System.OsPath.Data.ByteString.Short as BS8
#else
import System.OsPath.Encoding
import System.IO
    ( TextEncoding, utf8 )
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import System.OsPath.Data.ByteString.Short as BS
#endif



#ifdef WINDOWS_DOC
-- | Partial unicode friendly encoding.
--
-- This encodes as UTF16-LE (strictly), which is a pretty good guess.
--
-- Throws an 'EncodingException' if encoding fails.
#else
-- | Partial unicode friendly encoding.
--
-- This encodes as UTF8 (strictly), which is a good guess.
--
-- Throws an 'EncodingException' if encoding fails.
#endif
toPlatformStringUtf :: MonadThrow m => String -> m PLATFORM_STRING
#ifdef WINDOWS
toPlatformStringUtf = either throwM pure . toPlatformStringEnc utf16le
#else
toPlatformStringUtf = either throwM pure . toPlatformStringEnc utf8
#endif

-- | Like 'toPlatformStringUtf', except allows to provide an encoding.
toPlatformStringEnc :: TextEncoding
                    -> String
                    -> Either EncodingException PLATFORM_STRING
toPlatformStringEnc enc str = unsafePerformIO $ do
#ifdef WINDOWS
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> WindowsString <$> BS8.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#else
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> PosixString <$> BS.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#endif

#ifdef WINDOWS_DOC
-- | Like 'toPlatformStringUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. This is safe to 'unsafePerformIO'/'unsafeDupablePerformIO'.
#else
-- | This mimics the behavior of the base library when doing filesystem
-- operations, which uses shady PEP 383 style encoding (based on the current locale,
-- but PEP 383 only works properly on UTF-8 encodings, so good luck).
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#endif
toPlatformStringFS :: String -> IO PLATFORM_STRING
#ifdef WINDOWS
toPlatformStringFS = pure . WindowsString . encodeWithBaseWindows
#else
toPlatformStringFS = fmap PosixString . encodeWithBasePosix
#endif


#ifdef WINDOWS_DOC
-- | Partial unicode friendly decoding.
--
-- This decodes as UTF16-LE (strictly), which is a pretty good.
--
-- Throws a 'EncodingException' if decoding fails.
#else
-- | Partial unicode friendly decoding.
--
-- This decodes as UTF8 (strictly), which is a good guess. Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
#endif
fromPlatformStringUtf :: MonadThrow m => PLATFORM_STRING -> m String
#ifdef WINDOWS
fromPlatformStringUtf = either throwM pure . fromPlatformStringEnc utf16le
#else
fromPlatformStringUtf = either throwM pure . fromPlatformStringEnc utf8
#endif

-- | Like 'fromPlatformStringUtf', except allows to provide a text encoding.
--
-- The String is forced into memory to catch all exceptions.
fromPlatformStringEnc :: TextEncoding
                      -> PLATFORM_STRING
                      -> Either EncodingException String
#ifdef WINDOWS
fromPlatformStringEnc winEnc (WindowsString ba) = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen winEnc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#else
fromPlatformStringEnc unixEnc (PosixString ba) = unsafePerformIO $ do
  r <- try @SomeException $ BS.useAsCStringLen ba $ \fp -> GHC.peekCStringLen unixEnc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#endif


#ifdef WINDOWS_DOC
-- | Like 'fromPlatformStringUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which does permissive UTF-16 encoding, where coding errors generate
-- Chars in the surrogate range.
--
-- The reason this is in IO is because it unifies with the Posix counterpart,
-- which does require IO. 'unsafePerformIO'/'unsafeDupablePerformIO' are safe, however.
#else
-- | This mimics the behavior of the base library when doing filesystem
-- operations, which uses shady PEP 383 style encoding (based on the current locale,
-- but PEP 383 only works properly on UTF-8 encodings, so good luck).
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
#endif
fromPlatformStringFS :: PLATFORM_STRING -> IO String
#ifdef WINDOWS
fromPlatformStringFS (WindowsString ba) = pure $ decodeWithBaseWindows ba
#else
fromPlatformStringFS (PosixString ba) = decodeWithBasePosix ba
#endif


#ifdef WINDOWS_DOC
-- | Constructs a platform string from a ByteString.
--
-- This ensures valid UCS-2LE.
-- Note that this doesn't expand Word8 to Word16 on windows, so you may get invalid UTF-16.
--
-- Throws 'EncodingException' on invalid UCS-2LE (although unlikely).
#else
-- | Constructs a platform string from a ByteString.
--
-- This is a no-op.
#endif
bytesToPlatformString :: MonadThrow m
                      => ByteString
                      -> m PLATFORM_STRING
#ifdef WINDOWS
bytesToPlatformString bs =
  let ws = WindowsString . toShort $ bs
  in either throwM (const . pure $ ws) $ fromPlatformStringEnc ucs2le ws
#else
bytesToPlatformString = pure . PosixString . toShort
#endif


qq :: (ByteString -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
#ifdef WINDOWS
  { quoteExp  = quoteExp' . fromShort . either (error . show) id . encodeWith (mkUTF16le ErrorOnCodingFailure)
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#else
  { quoteExp  = quoteExp' . fromShort . either (error . show) id . encodeWith (mkUTF8 ErrorOnCodingFailure)
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#endif

mkPlatformString :: ByteString -> Q Exp
mkPlatformString bs =
  case bytesToPlatformString bs of
    Just afp -> lift afp
    Nothing -> error "invalid encoding"

#ifdef WINDOWS_DOC
-- | QuasiQuote a 'WindowsString'. This accepts Unicode characters
-- and encodes as UTF-16 on windows.
#else
-- | QuasiQuote a 'PosixString'. This accepts Unicode characters
-- and encodes as UTF-8 on unix.
#endif
pstr :: QuasiQuoter
pstr = qq mkPlatformString


-- | Unpack a platform string to a list of platform words.
unpackPlatformString :: PLATFORM_STRING -> [PLATFORM_WORD]
#ifdef WINDOWS
unpackPlatformString (WindowsString ba) = WindowsChar <$> BS.unpack ba
#else
unpackPlatformString (PosixString ba) = PosixChar <$> BS.unpack ba
#endif


-- | Pack a list of platform words to a platform string.
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to platform string is probably not what
-- you want, because it will truncate unicode code points.
packPlatformString :: [PLATFORM_WORD] -> PLATFORM_STRING
#ifdef WINDOWS
packPlatformString = WindowsString . BS.pack . fmap (\(WindowsChar w) -> w)
#else
packPlatformString = PosixString . BS.pack . fmap (\(PosixChar w) -> w)
#endif


#ifdef WINDOWS
-- | Truncates to 2 octets.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = WindowsChar . fromIntegral . fromEnum
#else
-- | Truncates to 1 octet.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = PosixChar . fromIntegral . fromEnum
#endif

-- | Converts back to a unicode codepoint (total).
toChar :: PLATFORM_WORD -> Char
#ifdef WINDOWS
toChar (WindowsChar w) = chr $ fromIntegral w
#else
toChar (PosixChar w) = chr $ fromIntegral w
#endif
