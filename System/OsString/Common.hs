{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE TypeApplications #-}
-- This template expects CPP definitions for:
--     MODULE_NAME = Posix | Windows
--     IS_WINDOWS  = False | True

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


import System.AbstractFilePath.Encoding ( encodeWith, EncodingException(..) )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
#ifdef WINDOWS
import System.AbstractFilePath.Encoding
import System.IO
    ( TextEncoding, utf16le )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import System.AbstractFilePath.Data.ByteString.Short.Word16 as BS
import qualified System.AbstractFilePath.Data.ByteString.Short as BS8
#else
import System.IO
    ( TextEncoding, utf8 )
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import System.AbstractFilePath.Data.ByteString.Short as BS
import GHC.IO.Encoding
    ( getFileSystemEncoding )
#endif



-- | Convert a String.
--
-- On windows this encodes as UTF16, which is a pretty good guess.
-- On unix this encodes as UTF8, which is a good guess.
--
-- Throws a 'EncodingException' if encoding fails.
toPlatformStringUtf :: MonadThrow m => String -> m PLATFORM_STRING
#ifdef WINDOWS
toPlatformStringUtf str = either throwM pure $ toPlatformStringEnc str utf16le
#else
toPlatformStringUtf str = either throwM pure $ toPlatformStringEnc str utf8
#endif

-- | Like 'toPlatformStringUtf', except allows to provide an encoding.
toPlatformStringEnc :: String
                    -> TextEncoding
                    -> Either EncodingException PLATFORM_STRING
toPlatformStringEnc str enc = unsafePerformIO $ do
#ifdef WINDOWS
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> WS <$> BS8.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#else
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> PS <$> BS.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#endif

-- | Like 'toPlatformStringUtf', except on unix this uses the current
-- filesystem locale for encoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
--
-- Throws a 'EncodingException' if encoding fails.
toPlatformStringFS :: String -> IO PLATFORM_STRING
#ifdef WINDOWS
toPlatformStringFS str = GHC.withCStringLen utf16le str $ \cstr -> WS <$> BS8.packCStringLen cstr
#else
toPlatformStringFS str = do
  enc <- getFileSystemEncoding
  GHC.withCStringLen enc str $ \cstr -> PS <$> BS.packCStringLen cstr
#endif


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16-LE (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
fromPlatformStringUtf :: MonadThrow m => PLATFORM_STRING -> m String
#ifdef WINDOWS
fromPlatformStringUtf ps = either throwM pure (fromPlatformStringEnc ps utf16le)
#else
fromPlatformStringUtf ps = either throwM pure (fromPlatformStringEnc ps utf8)
#endif

-- | Like 'fromPlatformStringUtf', except allows to provide a text encoding.
--
-- The String is forced into memory to catch all exceptions.
fromPlatformStringEnc :: PLATFORM_STRING
                      -> TextEncoding
                      -> Either EncodingException String
#ifdef WINDOWS
fromPlatformStringEnc (WS ba) winEnc = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen winEnc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#else
fromPlatformStringEnc (PS ba) unixEnc = unsafePerformIO $ do
  r <- try @SomeException $ BS.useAsCStringLen ba $ \fp -> GHC.peekCStringLen unixEnc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r
#endif


-- | Like 'fromPlatformStringUt', except on unix this uses the current
-- filesystem locale for decoding instead of always UTF8. On windows, uses UTF-16LE.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
--
-- Throws 'EncodingException' if decoding fails.
fromPlatformStringFS :: PLATFORM_STRING -> IO String
#ifdef WINDOWS
fromPlatformStringFS (WS ba) =
  BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen utf16le fp
#else
fromPlatformStringFS (PS ba) = do
  enc <- getFileSystemEncoding
  BS.useAsCStringLen ba $ \fp -> GHC.peekCStringLen enc fp
#endif


-- | Constructs a platform string from a ByteString.
--
-- On windows, this ensures valid UCS-2LE, on unix it is passed unchecked.
-- Note that this doesn't expand Word8 to Word16 on windows, so you may get invalid UTF-16.
--
-- Throws 'EncodingException' on invalid UCS-2LE on windows (although unlikely).
bytesToPlatformString :: MonadThrow m
                      => ByteString
                      -> m PLATFORM_STRING
#ifdef WINDOWS
bytesToPlatformString bs =
  let ws = WS . toShort $ bs
  in either throwM (const . pure $ ws) $ fromPlatformStringEnc ws ucs2le
#else
bytesToPlatformString = pure . PS . toShort
#endif


qq :: (ByteString -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
#ifdef WINDOWS
  { quoteExp  = quoteExp' . fromShort . either (error . show) id . encodeWith (mkUTF16le TransliterateCodingFailure)
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#else
  { quoteExp  = quoteExp' . fromShort . either (error . show) id . encodeWith (mkUTF8 TransliterateCodingFailure)
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

#ifdef WINDOWS
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
unpackPlatformString (WS ba) = WW <$> BS.unpack ba
#else
unpackPlatformString (PS ba) = PW <$> BS.unpack ba
#endif


-- | Pack a list of platform words to a platform string.
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to platform string is probably not what
-- you want, because it will truncate unicode code points.
packPlatformString :: [PLATFORM_WORD] -> PLATFORM_STRING
#ifdef WINDOWS
packPlatformString = WS . BS.pack . fmap (\(WW w) -> w)
#else
packPlatformString = PS . BS.pack . fmap (\(PW w) -> w)
#endif


#ifdef WINDOWS
-- | Truncates to 2 octets.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = WW . fromIntegral . fromEnum
#else
-- | Truncates to 1 octet.
unsafeFromChar :: Char -> PLATFORM_WORD
unsafeFromChar = PW . fromIntegral . fromEnum
#endif

-- | Converts back to a unicode codepoint (total).
toChar :: PLATFORM_WORD -> Char
#ifdef WINDOWS
toChar (WW w) = chr $ fromIntegral w
#else
toChar (PW w) = chr $ fromIntegral w
#endif
