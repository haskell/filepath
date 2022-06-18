{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedFFITypes #-}

module System.OsString.Internal where

import System.OsString.Internal.Types

import Control.Monad.Catch
    ( MonadThrow )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( fromShort )
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import System.IO
    ( TextEncoding )

import System.AbstractFilePath.Encoding ( encodeWith, EncodingException(..) )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import System.OsString.Windows
import qualified System.OsString.Windows as PF
#else
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import System.OsString.Posix
import qualified System.OsString.Posix as PF
#endif




-- | Convert a String.
--
-- On windows this encodes as UTF16-LE, which is a pretty good guess.
-- On unix this encodes as UTF8, which is a good guess.
--
-- Throws a 'EncodingException' if encoding fails.
toOsStringUtf :: MonadThrow m => String -> m OsString
toOsStringUtf = fmap OsString . toPlatformStringUtf

-- | Like 'toOsStringUtf', except allows to provide encodings.
toOsStringEnc :: TextEncoding  -- ^ unix text encoding
              -> TextEncoding  -- ^ windows text encoding
              -> String
              -> Either EncodingException OsString
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toOsStringEnc _ winEnc str = OsString <$> toPlatformStringEnc winEnc str
#else
toOsStringEnc unixEnc _ str = OsString <$> toPlatformStringEnc unixEnc str
#endif

-- | Like 'toOsStringUtf', except on unix this uses the current
-- filesystem locale for encoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
--
-- Throws a 'EncodingException' if decoding fails.
toOsStringFS :: String -> IO OsString
toOsStringFS = fmap OsString . toPlatformStringFS


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16-LE (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
fromOsStringUtf :: MonadThrow m => OsString -> m String
fromOsStringUtf (OsString x) = fromPlatformStringUtf x

-- | Like 'fromOsStringUtf', except allows to provide encodings.
--
-- The String is forced into memory to catch all exceptions.
fromOsStringEnc :: TextEncoding  -- ^ unix text encoding
                -> TextEncoding  -- ^ windows text encoding
                -> OsString
                -> Either EncodingException String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromOsStringEnc _ winEnc (OsString x) = fromPlatformStringEnc winEnc x
#else
fromOsStringEnc unixEnc _ (OsString x) = fromPlatformStringEnc unixEnc x
#endif


-- | Like 'fromOsStringUtf', except on unix this uses the current
-- filesystem locale for decoding instead of always UTF8. On windows, uses UTF-16LE.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
--
-- Throws 'EncodingException' if decoding fails.
fromOsStringFS :: OsString -> IO String
fromOsStringFS (OsString x) = fromPlatformStringFS x


-- | Constructs an @OsString@ from a ByteString.
--
-- On windows, this ensures valid UCS-2LE, on unix it is passed unchanged/unchecked.
--
-- Throws 'EncodingException' on invalid UCS-2LE on windows (although unlikely).
bytesToOsString :: MonadThrow m
                => ByteString
                -> m OsString
bytesToOsString = fmap OsString . bytesToPlatformString


qq :: (ByteString -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
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

mkOsString :: ByteString -> Q Exp
mkOsString bs =
  case bytesToOsString bs of
    Just afp -> lift afp
    Nothing -> error "invalid encoding"

-- | QuasiQuote an 'OsString'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16 on windows.
osstr :: QuasiQuoter
osstr = qq mkOsString


-- | Unpack an 'OsString' to a list of 'OsChar'.
unpackOsString :: OsString -> [OsChar]
unpackOsString (OsString x) = OsChar <$> unpackPlatformString x


-- | Pack a list of 'OsChar' to an 'OsString'
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to 'OsString' is probably not what
-- you want, because it will truncate unicode code points.
packOsString :: [OsChar] -> OsString
packOsString = OsString . packPlatformString . fmap (\(OsChar x) -> x)


-- | Truncates on unix to 1 and on Windows to 2 octets.
unsafeFromChar :: Char -> OsChar
unsafeFromChar = OsChar . PF.unsafeFromChar

-- | Converts back to a unicode codepoint (total).
toChar :: OsChar -> Char
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toChar (OsChar (WW w)) = chr $ fromIntegral w
#else
toChar (OsChar (PW w)) = chr $ fromIntegral w
#endif

