{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
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
  , toPlatformString
  , toPlatformStringIO
  , bsToPlatformString
  , pstr
  , packPlatformString

  -- * String deconstruction
  , fromPlatformString
#ifndef WINDOWS
  , fromPlatformStringEnc
#endif
  , fromPlatformStringIO
  , unpackPlatformString

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar
  )
where


import System.OsString.Internal.Types
#ifdef WINDOWS
  ( WindowsString
  , WindowsChar
  )
#else
  ( PosixString
  , PosixChar
  )
#endif

import System.AbstractFilePath.Data.ByteString.Short.Encode
  ( 
#ifdef WINDOWS
    encodeUtf16LE
#else
    encodeUtf8
#endif
  )
import System.AbstractFilePath.Data.ByteString.Short.Decode
    (
#ifdef WINDOWS
      decodeUtf16LE'
    , decodeUtf16LE''
#else
      decodeUtf8'
#endif
    )
import System.OsString.Internal.Types (
#ifdef WINDOWS
  WindowsString(..), WindowsChar(..)
#else
  PosixString(..), PosixChar(..)
#endif
  )

import Data.Char
import Control.Exception
    ( throwIO )
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Data.ByteString.Internal
    ( ByteString )
#ifndef WINDOWS
import Control.Exception
    ( SomeException, try, displayException )
import Control.DeepSeq ( force )
import Data.Bifunctor ( first )
import GHC.IO
    ( evaluate, unsafePerformIO )
import GHC.IO.Encoding
    ( getFileSystemEncoding )
import qualified GHC.Foreign as GHC
import System.IO.Error
    ( catchIOError )
import System.IO
    ( TextEncoding )
import System.AbstractFilePath.Data.ByteString.Short.Decode
    (
      UnicodeException (..)
    )
#endif
import Language.Haskell.TH
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )


#ifdef WINDOWS
import System.AbstractFilePath.Data.ByteString.Short.Word16 as BS
#else
import System.AbstractFilePath.Data.ByteString.Short as BS
#endif



-- | Total Unicode-friendly encoding.
--
-- On windows this encodes as UTF16, which is expected.
-- On unix this encodes as UTF8, which is a good guess.
toPlatformString :: String -> PLATFORM_STRING
#ifdef WINDOWS
toPlatformString = WS . encodeUtf16LE
#else
toPlatformString = PS . encodeUtf8
#endif

-- | Like 'toPlatformString', except on unix this uses the current
-- locale for encoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
toPlatformStringIO :: String -> IO PLATFORM_STRING
#ifdef WINDOWS
toPlatformStringIO = pure . WS . encodeUtf16LE
#else
toPlatformStringIO str = do
  enc <- getFileSystemEncoding
  cstr <- GHC.newCString enc str
  PS <$> BS.packCString cstr
#endif


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16 (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'UnicodeException' if decoding fails.
fromPlatformString :: MonadThrow m => PLATFORM_STRING -> m String
#ifdef WINDOWS
fromPlatformString (WS ba) = either throwM pure $ decodeUtf16LE' ba
#else
fromPlatformString (PS ba) = either throwM pure $ decodeUtf8' ba
#endif

#ifndef WINDOWS
-- | Like 'fromPlatformString', except on unix this uses the provided
-- 'TextEncoding' for decoding.
fromPlatformStringEnc :: PLATFORM_STRING -> TextEncoding -> Either UnicodeException String
fromPlatformStringEnc (PS ba) enc = unsafePerformIO $ do
  r <- try @SomeException $ BS.useAsCString ba $ \fp -> GHC.peekCString enc fp
  r' <- evaluate $ force $ first (flip DecodeError Nothing . displayException) $ r
  pure r'
#endif


-- | Like 'fromPlatformString', except on unix this uses the current
-- locale for decoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible.
--
-- Throws 'UnicodeException' if decoding fails.
fromPlatformStringIO :: PLATFORM_STRING -> IO String
#ifdef WINDOWS

fromPlatformStringIO (WS ba) = either throwIO pure $ decodeUtf16LE' ba
#else
fromPlatformStringIO (PS ba) = flip catchIOError (\_ -> throwIO (DecodeError "fromAbstractFilePath' failed" Nothing))
  $ BS.useAsCString ba $ \fp -> getFileSystemEncoding >>= \enc -> GHC.peekCString enc fp
#endif


-- | Constructs an platform string from a ByteString.
--
-- On windows, this ensures valid UTF16, on unix it is passed unchanged/unchecked.
--
-- Throws 'UnicodeException' on invalid UTF16 on windows.
bsToPlatformString :: MonadThrow m
             => ByteString
             -> m PLATFORM_STRING
#ifdef WINDOWS
bsToPlatformString bs =
  either throwM (const . pure . WS . toShort $ bs) $ decodeUtf16LE'' bs
#else
bsToPlatformString = pure . PS . toShort
#endif


qq :: (ByteString -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
#ifdef WINDOWS
  { quoteExp  = (\s -> quoteExp' . fromShort . encodeUtf16LE $ s)
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }
#else
  { quoteExp  = (\s -> quoteExp' . fromShort . encodeUtf8 $ s)
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
  case bsToPlatformString bs of
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


unpackPlatformString :: PLATFORM_STRING -> [PLATFORM_WORD]
#ifdef WINDOWS
unpackPlatformString (WS ba) = fmap WW $ BS.unpack ba
#else
unpackPlatformString (PS ba) = fmap PW $ BS.unpack ba
#endif


packPlatformString :: [PLATFORM_WORD] -> PLATFORM_STRING
#ifdef WINDOWS
packPlatformString ws = WS . BS.pack . fmap (\(WW w) -> w) $ ws
#else
packPlatformString ws = PS . BS.pack . fmap (\(PW w) -> w) $ ws
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
