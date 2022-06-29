{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedFFITypes #-}

module System.OsPath.Internal where

import {-# SOURCE #-} System.OsPath
    ( isValid )
import System.OsPath.Types
import System.OsString.Internal hiding ( fromBytes )
import qualified System.OsString.Internal as OS

import Control.Monad.Catch
    ( MonadThrow )
import Data.ByteString
    ( ByteString )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import System.IO
    ( TextEncoding )
import System.OsPath.Encoding ( EncodingException(..) )



-- | Partial unicode friendly encoding.
--
-- On windows this encodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this encodes as UTF8 (strictly), which is a good guess.
--
-- Throws a 'EncodingException' if encoding fails.
encodeUtf :: MonadThrow m => FilePath -> m OsPath
encodeUtf = OS.encodeUtf

-- | Encode a 'FilePath' with the specified encoding.
encodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding
           -> FilePath
           -> Either EncodingException OsPath
encodeWith = OS.encodeWith

-- | Like 'encodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
encodeFS :: FilePath -> IO OsPath
encodeFS = OS.encodeFS


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this decodes as UTF8 (strictly), which is a good guess.
--
-- Throws a 'EncodingException' if decoding fails.
decodeUtf :: MonadThrow m => OsPath -> m FilePath
decodeUtf = OS.decodeUtf

-- | Decode an 'OsPath' with the specified encoding.
decodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding
           -> OsPath
           -> Either EncodingException FilePath
decodeWith = OS.decodeWith

-- | Like 'decodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
decodeFS :: OsPath -> IO FilePath
decodeFS = OS.decodeFS


-- | Constructs an @OsPath@ from a ByteString.
--
-- On windows, this ensures valid UCS-2LE, on unix it is passed unchanged/unchecked.
--
-- Throws 'EncodingException' on invalid UCS-2LE on windows (although unlikely).
fromBytes :: MonadThrow m
          => ByteString
          -> m OsPath
fromBytes = OS.fromBytes


mkOsPath :: ByteString -> Q Exp
mkOsPath bs =
  case fromBytes bs of
    Just afp' ->
      if isValid afp'
      then lift afp'
      else error "invalid filepath"
    Nothing -> error "invalid encoding"

-- | QuasiQuote an 'OsPath'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16 on windows. Runs 'filepathIsValid'
-- on the input.
osp :: QuasiQuoter
osp = qq mkOsPath


-- | Unpack an 'OsPath' to a list of 'OsChar'.
unpack :: OsPath -> [OsChar]
unpack = OS.unpack


-- | Pack a list of 'OsChar' to an 'OsPath'.
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to 'OsPath' is probably not what
-- you want, because it will truncate unicode code points.
pack :: [OsChar] -> OsPath
pack = OS.pack

