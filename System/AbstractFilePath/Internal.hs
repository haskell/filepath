{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedFFITypes #-}

module System.AbstractFilePath.Internal where

import {-# SOURCE #-} System.AbstractFilePath
    ( isValid )
import System.AbstractFilePath.Types
import System.OsString.Internal
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
import System.AbstractFilePath.Encoding ( EncodingException(..) )



-- | Partial unicode friendly encoding.
--
-- On windows this encodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this encodes as UTF8 (strictly), which is a good guess.
--
-- Throws a 'EncodingException' if encoding fails.
toAbstractFilePathUtf :: MonadThrow m => String -> m AbstractFilePath
toAbstractFilePathUtf = toOsStringUtf

-- | Like 'toAbstractFilePathUtf', except allows to provide encodings.
toAbstractFilePathEnc :: TextEncoding  -- ^ unix text encoding
                      -> TextEncoding  -- ^ windows text encoding
                      -> String
                      -> Either EncodingException AbstractFilePath
toAbstractFilePathEnc = toOsStringEnc

-- | Like 'toAbstractFilePathUtf', except this mimics the behavior of the base library when doing filesystem
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
toAbstractFilePathFS :: String -> IO AbstractFilePath
toAbstractFilePathFS = toOsStringFS


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this decodes as UTF8 (strictly), which is a good guess. Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
--
-- Note that filenames of different encodings may have the same @String@
-- representation, although they're not the same byte-wise.
fromAbstractFilePathUtf :: MonadThrow m => AbstractFilePath -> m String
fromAbstractFilePathUtf = fromOsStringUtf

-- | Like 'fromAbstractFilePathUtf', except allows to provide encodings.
fromAbstractFilePathEnc :: TextEncoding  -- ^ unix text encoding
                        -> TextEncoding  -- ^ windows text encoding
                        -> AbstractFilePath
                        -> Either EncodingException String
fromAbstractFilePathEnc = fromOsStringEnc

-- | Like 'fromAbstractFilePathUtf', except this mimics the behavior of the base library when doing filesystem
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
fromAbstractFilePathFS :: AbstractFilePath -> IO String
fromAbstractFilePathFS = fromOsStringFS


-- | Constructs an @AbstractFilePath@ from a ByteString.
--
-- On windows, this ensures valid UCS-2LE, on unix it is passed unchanged/unchecked.
--
-- Throws 'EncodingException' on invalid UCS-2LE on windows (although unlikely).
bytesToAFP :: MonadThrow m
           => ByteString
           -> m AbstractFilePath
bytesToAFP = OS.bytesToOsString


mkAbstractFilePath :: ByteString -> Q Exp
mkAbstractFilePath bs =
  case bytesToAFP bs of
    Just afp' ->
      if isValid afp'
      then lift afp'
      else error "invalid filepath"
    Nothing -> error "invalid encoding"

-- | QuasiQuote an 'AbstractFilePath'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16 on windows. Runs 'filepathIsValid'
-- on the input.
afp :: QuasiQuoter
afp = qq mkAbstractFilePath


-- | Unpack an 'AbstractFilePath' to a list of 'OsChar'.
unpackAFP :: AbstractFilePath -> [OsChar]
unpackAFP = unpackOsString


-- | Pack a list of 'OsChar' to an 'AbstractFilePath'.
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to 'AbstractFilePath' is probably not what
-- you want, because it will truncate unicode code points.
packAFP :: [OsChar] -> AbstractFilePath
packAFP = packOsString

