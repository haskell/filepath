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



-- | Convert a String.
--
-- On windows this encodes as UTF16, which is a pretty good guess.
-- On unix this encodes as UTF8, which is a good guess.
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

-- | Like 'toAbstractFilePathUtf', except on unix this uses the current
-- filesystem locale for encoding instead of always UTF8.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
--
-- Throws 'EncodingException' if decoding fails.
toAbstractFilePathFS :: String -> IO AbstractFilePath
toAbstractFilePathFS = toOsStringFS


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16-LE (which is the expected filename encoding).
-- On unix this decodes as UTF8 (which is a good guess). Note that
-- filenames on unix are encoding agnostic char arrays.
--
-- Throws a 'EncodingException' if decoding fails.
--
-- Note that filenames of different encodings may have the same @String@
-- representation, although they're not the same byte-wise.
fromAbstractFilePathUtf :: MonadThrow m => AbstractFilePath -> m String
fromAbstractFilePathUtf = fromOsStringUtf

-- | Like 'fromAbstractFilePathUtf', except on unix this uses the provided
-- 'TextEncoding' for decoding.
--
-- On windows, the TextEncoding parameter is ignored.
fromAbstractFilePathEnc :: TextEncoding  -- ^ unix text encoding
                        -> TextEncoding  -- ^ windows text encoding
                        -> AbstractFilePath
                        -> Either EncodingException String
fromAbstractFilePathEnc = fromOsStringEnc

-- | Like 'fromAbstractFilePathUtf', except on unix this uses the current
-- locale for decoding instead of always UTF8. On windows, uses UTF-16LE.
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
--
-- Throws 'EncodingException' if decoding fails.
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


-- | Unpack an 'AbstractFilePath to a list of 'OsChar'.
unpackAFP :: AbstractFilePath -> [OsChar]
unpackAFP = unpackOsString


-- | Pack a list of 'OsChar' to an 'AbstractFilePath'.
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to 'AbstractFilePath' is probably not what
-- you want, because it will truncate unicode code points.
packAFP :: [OsChar] -> AbstractFilePath
packAFP = packOsString

