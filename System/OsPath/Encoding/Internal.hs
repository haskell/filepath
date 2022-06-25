{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , TypeApplications
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}


module System.OsPath.Encoding.Internal where

import qualified System.OsPath.Data.ByteString.Short as BS8
import qualified System.OsPath.Data.ByteString.Short.Word16 as BS16

import GHC.Base
import GHC.Real
import GHC.Num
-- import GHC.IO
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import Data.Bits
import Control.Exception (SomeException, try, Exception (displayException), evaluate)
import qualified GHC.Foreign as GHC
import Data.Either (Either)
import GHC.IO (unsafePerformIO)
import Control.DeepSeq (force, NFData (rnf))
import Data.Bifunctor (first)
import Data.Data (Typeable)
import GHC.Show (Show (show))
import Numeric (showHex)
import Foreign.C (CString, CStringLen)
import Data.Char (chr)
import Foreign
import Prelude (FilePath)
import GHC.IO.Encoding (getFileSystemEncoding)

-- -----------------------------------------------------------------------------
-- UCS-2 LE
--

ucs2le :: TextEncoding
ucs2le = mkUcs2le ErrorOnCodingFailure

mkUcs2le :: CodingFailureMode -> TextEncoding
mkUcs2le cfm = TextEncoding { textEncodingName = "UCS-2LE",
                              mkTextDecoder = ucs2le_DF cfm,
                              mkTextEncoder = ucs2le_EF cfm }

ucs2le_DF :: CodingFailureMode -> IO (TextDecoder ())
ucs2le_DF cfm =
  return (BufferCodec {
             encode   = ucs2le_decode,
             recover  = recoverDecode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })

ucs2le_EF :: CodingFailureMode -> IO (TextEncoder ())
ucs2le_EF cfm =
  return (BufferCodec {
             encode   = ucs2le_encode,
             recover  = recoverEncode cfm,
             close    = return (),
             getState = return (),
             setState = const $ return ()
          })


ucs2le_decode :: DecodeBuffer
ucs2le_decode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
       loop !ir !ow
         | ow >= os     = done OutputUnderflow ir ow
         | ir >= iw     = done InputUnderflow ir ow
         | ir + 1 == iw = done InputUnderflow ir ow
         | otherwise = do
              c0 <- readWord8Buf iraw ir
              c1 <- readWord8Buf iraw (ir+1)
              let x1 = fromIntegral c1 `shiftL` 8 + fromIntegral c0
              ow' <- writeCharBuf oraw ow (unsafeChr x1)
              loop (ir+2) ow'

       -- lambda-lifted, to avoid thunks being built in the inner-loop:
       done why !ir !ow = return (why,
                                  if ir == iw then input{ bufL=0, bufR=0 }
                                              else input{ bufL=ir },
                                  output{ bufR=ow })
    in
    loop ir0 ow0


ucs2le_encode :: EncodeBuffer
ucs2le_encode
  input@Buffer{  bufRaw=iraw, bufL=ir0, bufR=iw,  bufSize=_  }
  output@Buffer{ bufRaw=oraw, bufL=_,   bufR=ow0, bufSize=os }
 = let
      done why !ir !ow = return (why,
                                 if ir == iw then input{ bufL=0, bufR=0 }
                                             else input{ bufL=ir },
                                 output{ bufR=ow })
      loop !ir !ow
        | ir >= iw     =  done InputUnderflow ir ow
        | os - ow < 2  =  done OutputUnderflow ir ow
        | otherwise = do
           (c,ir') <- readCharBuf iraw ir
           case ord c of
             x | x < 0x10000 -> do
                     writeWord8Buf oraw ow     (fromIntegral x)
                     writeWord8Buf oraw (ow+1) (fromIntegral (x `shiftR` 8))
                     loop ir' (ow+2)
               | otherwise -> done InvalidSequence ir ow
    in
    loop ir0 ow0


-- -----------------------------------------------------------------------------
-- Windows encoding (ripped off from base)
--



-- On Windows, wchar_t is 16 bits wide and CWString uses the UTF-16 encoding.

-- coding errors generate Chars in the surrogate range
cWcharsToChars :: [Word16] -> [Char]
cWcharsToChars = map chr . fromUTF16 . map fromIntegral
 where
  fromUTF16 :: [Int] -> [Int]
  fromUTF16 (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c:wcs) = c : fromUTF16 wcs
  fromUTF16 [] = []

charsToCWchars :: [Char] -> [Word16]
charsToCWchars = foldr (utf16Char . ord) []
 where
  utf16Char :: Int -> [Word16] -> [Word16]
  utf16Char c wcs
    | c < 0x10000 = fromIntegral c : wcs
    | otherwise   = let c' = c - 0x10000 in
                    fromIntegral (c' `div` 0x400 + 0xd800) :
                    fromIntegral (c' `mod` 0x400 + 0xdc00) : wcs

-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- FFI
--

-- | Marshal a Haskell string into a NUL terminated C wide string using
-- temporary storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCWString :: String -> (Ptr Word16 -> IO a) -> IO a
withCWString  = withArray0 wNUL . charsToCWchars

peekCWString    :: Ptr Word16 -> IO String
peekCWString cp  = do
  cs <- peekArray0 wNUL cp
  return (cWcharsToChars cs)

withFilePathWin :: FilePath -> (Ptr Word16 -> IO a) -> IO a
withFilePathWin = withCWString

peekFilePathWin :: Ptr Word16 -> IO FilePath
peekFilePathWin = peekCWString

withFilePathPosix :: FilePath -> (CString -> IO a) -> IO a
withFilePathPosix fp f = getFileSystemEncoding >>= \enc -> GHC.withCString enc fp f

peekFilePathLenPosix :: CStringLen -> IO FilePath
peekFilePathLenPosix fp = getFileSystemEncoding >>= \enc -> GHC.peekCStringLen enc fp


-- -----------------------------------------------------------------------------
-- Encoders / decoders
--

-- | Decode with the given 'TextEncoding'.
decodeWith :: TextEncoding -> BS8.ShortByteString -> Either EncodingException String
decodeWith enc ba = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen enc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r

-- | Encode with the given 'TextEncoding'.
encodeWith :: TextEncoding -> String -> Either EncodingException BS8.ShortByteString
encodeWith enc str = unsafePerformIO $ do
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> BS8.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r

-- | This mimics the filepath ddecoder base uses on unix.
decodeWithBasePosix :: BS8.ShortByteString -> IO String
decodeWithBasePosix ba = BS8.useAsCStringLen ba $ \fp -> peekFilePathLenPosix fp

-- | This mimics the filepath dencoder base uses on unix.
encodeWithBasePosix :: String -> IO BS8.ShortByteString
encodeWithBasePosix str = withFilePathPosix str $ \cstr -> BS8.packCString cstr

-- | This mimics the filepath decoder base uses on windows.
decodeWithBaseWindows :: BS16.ShortByteString -> String
decodeWithBaseWindows = cWcharsToChars . BS16.unpack

-- | This mimics the filepath dencoder base uses on windows.
encodeWithBaseWindows :: String -> BS8.ShortByteString
encodeWithBaseWindows = BS16.pack . charsToCWchars


-- -----------------------------------------------------------------------------
-- Types
--

data EncodingException =
    EncodingError String (Maybe Word8)
    -- ^ Could not decode a byte sequence because it was invalid under
    -- the given encoding, or ran out of input in mid-decode.
    deriving (Eq, Typeable)


showEncodingException :: EncodingException -> String
showEncodingException (EncodingError desc (Just w))
    = "Cannot decode byte '\\x" ++ showHex w ("': " ++ desc)
showEncodingException (EncodingError desc Nothing)
    = "Cannot decode input: " ++ desc

instance Show EncodingException where
    show = showEncodingException

instance Exception EncodingException

instance NFData EncodingException where
    rnf (EncodingError desc w) = rnf desc `seq` rnf w


-- -----------------------------------------------------------------------------
-- Words
--

wNUL :: Word16
wNUL = 0x00
