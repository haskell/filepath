{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , TypeApplications
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}


module System.AbstractFilePath.Encoding where

import qualified System.AbstractFilePath.Data.ByteString.Short as BS8

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
import Data.Word (Word8)
import Data.Data (Typeable)
import GHC.Show (Show (show))
import Numeric (showHex)

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
-- Utils
--

decodeWith :: TextEncoding -> BS8.ShortByteString -> Either EncodingException String
decodeWith enc ba = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen enc fp
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r

encodeWith :: TextEncoding -> String -> Either EncodingException BS8.ShortByteString
encodeWith enc str = unsafePerformIO $ do
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> BS8.packCStringLen cstr
  evaluate $ force $ first (flip EncodingError Nothing . displayException) r

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


