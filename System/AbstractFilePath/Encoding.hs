{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
  #-}
{-# OPTIONS_GHC  -funbox-strict-fields #-}


module System.AbstractFilePath.Encoding where

import GHC.Base
import GHC.Real
import GHC.Num
-- import GHC.IO
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import Data.Bits

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

