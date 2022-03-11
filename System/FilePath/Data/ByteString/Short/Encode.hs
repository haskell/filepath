{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module System.FilePath.Data.ByteString.Short.Encode (encodeUtf8, encodeUtf16LE) where

import Data.Bits
    ( shiftR, (.&.) )
import Data.ByteString.Short
    ( ShortByteString )
import Data.Char
    ( ord )
import Data.Word
    ( Word8 )

import qualified Data.ByteString.Short as BS
    ( pack )


encodeUtf8 :: String -> ShortByteString
encodeUtf8 = BS.pack . encode
  where
    encode :: String -> [Word8]
    encode = concatMap encodeChar

    encodeChar :: Char -> [Word8]
    encodeChar = map fromIntegral . go . ord
     where
      go oc
       | oc <= 0x7f       = [oc]

       | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                            , 0x80 + oc .&. 0x3f
                            ]

       | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                            , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                            , 0x80 + oc .&. 0x3f
                            ]
       | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                            , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                            , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                            , 0x80 + oc .&. 0x3f
                            ]
{-# INLINE encodeUtf8 #-}


encodeUtf16LE :: String -> ShortByteString
encodeUtf16LE = BS.pack . encode
  where
    encode :: String -> [Word8]
    encode = concatMap encodeChar

    encodeChar :: Char -> [Word8]
    encodeChar = map fromIntegral . go . ord
      where
        go oc
          | oc < 0x10000 = [ oc, oc `shiftR` 8 ]
          | otherwise =
            let m = oc - 0x10000
            in [ m `shiftR` 10
               , (m `shiftR` 18) + 0xD8
               , m .&. 0x3FF 
               , ((m .&. 0x3FF) `shiftR` 8) + 0xDC ]
{-# INLINE encodeUtf16LE #-}
