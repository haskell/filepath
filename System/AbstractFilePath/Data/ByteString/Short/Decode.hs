{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module System.AbstractFilePath.Data.ByteString.Short.Decode (decodeUtf16LE, decodeUtf16LEWith, decodeUtf16LE', decodeUtf16LE'', decodeUtf8, decodeUtf8With, decodeUtf8', strictDecode, lenientDecode, OnError, OnDecodeError, UnicodeException(..)) where

import Data.ByteString.Internal
    ( ByteString )
import Data.ByteString.Short
    ( ShortByteString, toShort )
import Data.Bits
    ( shiftL )
import Data.Typeable (Typeable)
import Data.Word
    ( Word16, Word8 )
import GHC.Word (Word8(..), Word16(..))
import Control.Exception
import qualified Data.ByteString.Short as BS
    ( index, length )
import GHC.IO
    ( unsafeDupablePerformIO )
import GHC.Base
import Control.DeepSeq (NFData, rnf)
import Numeric (showHex)
#if !MIN_VERSION_base(4,16,0)

word8ToWord#  :: Word# -> Word#
word16ToWord# :: Word# -> Word#
word8ToWord#  w = w
word16ToWord# w = w
{-# INLINE word16ToWord# #-}
{-# INLINE word8ToWord# #-}

#endif

unsafeChr16 :: Word16 -> Char
unsafeChr16 (W16# w#) = C# (chr# (word2Int# (word16ToWord# w#)))
{-# INLINE unsafeChr16 #-}

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# (word8ToWord# w#)))
{-# INLINE unsafeChr8 #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using UTF-8
-- encoding.
streamUtf8 :: OnDecodeError -> ShortByteString -> [Char]
streamUtf8 onErr bs = go 0
    where
      l = BS.length bs
      go i
          | i >= l = []
          | validate1_8 x1 = unsafeChr8 x1 : go (i+1)
          | i+1 < l && validate2_8 x1 x2 = chr2 x1 x2 : go (i+2)
          | i+2 < l && validate3_8 x1 x2 x3 = chr3 x1 x2 x3 : go (i+3)
          | i+3 < l && validate4_8 x1 x2 x3 x4 = chr4 x1 x2 x3 x4 : go (i+4)
          | otherwise = decodeError "streamUtf8" "UTF-8" onErr (Just x1) ++ go (i+1)
          where
            x1 = idx i
            x2 = idx (i + 1)
            x3 = idx (i + 2)
            x4 = idx (i + 3)
            idx = BS.index bs
{-# INLINE [0] streamUtf8 #-}

-- | /O(n)/ Convert a 'ShortByteString' into a 'Stream Char', using little
-- endian UTF-16 encoding.
streamUtf16LE :: OnDecodeError -> ShortByteString -> [Char]
streamUtf16LE onErr bs = go 0
    where
      l = BS.length bs
      {-# INLINE go #-}
      go i
          | i >= l                         = []
          | i+1 < l && validate1_16 x1    = unsafeChr16 x1 : go (i+2)
          | i+3 < l && validate2_16 x1 x2 = chr2_16 x1 x2 : go (i+4)
          | otherwise = decodeError "streamUtf16LE" "UTF-16LE" onErr Nothing ++ go (i+1)
          where
            x1    = idx i       + (idx (i + 1) `shiftL` 8)
            x2    = idx (i + 2) + (idx (i + 3) `shiftL` 8)
            idx = fromIntegral . BS.index bs :: Int -> Word16
{-# INLINE [0] streamUtf16LE #-}

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> ShortByteString -> String
decodeUtf16LEWith = streamUtf16LE
{-# INLINE decodeUtf16LEWith #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf16LE :: ShortByteString -> String
decodeUtf16LE = decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE #-}

-- | Decode text from little endian UTF-16 encoding.
decodeUtf8With :: OnDecodeError -> ShortByteString -> String
decodeUtf8With = streamUtf8
{-# INLINE decodeUtf8With #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf8 :: ShortByteString -> String
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE decodeUtf8 #-}


decodeError :: String -> String -> OnDecodeError -> Maybe Word8 -> String
decodeError func kind onErr mb =
    case onErr desc mb of
      Nothing -> []
      Just c  -> [c]
    where desc = "System.AbstractFilePath.Data.ByteString.Short.Decode." ++ func ++ ": Invalid " ++
                 kind ++ " stream"


-- | Decode a 'ShortByteString' containing UTF-8 encoded text.
--
-- If the input contains any invalid UTF-8 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf8' :: ShortByteString -> Either UnicodeException String
decodeUtf8' = unsafeDupablePerformIO . try . evaluate . decodeUtf8With strictDecode
{-# INLINE decodeUtf8' #-}


-- | Decode a 'ShortByteString' containing UTF-16 encoded text.
--
-- If the input contains any invalid UTF-16 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf16LE' :: ShortByteString -> Either UnicodeException String
decodeUtf16LE' = unsafeDupablePerformIO . try . evaluate . decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE' #-}


-- | Decode a 'ByteString' containing UTF-16 encoded text.
--
-- If the input contains any invalid UTF-16 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf16LE'' :: ByteString -> Either UnicodeException String
decodeUtf16LE'' = unsafeDupablePerformIO . try . evaluate . decodeUtf16LEWith strictDecode . toShort
{-# INLINE decodeUtf16LE'' #-}

-- | Throw a 'UnicodeException' if decoding fails.
strictDecode :: OnDecodeError
strictDecode desc c = throw (DecodeError desc c)

-- | Replace an invalid input byte with the Unicode replacement
-- character U+FFFD.
lenientDecode :: OnDecodeError
lenientDecode _ _ = Just '\xfffd'


type OnError a b = String -> Maybe a -> Maybe b
type OnDecodeError = OnError Word8 Char
-- | An exception type for representing Unicode encoding errors.
data UnicodeException =
    DecodeError String (Maybe Word8)
    -- ^ Could not decode a byte sequence because it was invalid under
    -- the given encoding, or ran out of input in mid-decode.
    deriving (Eq, Typeable)

-------------------------------
-- Internal


between :: Word8                -- ^ byte to check
        -> Word8                -- ^ lower bound
        -> Word8                -- ^ upper bound
        -> Bool
between x y z = x >= y && x <= z
{-# INLINE between #-}


validate1_8 :: Word8 -> Bool
validate1_8 x1 = x1 <= 0x7F
{-# INLINE validate1_8 #-}

validate2_8 :: Word8 -> Word8 -> Bool
validate2_8 x1 x2 = between x1 0xC2 0xDF && between x2 0x80 0xBF
{-# INLINE validate2_8 #-}

validate3_8 :: Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate3_8 #-}
validate3_8 x1 x2 x3 = validate3_1 || validate3_2 || validate3_3 || validate3_4
  where
    validate3_1 = (x1 == 0xE0) &&
                  between x2 0xA0 0xBF &&
                  between x3 0x80 0xBF
    validate3_2 = between x1 0xE1 0xEC &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF
    validate3_3 = x1 == 0xED &&
                  between x2 0x80 0x9F &&
                  between x3 0x80 0xBF
    validate3_4 = between x1 0xEE 0xEF &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF

validate4_8 :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate4_8 #-}
validate4_8 x1 x2 x3 x4 = validate4_1 || validate4_2 || validate4_3
  where
    validate4_1 = x1 == 0xF0 &&
                  between x2 0x90 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_2 = between x1 0xF1 0xF3 &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_3 = x1 == 0xF4 &&
                  between x2 0x80 0x8F &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF

chr2 :: Word8 -> Word8 -> Char
chr2 (W8# x1#) (W8# x2#) = C# (chr# (z1# +# z2#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> Word8 -> Word8 -> Char
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = C# (chr# (z1# +# z2# +# z3#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !y3# = word2Int# (word8ToWord# x3#)
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !y3# = word2Int# (word8ToWord# x3#)
      !y4# = word2Int# (word8ToWord# x4#)
      !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
      !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
      !z4# = y4# -# 0x80#
{-# INLINE chr4 #-}

validate1_16 :: Word16 -> Bool
validate1_16 x1 = x1 < 0xD800 || x1 > 0xDFFF
{-# INLINE validate1_16 #-}

validate2_16 ::  Word16 -> Word16 -> Bool
validate2_16 x1 x2 = x1 >= 0xD800 && x1 <= 0xDBFF &&
                  x2 >= 0xDC00 && x2 <= 0xDFFF
{-# INLINE validate2_16 #-}

chr2_16 :: Word16 -> Word16 -> Char
chr2_16 (W16# a#) (W16# b#) = C# (chr# (upper# +# lower# +# 0x10000#))
    where
      !x# = word2Int# (word16ToWord# a#)
      !y# = word2Int# (word16ToWord# b#)
      !upper# = uncheckedIShiftL# (x# -# 0xD800#) 10#
      !lower# = y# -# 0xDC00#
{-# INLINE chr2_16 #-}


showUnicodeException :: UnicodeException -> String
showUnicodeException (DecodeError desc (Just w))
    = "Cannot decode byte '\\x" ++ showHex w ("': " ++ desc)
showUnicodeException (DecodeError desc Nothing)
    = "Cannot decode input: " ++ desc

instance Show UnicodeException where
    show = showUnicodeException

instance Exception UnicodeException

instance NFData UnicodeException where
    rnf (DecodeError desc w) = rnf desc `seq` rnf w
