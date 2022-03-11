{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module System.FilePath.Data.ByteString.Short.Decode (decodeUtf16LE, decodeUtf16LEWith, decodeUtf16LE', decodeUtf16LE'', decodeUtf8, decodeUtf8With, decodeUtf8') where

import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( ShortByteString )
import Data.Text
    ( Text )
import Data.Text.Encoding.Error
    ( OnDecodeError, UnicodeException, strictDecode )
import Data.Text.Internal.Fusion.Types
    ( Step (..), Stream (..) )
import Data.Text.Internal.Fusion.Size
    ( maxSize )
import Data.Text.Internal.Unsafe.Char
#if MIN_VERSION_text(2,0,0)
    ( unsafeChr16, unsafeChr8 )
#else
    ( unsafeChr, unsafeChr8 )
#endif
import Data.Bits
    ( unsafeShiftL, unsafeShiftR )
import Data.Word
    ( Word16, Word8 )

import Control.Exception
    ( evaluate, try )
import qualified Data.ByteString.Short as BS
    ( index, length )
import qualified Data.Text.Encoding as E
import qualified Data.Text.Internal.Encoding.Utf16 as U16
import qualified Data.Text.Internal.Encoding.Utf8 as U8
import GHC.IO
    ( unsafeDupablePerformIO )

#if !MIN_VERSION_text(2,0,0)
unsafeChr16 :: Word16 -> Char
unsafeChr16 = unsafeChr
#endif

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using UTF-8
-- encoding.
streamUtf8 :: OnDecodeError -> ShortByteString -> Stream Char
streamUtf8 onErr bs = Stream next 0 (maxSize l)
    where
      l = BS.length bs
      next i
          | i >= l = Done
          | U8.validate1 x1 = Yield (unsafeChr8 x1) (i+1)
          | i+1 < l && U8.validate2 x1 x2 = Yield (U8.chr2 x1 x2) (i+2)
          | i+2 < l && U8.validate3 x1 x2 x3 = Yield (U8.chr3 x1 x2 x3) (i+3)
          | i+3 < l && U8.validate4 x1 x2 x3 x4 = Yield (U8.chr4 x1 x2 x3 x4) (i+4)
          | otherwise = decodeError "streamUtf8" "UTF-8" onErr (Just x1) (i+1)
          where
            x1 = idx i
            x2 = idx (i + 1)
            x3 = idx (i + 2)
            x4 = idx (i + 3)
            idx = BS.index bs
{-# INLINE [0] streamUtf8 #-}

-- | /O(n)/ Convert a 'ShortByteString' into a 'Stream Char', using little
-- endian UTF-16 encoding.
streamUtf16LE :: OnDecodeError -> ShortByteString -> Stream Char
streamUtf16LE onErr bs = Stream next 0 (maxSize (l `unsafeShiftR` 1))
    where
      l = BS.length bs
      {-# INLINE next #-}
      next i
          | i >= l                         = Done
          | i+1 < l && U16.validate1 x1    = Yield (unsafeChr16 x1) (i+2)
          | i+3 < l && U16.validate2 x1 x2 = Yield (U16.chr2 x1 x2) (i+4)
          | otherwise = decodeError "streamUtf16LE" "UTF-16LE" onErr Nothing (i+1)
          where
            x1    = idx i       + (idx (i + 1) `unsafeShiftL` 8)
            x2    = idx (i + 2) + (idx (i + 3) `unsafeShiftL` 8)
            idx = fromIntegral . BS.index bs :: Int -> Word16
{-# INLINE [0] streamUtf16LE #-}

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> ShortByteString -> String
decodeUtf16LEWith onErr bs = unstream (streamUtf16LE onErr bs)
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
decodeUtf8With onErr bs = unstream (streamUtf8 onErr bs)
{-# INLINE decodeUtf8With #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf8 :: ShortByteString -> String
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE decodeUtf8 #-}


decodeError :: forall s. String -> String -> OnDecodeError -> Maybe Word8
            -> s -> Step s Char
decodeError func kind onErr mb i =
    case onErr desc mb of
      Nothing -> Skip i
      Just c  -> Yield c i
    where desc = "Data.Text.Internal.Encoding.Fusion." ++ func ++ ": Invalid " ++
                 kind ++ " stream"

-- | /O(n)/ Convert a 'Stream Char' into a 'Text'.
unstream :: Stream Char -> String
unstream (Stream next0 s0 _) = go s0
  where
    go si =
      case next0 si of
          Done        -> []
          Skip si'    -> go si'
          Yield c si' -> c : go si'
{-# INLINE [0] unstream #-}


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
decodeUtf16LE'' :: ByteString -> Either UnicodeException Text
decodeUtf16LE'' = unsafeDupablePerformIO . try . evaluate . E.decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE'' #-}

