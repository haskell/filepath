{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module EncodingSpec where

import System.AbstractFilePath.Data.ByteString.Short.Decode
    ( UnicodeException(..) )

import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS

import Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified System.AbstractFilePath.Data.ByteString.Short as BS8
import qualified GHC.Foreign as GHC
import System.AbstractFilePath.Encoding
import GHC.IO (unsafePerformIO)
import System.IO
    ( TextEncoding, utf16le )
import Control.Exception
import Control.DeepSeq
import Data.Bifunctor ( first )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import GHC.Char ( chr )
import Foreign.C.Types
import Data.Char ( ord )
import Foreign.C (CWStringLen)
import Foreign.C.String ( withCStringLen )
import Foreign.Marshal.Array
import Foreign.Ptr


tests :: [TestTree]
tests =
  [ testProperty "ucs2le_decode . ucs2le_encode == id" $
    \(padEven -> ba) ->
      let encoded = encode ucs2le (BS8.toShort ba)
          decoded = decode ucs2le =<< encoded
      in (BS8.fromShort <$> decoded) === Right ba
  , testCase "utf16 doesn't handle invalid surrogate pairs" $
      let str = [toEnum 55296, toEnum 55297]
          decoded = decode utf16le str
          encoded = encode utf16le =<< decoded
      in encoded @?= Left (DecodeError "recoverEncode: invalid argument (invalid character)" Nothing)
  , testCase "ucs2 handles invalid surrogate pairs" $
      let str = [toEnum 55296, toEnum 55297]
          decoded = decode ucs2le str
          encoded = encode ucs2le =<< decoded
      in encoded @?= Right str
  , testProperty "cannot roundtrip arbitrary bytes through utf-16 (via cWcharsToChars / charsToCWchars)" $
      \(padEven -> bs) ->
        let encoded = encode' (BS8.toShort bs)
            decoded = decode' =<< encoded
        in expectFailure $ (either (const 0) BS8.length decoded, decoded) === (BS8.length (BS8.toShort bs), Right (BS8.toShort bs))
  , testProperty "cannot roundtrip arbitrary bytes through utf-16 (with RoundtripFailure)" $
      \(padEven -> bs) ->
        let encoded = encode (mkUTF16le RoundtripFailure) (BS8.toShort bs)
            decoded = decode (mkUTF16le RoundtripFailure) =<< encoded
        in expectFailure $ (either (const 0) BS8.length decoded, decoded) === (BS8.length (BS8.toShort bs), Right (BS8.toShort bs))
  ]


padEven :: ByteString -> ByteString
padEven bs
  | even (BS.length bs) = bs
  | otherwise = bs `BS.append` BS.pack [70]


encode :: TextEncoding -> BS8.ShortByteString -> Either UnicodeException String
encode enc ba = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen enc fp
  evaluate $ force $ first (flip DecodeError Nothing . displayException) r

decode :: TextEncoding -> String -> Either UnicodeException BS8.ShortByteString
decode enc str = unsafePerformIO $ do
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> BS8.packCStringLen cstr
  evaluate $ force $ first (flip DecodeError Nothing . displayException) r



-- On Windows, wchar_t is 16 bits wide and CWString uses the UTF-16 encoding.

-- coding errors generate Chars in the surrogate range
cWcharsToChars :: [CWchar] -> [Char]
cWcharsToChars = map chr . fromUTF16 . map fromIntegral
 where
  fromUTF16 (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c:wcs) = c : fromUTF16 wcs
  fromUTF16 [] = []

charsToCWchars :: [Char] -> [CWchar]
charsToCWchars = foldr utf16Char [] . fmap ord
 where
  utf16Char c wcs
    | c < 0x10000 = fromIntegral c : wcs
    | otherwise   = let c' = c - 0x10000 in
                    fromIntegral (c' `div` 0x400 + 0xd800) :
                    fromIntegral (c' `mod` 0x400 + 0xdc00) : wcs


peekCWStringLen'           :: CWStringLen -> IO String
peekCWStringLen' (cp, len)  = do
  cs <- peekArray len cp
  return (cWcharsToChars cs)

withCWStringLen         :: String -> (CWStringLen -> IO a) -> IO a
withCWStringLen str f    =
  withArrayLen (charsToCWchars str) $ \ len ptr -> f (ptr, len)

wNUL :: CWchar
wNUL = 0

encode' :: BS8.ShortByteString -> Either UnicodeException String
encode' ba = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \(fp, len) -> peekCWStringLen' (castPtr fp, len)
  evaluate $ force $ first (flip DecodeError Nothing . displayException) r

decode' :: String -> Either UnicodeException BS8.ShortByteString
decode' str = unsafePerformIO $ do
  r <- try @SomeException $ withCStringLen str $ \(fp, len) -> BS8.packCStringLen (castPtr fp, len)
  evaluate $ force $ first (flip DecodeError Nothing . displayException) r

