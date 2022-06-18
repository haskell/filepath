{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module EncodingSpec where

import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS

import Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.Either ( isRight )
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
import GHC.IO.Encoding.UTF8 ( mkUTF8 )


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
      in encoded @?= Left "recoverEncode: invalid argument (invalid character)"
  , testCase "ucs2 handles invalid surrogate pairs" $
      let str = [toEnum 55296, toEnum 55297]
          decoded = decode ucs2le str
          encoded = encode ucs2le =<< decoded
      in encoded @?= Right str
  , testProperty "cannot roundtrip arbitrary bytes through utf-16 (with RoundtripFailure)" $
      \(padEven -> bs) ->
        let encoded = encode (mkUTF16le RoundtripFailure) (BS8.toShort bs)
            decoded = decode (mkUTF16le RoundtripFailure) =<< encoded
        in expectFailure $ (either (const 0) BS8.length decoded, decoded) === (BS8.length (BS8.toShort bs), Right (BS8.toShort bs))
  , testProperty "encodeWith/decodeWith ErrorOnCodingFailure fails (utf16le)" $
      \(padEven -> bs) ->
        let decoded = decodeWith (mkUTF16le ErrorOnCodingFailure) (BS8.toShort bs)
            encoded = (encodeWith (mkUTF16le ErrorOnCodingFailure)) =<< decoded
        in expectFailure $ (isRight encoded, isRight decoded) === (True, True)
  , testProperty "encodeWith/decodeWith ErrorOnCodingFailure fails (utf8)" $
      \(padEven -> bs) ->
        let decoded = decodeWith (mkUTF8 ErrorOnCodingFailure) (BS8.toShort bs)
            encoded = (encodeWith (mkUTF8 ErrorOnCodingFailure)) =<< decoded
        in expectFailure $ (isRight encoded, isRight decoded) === (True, True)
  , testProperty "encodeWith/decodeWith TransliterateCodingFailure never fails (utf16le)" $
      \(padEven -> bs) ->
        let decoded = decodeWith (mkUTF16le TransliterateCodingFailure) (BS8.toShort bs)
            encoded = (encodeWith (mkUTF16le TransliterateCodingFailure)) =<< decoded
        in (isRight encoded, isRight decoded) === (True, True)
  , testProperty "encodeWith/decodeWith TransliterateCodingFailure never fails (utf8)" $
      \(padEven -> bs) ->
        let decoded = decodeWith (mkUTF8 TransliterateCodingFailure) (BS8.toShort bs)
            encoded = (encodeWith (mkUTF8 TransliterateCodingFailure)) =<< decoded
        in (isRight encoded, isRight decoded) === (True, True)
  ]


padEven :: ByteString -> ByteString
padEven bs
  | even (BS.length bs) = bs
  | otherwise = bs `BS.append` BS.pack [70]


encode :: TextEncoding -> BS8.ShortByteString -> Either String String
encode enc ba = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen enc fp
  evaluate $ force $ first (displayException) r

decode :: TextEncoding -> String -> Either String BS8.ShortByteString
decode enc str = unsafePerformIO $ do
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> BS8.packCStringLen cstr
  evaluate $ force $ first (displayException) r


