{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module EncodingSpec where

import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS

import Arbitrary
import Test.QuickCheck

import Data.Either ( isRight )
import qualified System.OsPath.Data.ByteString.Short as BS8
import qualified System.OsPath.Data.ByteString.Short.Word16 as BS16
import System.OsPath.Encoding.Internal
import GHC.IO (unsafePerformIO)
import GHC.IO.Encoding ( setFileSystemEncoding )
import System.IO
    ( utf16le )
import Control.Exception
import Control.DeepSeq
import Data.Bifunctor ( first )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import GHC.IO.Encoding.UTF8 ( mkUTF8 )


tests :: [(String, Property)]
tests =
  [ ("ucs2le_decode . ucs2le_encode == id",
    property $ \(padEven -> ba) ->
      let decoded = decodeWithTE ucs2le (BS8.toShort ba)
          encoded = encodeWithTE ucs2le =<< decoded
      in (BS8.fromShort <$> encoded) === Right ba)
  , ("utf16 doesn't handle invalid surrogate pairs",
     property $
      let str = [toEnum 55296, toEnum 55297]
          encoded = encodeWithTE utf16le str
          decoded = decodeWithTE utf16le =<< encoded
      in decoded === Left (EncodingError "recoverEncode: invalid argument (invalid character)" Nothing))
  , ("ucs2 handles invalid surrogate pairs",
     property $
      let str = [toEnum 55296, toEnum 55297]
          encoded = encodeWithTE ucs2le str
          decoded = decodeWithTE ucs2le =<< encoded
      in decoded === Right str)
  , ("can roundtrip arbitrary bytes through utf-8 (with RoundtripFailure)",
     property $
      \bs ->
        let decoded = decodeWithTE (mkUTF8 RoundtripFailure) (BS8.toShort bs)
            encoded = encodeWithTE (mkUTF8 RoundtripFailure) =<< decoded
        in (either (const 0) BS8.length encoded, encoded) === (BS8.length (BS8.toShort bs), Right (BS8.toShort bs)))

  , ("can decode arbitrary strings through utf-8 (with RoundtripFailure)",
     property $
      \(NonNullSurrogateString str) ->
        let encoded = encodeWithTE (mkUTF8 RoundtripFailure) str
            decoded = decodeWithTE (mkUTF8 RoundtripFailure) =<< encoded
        in expectFailure $ (either (const 0) length decoded, decoded) === (length str, Right str))

  , ("utf-8 roundtrip encode cannot deal with some surrogates",
     property $
      let str = [toEnum 0xDFF0, toEnum 0xDFF2]
          encoded = encodeWithTE (mkUTF8 RoundtripFailure) str
          decoded = decodeWithTE (mkUTF8 RoundtripFailure) =<< encoded
      in decoded === Left (EncodingError "recoverEncode: invalid argument (invalid character)" Nothing))

  , ("cannot roundtrip arbitrary bytes through utf-16 (with RoundtripFailure)",
     property $
      \(padEven -> bs) ->
        let decoded = decodeWithTE (mkUTF16le RoundtripFailure) (BS8.toShort bs)
            encoded = encodeWithTE (mkUTF16le RoundtripFailure) =<< decoded
        in expectFailure $ (either (const 0) BS8.length encoded, encoded) === (BS8.length (BS8.toShort bs), Right (BS8.toShort bs)))
  , ("encodeWithTE/decodeWithTE ErrorOnCodingFailure fails (utf16le)",
     property $
      \(padEven -> bs) ->
        let decoded = decodeWithTE (mkUTF16le ErrorOnCodingFailure) (BS8.toShort bs)
            encoded = encodeWithTE (mkUTF16le ErrorOnCodingFailure) =<< decoded
        in expectFailure $ (isRight encoded, isRight decoded) === (True, True))
  , ("encodeWithTE/decodeWithTE ErrorOnCodingFailure fails (utf8)",
     property $
      \bs ->
        let decoded = decodeWithTE (mkUTF8 ErrorOnCodingFailure) (BS8.toShort bs)
            encoded = encodeWithTE (mkUTF8 ErrorOnCodingFailure) =<< decoded
        in expectFailure $ (isRight encoded, isRight decoded) === (True, True))
  , ("encodeWithTE/decodeWithTE TransliterateCodingFailure never fails (utf16le)",
     property $
      \(padEven -> bs) ->
        let decoded = decodeWithTE (mkUTF16le TransliterateCodingFailure) (BS8.toShort bs)
            encoded = encodeWithTE (mkUTF16le TransliterateCodingFailure) =<< decoded
        in (isRight encoded, isRight decoded) === (True, True))
  , ("encodeWithTE/decodeWithTE TransliterateCodingFailure never fails (utf8)",
     property $
      \bs ->
        let decoded = decodeWithTE (mkUTF8 TransliterateCodingFailure) (BS8.toShort bs)
            encoded = encodeWithTE (mkUTF8 TransliterateCodingFailure) =<< decoded
        in (isRight encoded, isRight decoded) === (True, True))
  , ("encodeWithBaseWindows/decodeWithBaseWindows never fails (utf16le)",
     property $
      \(padEven -> bs) ->
        let decoded = decodeW' (BS8.toShort bs)
            encoded = encodeW' =<< decoded
        in (isRight encoded, isRight decoded) === (True, True))
  , ("encodeWithBasePosix/decodeWithBasePosix never fails (utf8b)",
     property $
      \bs -> ioProperty $ do
        setFileSystemEncoding (mkUTF8 TransliterateCodingFailure)
        let decoded = decodeP' (BS8.toShort bs)
            encoded = encodeP' =<< decoded
        pure $ (isRight encoded, isRight decoded) === (True, True))

  , ("decodeWithBaseWindows == utf16le_b",
     property $
      \(BS8.toShort . padEven -> bs) ->
        let decoded  = decodeW' bs
            decoded' = first displayException $ decodeWithTE (mkUTF16le_b ErrorOnCodingFailure) bs
        in decoded === decoded')

  , ("encodeWithBaseWindows == utf16le_b",
     property $
      \(NonNullSurrogateString str) ->
        let decoded  = encodeW' str
            decoded' = first displayException $ encodeWithTE (mkUTF16le_b ErrorOnCodingFailure) str
        in decoded === decoded')

  , ("encodeWithTE/decodeWithTE never fails (utf16le_b)",
     property $
      \(padEven -> bs) ->
        let decoded = decodeWithTE (mkUTF16le_b ErrorOnCodingFailure) (BS8.toShort bs)
            encoded = encodeWithTE (mkUTF16le_b ErrorOnCodingFailure) =<< decoded
        in (isRight encoded, isRight decoded) === (True, True))
  ]


padEven :: ByteString -> ByteString
padEven bs
  | even (BS.length bs) = bs
  | otherwise = bs `BS.append` BS.pack [70]


decodeP' :: BS8.ShortByteString -> Either String String
decodeP' ba = unsafePerformIO $ do
  r <- try @SomeException $ decodeWithBasePosix ba
  evaluate $ force $ first displayException r

encodeP' :: String -> Either String BS8.ShortByteString
encodeP' str = unsafePerformIO $ do
  r <- try @SomeException $ encodeWithBasePosix str
  evaluate $ force $ first displayException r

decodeW' :: BS16.ShortByteString -> Either String String
decodeW' ba = unsafePerformIO $ do
  r <- try @SomeException $ decodeWithBaseWindows ba
  evaluate $ force $ first displayException r

encodeW' :: String -> Either String BS8.ShortByteString
encodeW' str = unsafePerformIO $ do
  r <- try @SomeException $ encodeWithBaseWindows str
  evaluate $ force $ first displayException r

