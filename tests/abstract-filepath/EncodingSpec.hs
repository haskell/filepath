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
import qualified GHC.Foreign as GHC
import System.OsPath.Encoding.Internal
import GHC.IO (unsafePerformIO)
import GHC.IO.Encoding ( setFileSystemEncoding )
import System.IO
    ( TextEncoding, utf16le )
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
      let decoded = decode ucs2le (BS8.toShort ba)
          encoded = encode ucs2le =<< decoded
      in (BS8.fromShort <$> encoded) === Right ba)
  , ("utf16 doesn't handle invalid surrogate pairs",
     property $
      let str = [toEnum 55296, toEnum 55297]
          encoded = encode utf16le str
          decoded = decode utf16le =<< encoded
      in decoded === Left "recoverEncode: invalid argument (invalid character)")
  , ("ucs2 handles invalid surrogate pairs",
     property $
      let str = [toEnum 55296, toEnum 55297]
          encoded = encode ucs2le str
          decoded = decode ucs2le =<< encoded
      in decoded === Right str)
  , ("can roundtrip arbitrary bytes through utf-8 (with RoundtripFailure)",
     property $
      \bs ->
        let decoded = decode (mkUTF8 RoundtripFailure) (BS8.toShort bs)
            encoded = encode (mkUTF8 RoundtripFailure) =<< decoded
        in (either (const 0) BS8.length encoded, encoded) === (BS8.length (BS8.toShort bs), Right (BS8.toShort bs)))

  , ("can decode arbitrary strings through utf-8 (with RoundtripFailure)",
     property $
      \(NonNullSurrogateString str) ->
        let encoded = encode (mkUTF8 RoundtripFailure) str
            decoded = decode (mkUTF8 RoundtripFailure) =<< encoded
        in expectFailure $ (either (const 0) length decoded, decoded) === (length str, Right str))

  , ("utf-8 roundtrip encode cannot deal with some surrogates",
     property $
      let str = [toEnum 0xDFF0, toEnum 0xDFF2]
          encoded = encode (mkUTF8 RoundtripFailure) str
          decoded = decode (mkUTF8 RoundtripFailure) =<< encoded
      in decoded === Left "recoverEncode: invalid argument (invalid character)")

  , ("cannot roundtrip arbitrary bytes through utf-16 (with RoundtripFailure)",
     property $
      \(padEven -> bs) ->
        let decoded = decode (mkUTF16le RoundtripFailure) (BS8.toShort bs)
            encoded = encode (mkUTF16le RoundtripFailure) =<< decoded
        in expectFailure $ (either (const 0) BS8.length encoded, encoded) === (BS8.length (BS8.toShort bs), Right (BS8.toShort bs)))
  , ("encodeWith/decodeWith ErrorOnCodingFailure fails (utf16le)",
     property $
      \(padEven -> bs) ->
        let decoded = decodeWith (mkUTF16le ErrorOnCodingFailure) (BS8.toShort bs)
            encoded = encodeWith (mkUTF16le ErrorOnCodingFailure) =<< decoded
        in expectFailure $ (isRight encoded, isRight decoded) === (True, True))
  , ("encodeWith/decodeWith ErrorOnCodingFailure fails (utf8)",
     property $
      \bs ->
        let decoded = decodeWith (mkUTF8 ErrorOnCodingFailure) (BS8.toShort bs)
            encoded = encodeWith (mkUTF8 ErrorOnCodingFailure) =<< decoded
        in expectFailure $ (isRight encoded, isRight decoded) === (True, True))
  , ("encodeWith/decodeWith TransliterateCodingFailure never fails (utf16le)",
     property $
      \(padEven -> bs) ->
        let decoded = decodeWith (mkUTF16le TransliterateCodingFailure) (BS8.toShort bs)
            encoded = encodeWith (mkUTF16le TransliterateCodingFailure) =<< decoded
        in (isRight encoded, isRight decoded) === (True, True))
  , ("encodeWith/decodeWith TransliterateCodingFailure never fails (utf8)",
     property $
      \bs ->
        let decoded = decodeWith (mkUTF8 TransliterateCodingFailure) (BS8.toShort bs)
            encoded = encodeWith (mkUTF8 TransliterateCodingFailure) =<< decoded
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
  ]


padEven :: ByteString -> ByteString
padEven bs
  | even (BS.length bs) = bs
  | otherwise = bs `BS.append` BS.pack [70]


decode :: TextEncoding -> BS8.ShortByteString -> Either String String
decode enc ba = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> GHC.peekCStringLen enc fp
  evaluate $ force $ first displayException r

encode :: TextEncoding -> String -> Either String BS8.ShortByteString
encode enc str = unsafePerformIO $ do
  r <- try @SomeException $ GHC.withCStringLen enc str $ \cstr -> BS8.packCStringLen cstr
  evaluate $ force $ first displayException r


decodeP' :: BS8.ShortByteString -> Either String String
decodeP' ba = unsafePerformIO $ do
  r <- try @SomeException $ BS8.useAsCStringLen ba $ \fp -> peekFilePathLenPosix fp
  evaluate $ force $ first displayException r

encodeP' :: String -> Either String BS8.ShortByteString
encodeP' str = unsafePerformIO $ do
  r <- try @SomeException $ withFilePathPosix str $ \cstr -> BS8.packCString cstr
  evaluate $ force $ first displayException r

decodeW' :: BS16.ShortByteString -> Either String String
decodeW' ba = unsafePerformIO $ do
  r <- try @SomeException $ BS16.useAsCWString ba $ \fp -> peekFilePathWin fp
  evaluate $ force $ first displayException r

encodeW' :: String -> Either String BS8.ShortByteString
encodeW' str = unsafePerformIO $ do
  r <- try @SomeException $ withFilePathWin str $ \cstr -> BS16.packCWString cstr
  evaluate $ force $ first displayException r

