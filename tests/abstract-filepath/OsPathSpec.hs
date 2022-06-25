{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module OsPathSpec where

import Data.Maybe

import System.OsPath
import System.OsString.Internal.Types
import System.OsPath.Posix as Posix
import System.OsPath.Windows as Windows
import System.OsPath.Encoding
import qualified System.OsString.Internal.Types as OS
import System.OsPath.Data.ByteString.Short ( toShort )

import Control.Exception
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Test.QuickCheck
import Test.QuickCheck.Checkers
import qualified Test.QuickCheck.Classes as QC
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import GHC.IO.Encoding ( setFileSystemEncoding )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import Control.DeepSeq
import Data.Bifunctor ( first )
import qualified Data.ByteString.Char8 as C
import qualified System.OsPath.Data.ByteString.Short.Word16 as BS16
import qualified System.OsPath.Data.ByteString.Short as SBS
import Data.Char ( ord )

import Arbitrary


fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b


tests :: [(String, Property)]
tests =
  [ ("fromOsPathUtf . toOsPathUtf == id",
    property $ \(NonNullString str) -> (fromOsPathUtf . fromJust . toOsPathUtf) str == Just str)

  , ("fromPlatformStringUtf . toPlatformStringUtf == id (Posix)",
    property $ \(NonNullString str) -> (Posix.fromPlatformStringUtf . fromJust . Posix.toPlatformStringUtf) str == Just str)
  , ("fromPlatformStringUtf . toPlatformStringUtf == id (Windows)",
    property $ \(NonNullString str) -> (Windows.fromPlatformStringUtf . fromJust . Windows.toPlatformStringUtf) str == Just str)

  , ("toPlatformStringEnc ucs2le . fromPlatformStringEnc ucs2le == id (Posix)",
    property $ \(padEven -> bs) -> (Posix.toPlatformStringEnc ucs2le . (\(Right r) -> r) . Posix.fromPlatformStringEnc ucs2le . OS.PS . toShort) bs === Right (OS.PS . toShort $ bs))
  , ("toPlatformStringEnc ucs2le . fromPlatformStringEnc ucs2le == id (Windows)",
    property $ \(padEven -> bs) -> (Windows.toPlatformStringEnc ucs2le . (\(Right r) -> r) . Windows.fromPlatformStringEnc ucs2le . OS.WS . toShort) bs
           === Right (OS.WS . toShort $ bs))

  , ("fromPlatformStringFS . toPlatformStringFS == id (Posix)",
    property $ \(NonNullString str) -> ioProperty $ do
      setFileSystemEncoding (mkUTF8 TransliterateCodingFailure)
      r1 <- Posix.toPlatformStringFS str
      r2 <- try @SomeException $ Posix.fromPlatformStringFS r1
      r3 <- evaluate $ force $ first displayException r2
      pure (r3 === Right str)
      )
  , ("fromPlatformStringFS . toPlatformStringFS == id (Windows)",
    property $ \(NonNullString str) -> ioProperty $ do
      r1 <- Windows.toPlatformStringFS str
      r2 <- try @SomeException $ Windows.fromPlatformStringFS r1
      r3 <- evaluate $ force $ first displayException r2
      pure (r3 === Right str)
      )

  , ("fromPlatformString* functions are equivalent under ASCII (Windows)",
    property $ \(WindowsString . BS16.pack . map (fromIntegral . ord) . nonNullAsciiString -> str) -> ioProperty $ do
      r1         <- Windows.fromPlatformStringFS str
      r2         <- Windows.fromPlatformStringUtf str
      (Right r3) <- pure $ Windows.fromPlatformStringEnc (mkUTF16le TransliterateCodingFailure) str
      (Right r4) <- pure $ Windows.fromPlatformStringEnc (mkUTF16le RoundtripFailure) str
      (Right r5) <- pure $ Windows.fromPlatformStringEnc (mkUTF16le ErrorOnCodingFailure) str
      pure (    r1 === r2
           .&&. r1 === r3
           .&&. r1 === r4
           .&&. r1 === r5
           )
    )

  , ("fromPlatformString* functions are equivalent under ASCII (Posix)",
    property $ \(PosixString . SBS.toShort . C.pack . nonNullAsciiString -> str) -> ioProperty $ do
      r1         <-        Posix.fromPlatformStringFS str
      r2         <-        Posix.fromPlatformStringUtf str
      (Right r3) <- pure $ Posix.fromPlatformStringEnc (mkUTF8 TransliterateCodingFailure) str
      (Right r4) <- pure $ Posix.fromPlatformStringEnc (mkUTF8 RoundtripFailure) str
      (Right r5) <- pure $ Posix.fromPlatformStringEnc (mkUTF8 ErrorOnCodingFailure) str
      pure (    r1 === r2
           .&&. r1 === r3
           .&&. r1 === r4
           .&&. r1 === r5
           )
    )

  , ("toPlatformString* functions are equivalent under ASCII (Windows)",
    property $ \(NonNullAsciiString str) -> ioProperty $ do
      r1         <- Windows.toPlatformStringFS str
      r2         <- Windows.toPlatformStringUtf str
      (Right r3) <- pure $ Windows.toPlatformStringEnc (mkUTF16le TransliterateCodingFailure) str
      (Right r4) <- pure $ Windows.toPlatformStringEnc (mkUTF16le RoundtripFailure) str
      (Right r5) <- pure $ Windows.toPlatformStringEnc (mkUTF16le ErrorOnCodingFailure) str
      pure (    r1 === r2
           .&&. r1 === r3
           .&&. r1 === r4
           .&&. r1 === r5
           )
    )

  , ("toPlatformString* functions are equivalent under ASCII (Posix)",
    property $ \(NonNullAsciiString str) -> ioProperty $ do
      r1         <-        Posix.toPlatformStringFS str
      r2         <-        Posix.toPlatformStringUtf str
      (Right r3) <- pure $ Posix.toPlatformStringEnc (mkUTF8 TransliterateCodingFailure) str
      (Right r4) <- pure $ Posix.toPlatformStringEnc (mkUTF8 RoundtripFailure) str
      (Right r5) <- pure $ Posix.toPlatformStringEnc (mkUTF8 ErrorOnCodingFailure) str
      pure (    r1 === r2
           .&&. r1 === r3
           .&&. r1 === r4
           .&&. r1 === r5
           )
    )
  , ("Unit test toPlatformString* (Posix)",
    property $ ioProperty $ do
      let str = "ABcK_(ツ123_&**"
      let expected = PosixString $ SBS.pack [0x41,0x42,0x63,0x4b,0x5f,0x28,0xe3,0x83,0x84,0x31,0x32,0x33,0x5f,0x26,0x2a,0x2a]
      r1         <-        Posix.toPlatformStringFS str
      r2         <-        Posix.toPlatformStringUtf str
      (Right r3) <- pure $ Posix.toPlatformStringEnc (mkUTF8 TransliterateCodingFailure) str
      (Right r4) <- pure $ Posix.toPlatformStringEnc (mkUTF8 RoundtripFailure) str
      (Right r5) <- pure $ Posix.toPlatformStringEnc (mkUTF8 ErrorOnCodingFailure) str
      pure (    r1 === expected
           .&&. r2 === expected
           .&&. r3 === expected
           .&&. r4 === expected
           .&&. r5 === expected
           )
    )
  , ("Unit test toPlatformString* (WindowsString)",
    property $ ioProperty $ do
      let str = "ABcK_(ツ123_&**"
      let expected = WindowsString $ BS16.pack [0x0041,0x0042,0x0063,0x004b,0x005f,0x0028,0x30c4,0x0031,0x0032,0x0033,0x005f,0x0026,0x002a,0x002a]
      r1         <-        Windows.toPlatformStringFS str
      r2         <-        Windows.toPlatformStringUtf str
      (Right r3) <- pure $ Windows.toPlatformStringEnc (mkUTF16le TransliterateCodingFailure) str
      (Right r4) <- pure $ Windows.toPlatformStringEnc (mkUTF16le RoundtripFailure) str
      (Right r5) <- pure $ Windows.toPlatformStringEnc (mkUTF16le ErrorOnCodingFailure) str
      pure (    r1 === expected
           .&&. r2 === expected
           .&&. r3 === expected
           .&&. r4 === expected
           .&&. r5 === expected
           )
    )

  , ("Unit test fromPlatformString* (Posix)",
    property $ ioProperty $ do
      let bs = PosixString $ SBS.pack [0x41,0x42,0x63,0x4b,0x5f,0x28,0xe3,0x83,0x84,0x31,0x32,0x33,0x5f,0x26,0x2a,0x2a]
      let expected = "ABcK_(ツ123_&**"
      r1         <-        Posix.fromPlatformStringFS bs
      r2         <-        Posix.fromPlatformStringUtf bs
      (Right r3) <- pure $ Posix.fromPlatformStringEnc (mkUTF8 TransliterateCodingFailure) bs
      (Right r4) <- pure $ Posix.fromPlatformStringEnc (mkUTF8 RoundtripFailure) bs
      (Right r5) <- pure $ Posix.fromPlatformStringEnc (mkUTF8 ErrorOnCodingFailure) bs
      pure (    r1 === expected
           .&&. r2 === expected
           .&&. r3 === expected
           .&&. r4 === expected
           .&&. r5 === expected
           )
    )
  , ("Unit test fromPlatformString* (WindowsString)",
    property $ ioProperty $ do
      let bs = WindowsString $ BS16.pack [0x0041,0x0042,0x0063,0x004b,0x005f,0x0028,0x30c4,0x0031,0x0032,0x0033,0x005f,0x0026,0x002a,0x002a]
      let expected = "ABcK_(ツ123_&**"
      r1         <-        Windows.fromPlatformStringFS bs
      r2         <-        Windows.fromPlatformStringUtf bs
      (Right r3) <- pure $ Windows.fromPlatformStringEnc (mkUTF16le TransliterateCodingFailure) bs
      (Right r4) <- pure $ Windows.fromPlatformStringEnc (mkUTF16le RoundtripFailure) bs
      (Right r5) <- pure $ Windows.fromPlatformStringEnc (mkUTF16le ErrorOnCodingFailure) bs
      pure (    r1 === expected
           .&&. r2 === expected
           .&&. r3 === expected
           .&&. r4 === expected
           .&&. r5 === expected
           )
    )


  ] ++ testBatch (QC.ord (\(a :: OsPath) -> pure a))
    ++ testBatch (QC.monoid (undefined :: OsPath))

    ++ testBatch (QC.ord (\(a :: OsString) -> pure a))
    ++ testBatch (QC.monoid (undefined :: OsString))

    ++ testBatch (QC.ord (\(a :: WindowsString) -> pure a))
    ++ testBatch (QC.monoid (undefined :: WindowsString))

    ++ testBatch (QC.ord (\(a :: PosixString) -> pure a))
    ++ testBatch (QC.monoid (undefined :: PosixString))

    ++ testBatch (QC.ord (\(a :: PlatformString) -> pure a))
    ++ testBatch (QC.monoid (undefined :: PlatformString))

-- | Allows to insert a 'TestBatch' into a Spec.
testBatch :: TestBatch -> [(String, Property)]
testBatch (_, tests') = tests'


padEven :: ByteString -> ByteString
padEven bs
  | even (BS.length bs) = bs
  | otherwise = bs `BS.append` BS.pack [70]
