{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module AbstractFilePathSpec where

import Data.Maybe

import System.AbstractFilePath
import System.OsString.Internal.Types
import System.AbstractFilePath.Posix as Posix
import System.AbstractFilePath.Windows as Windows
import System.AbstractFilePath.Encoding
import qualified System.OsString.Internal.Types as OS
import System.AbstractFilePath.Data.ByteString.Short ( toShort )

import Control.Exception
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import GHC.IO.Encoding ( setFileSystemEncoding )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import Control.DeepSeq
import Data.Bifunctor ( first )

import Arbitrary


fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b


tests :: [(String, Property)]
tests =
  [ ("fromAbstractFilePathUtf . toAbstractFilePathUtf == id",
    property $ \(NonNullString str) -> (fromAbstractFilePathUtf . fromJust . toAbstractFilePathUtf) str == Just str)
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
      r1 <- Posix.toPlatformStringUtf str
      r2 <- try @SomeException $ Posix.fromPlatformStringFS r1
      r3 <- evaluate $ force $ first displayException r2
      pure (r3 === Right str)
      )
  , ("fromPlatformStringFS . toPlatformStringFS == id (Windows)",
    property $ \(NonNullString str) -> ioProperty $ do
      r1 <- Windows.toPlatformStringUtf str
      r2 <- try @SomeException $ Windows.fromPlatformStringFS r1
      r3 <- evaluate $ force $ first displayException r2
      pure (r3 === Right str)
      )

  ] ++ testBatch (ord (\(a :: AbstractFilePath) -> pure a))
    ++ testBatch (monoid (undefined :: AbstractFilePath))

    ++ testBatch (ord (\(a :: OsString) -> pure a))
    ++ testBatch (monoid (undefined :: OsString))

    ++ testBatch (ord (\(a :: WindowsString) -> pure a))
    ++ testBatch (monoid (undefined :: WindowsString))

    ++ testBatch (ord (\(a :: PosixString) -> pure a))
    ++ testBatch (monoid (undefined :: PosixString))

    ++ testBatch (ord (\(a :: PlatformString) -> pure a))
    ++ testBatch (monoid (undefined :: PlatformString))

-- | Allows to insert a 'TestBatch' into a Spec.
testBatch :: TestBatch -> [(String, Property)]
testBatch (_, tests') = tests'


padEven :: ByteString -> ByteString
padEven bs
  | even (BS.length bs) = bs
  | otherwise = bs `BS.append` BS.pack [70]
