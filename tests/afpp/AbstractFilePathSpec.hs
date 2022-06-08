{-# LANGUAGE ScopedTypeVariables #-}

module AbstractFilePathSpec where

import Data.Maybe

import System.AbstractFilePath
import System.OsString.Internal.Types
import System.AbstractFilePath.Data.ByteString.Short.Decode
    ( decodeUtf16LE, decodeUtf8 )
import System.AbstractFilePath.Data.ByteString.Short.Encode
    ( encodeUtf16LE, encodeUtf8 )
import System.AbstractFilePath.Posix as Posix
import System.AbstractFilePath.Windows as Windows

import Arbitrary
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck
    ( Testable (property) )
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


tests :: [TestTree]
tests =
  [ testProperty "decodeUtf8 . encodeUtf8 == id" $
    \str -> (decodeUtf8 . encodeUtf8) str == str
  , testProperty "decodeUtf16LE . encodeUtf16LE == id" $
    \str -> (decodeUtf16LE . encodeUtf16LE) str == str
  , testProperty "fromAbstractFilePathUtf . toAbstractFilePathUtf == id" $
    \(NonNullString str) -> (fromAbstractFilePathUtf . fromJust . toAbstractFilePathUtf) str == Just str
  , testProperty "fromPlatformStringUtf . toPlatformStringUtf == id (Posix)" $
    \(NonNullString str) -> (Posix.fromPlatformStringUtf . fromJust . Posix.toPlatformStringUtf) str == Just str
  , testProperty "fromPlatformStringUtf . toPlatformStringUtf == id (Windows)" $
    \(NonNullString str) -> (Windows.fromPlatformStringUtf . fromJust . Windows.toPlatformStringUtf) str == Just str

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
testBatch :: TestBatch -> [TestTree]
testBatch (batchName, tests) =
    fmap (\(str, prop) -> testProperty str prop) tests
