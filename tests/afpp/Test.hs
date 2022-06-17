module Main (main) where

import Test.Tasty

import qualified AbstractFilePathSpec
import qualified EncodingSpec

main :: IO ()
main = defaultMain $ do
  testGroup "All" $
    AbstractFilePathSpec.tests <> EncodingSpec.tests
