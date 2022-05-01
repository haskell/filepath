module Main (main) where

import Test.Tasty

import qualified AbstractFilePathSpec

main :: IO ()
main = defaultMain $ testGroup "All"
  AbstractFilePathSpec.tests
