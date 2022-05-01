module Main (main) where

import Test.Tasty

import qualified Properties

main :: IO ()
main = defaultMain $ testGroup "All"
  [ Properties.testSuite
  ]
