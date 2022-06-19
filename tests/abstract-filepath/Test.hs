module Main (main) where

import qualified AbstractFilePathSpec
import qualified EncodingSpec
import TestUtil

main :: IO ()
main = runTests (EncodingSpec.tests ++ AbstractFilePathSpec.tests)
