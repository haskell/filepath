module Main (main) where

import qualified OsPathSpec
import qualified EncodingSpec
import TestUtil

main :: IO ()
main = runTests (EncodingSpec.tests ++ OsPathSpec.tests)
