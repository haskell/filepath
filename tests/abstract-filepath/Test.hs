module Main (main) where

import qualified OsPathSpec
import TestUtil

main :: IO ()
main = runTests (OsPathSpec.tests)
