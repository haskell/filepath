{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Properties.ShortByteString as PropSBS
import qualified Properties.ShortByteString.Word16 as PropSBSW16
import TestUtil

main :: IO ()
main = runTests (PropSBS.tests ++ PropSBSW16.tests)
