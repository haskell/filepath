{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Properties (testSuite) where

import Test.Tasty

import qualified Properties.ShortByteString as PropSBS
import qualified Properties.ShortByteString.Word16 as PropSBSW16


------------------------------------------------------------------------
-- The entry point

testSuite :: TestTree
testSuite = testGroup "Properties"
  [ testGroup "ShortByteString"        PropSBS.tests
  , testGroup "ShortByteString.Word16" PropSBSW16.tests
  ]

