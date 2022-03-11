{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

-- We need @AllowAmbiguousTypes@ in order to be able to use @TypeApplications@
-- to disambiguate the desired instance of class methods whose instance cannot
-- be inferred from the caller's context.  We would otherwise have to use
-- proxy arguments.  Here the 'RdInt' class methods used to generate tests for
-- all the various 'readInt' types require explicit type applications.

module Properties (testSuite) where

import Test.Tasty

import qualified Properties.ShortByteString as PropSBS


------------------------------------------------------------------------
-- The entry point

testSuite :: TestTree
testSuite = testGroup "Properties"
  [ testGroup "ShortByteString.Word16" PropSBS.tests
  ]

