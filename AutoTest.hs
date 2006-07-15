
module AutoTest(module AutoTest, module Test.QuickCheck) where

import Test.QuickCheck
import Char


constTest :: Bool -> IO ()
constTest True = return ()
constTest False = error "Failed on constTest"



data QFilePath = QFilePath FilePath
                 deriving Show

instance Arbitrary QFilePath where
    arbitrary = vector 25 >>= return . QFilePath


instance Arbitrary Char where
    arbitrary = oneof $ map return "?|./:\\abcd 123;_"

