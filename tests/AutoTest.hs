
module AutoTest(
    module AutoTest,
    module Test.QuickCheck,
    module Data.List
    ) where

import Test.QuickCheck hiding ((==>))
import Data.Char
import System.Random
import Data.List
import Control.Monad

infixr 0 ==>
a ==> b = not a || b


constTest :: Bool -> IO ()
constTest True = return ()
constTest False = error "Failed on constTest"



newtype QFilePath = QFilePath FilePath
                    deriving Show

instance Arbitrary QFilePath where
    arbitrary = fmap (QFilePath . map fromQChar) arbitrary
    shrink (QFilePath x) = map QFilePath $ shrink x

newtype QChar = QChar {fromQChar :: Char}

instance Arbitrary QChar where
    arbitrary = fmap QChar $ elements "?./:\\a ;_"



quickSafe :: Testable a => a -> IO ()
quickSafe prop = do
    res <- quickCheckWithResult stdArgs{chatty=False, maxSuccess=10000} prop
    case res of
        Success{} -> return ()
        -- Output is already escaped. Do not call show on it, but print as-is.
        _ -> error $ show res{output=""} ++ "\n" ++ (output res)
