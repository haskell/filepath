
module TestUtil(
    module TestUtil,
    module Test.QuickCheck,
    module Data.List
    ) where

import Test.QuickCheck hiding ((==>))
import Data.List

infixr 0 ==>
a ==> b = not a || b


newtype QFilePath = QFilePath FilePath
                    deriving Show

instance Arbitrary QFilePath where
    arbitrary = fmap (QFilePath . map fromQChar) arbitrary
    shrink (QFilePath x) = map QFilePath $ shrink x

newtype QChar = QChar {fromQChar :: Char}

instance Arbitrary QChar where
    arbitrary = fmap QChar $ elements "?./:\\a ;_"



test :: Testable a => a -> IO ()
test prop = do
    res <- quickCheckWithResult stdArgs{chatty=False, maxSuccess=10000} prop
    case res of
        Success{} -> return ()
        -- Output is already escaped. Do not call show on it, but print as-is.
        _ -> error $ show res{output=""} ++ "\n" ++ (output res)
