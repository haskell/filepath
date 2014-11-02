
module TestUtil(
    (==>), QFilePath(..), test,
    module Test.QuickCheck,
    module Data.List
    ) where

import Test.QuickCheck hiding ((==>))
import Data.List
import Control.Monad

infixr 0 ==>
a ==> b = not a || b


newtype QFilePath = QFilePath FilePath deriving Show

instance Arbitrary QFilePath where
    arbitrary = fmap QFilePath arbitraryFilePath
    shrink (QFilePath x) = map QFilePath $ shrink x


arbitraryFilePath :: Gen FilePath
arbitraryFilePath = sized $ \n -> do
    k <- choose (0,n)
    replicateM k $ elements "?./:\\a ;_"


test :: Testable a => a -> IO ()
test prop = do
    res <- quickCheckWithResult stdArgs{chatty=False, maxSuccess=10000} prop
    case res of
        Success{} -> return ()
        -- Output is already escaped. Do not call show on it, but print as-is.
        _ -> error $ show res{output=""} ++ "\n" ++ (output res)
