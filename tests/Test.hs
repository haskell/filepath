
module Test(main) where

import TestGen
import Control.Monad
import Test.QuickCheck


main :: IO ()
main = do
    let total = length tests
    bs <- forM (zip [1..] tests) $ \(i,(msg,prop)) -> do
        putStrLn $ "Test " ++ show i ++ " of " ++ show total ++ ": " ++ msg
        res <- quickCheckWithResult stdArgs{maxSuccess=10000} prop
        case res of
            Success{} -> return True
            _ -> putStrLn "TEST FAILURE!" >> return False
    let bad = length $ filter (== False) bs
    if bad == 0 then
        putStrLn $ "Success, " ++ show total ++ " tests passed"
     else
        fail $ "FAILURE, failed " ++ show bad ++ " of " ++ show total ++ " tests (look for FAILURE)"
