
module Test(main) where

import System.Environment
import TestGen
import Control.Monad
import Test.QuickCheck


main :: IO ()
main = do
    args <- getArgs
    let count = case args of i:_ -> read i; _ -> 10000
    putStrLn $ "Testing with " ++ show count ++ " repetitions"
    let total = length tests
    bs <- forM (zip [1..] tests) $ \(i,(msg,prop)) -> do
        putStrLn $ "Test " ++ show i ++ " of " ++ show total ++ ": " ++ msg
        res <- quickCheckWithResult stdArgs{maxSuccess=count} prop
        case res of
            Success{} -> return True
            _ -> putStrLn "TEST FAILURE!" >> return False
    let bad = length $ filter (== False) bs
    if bad == 0 then
        putStrLn $ "Success, " ++ show total ++ " tests passed"
     else
        fail $ "FAILURE, failed " ++ show bad ++ " of " ++ show total ++ " tests (look for FAILURE)"
