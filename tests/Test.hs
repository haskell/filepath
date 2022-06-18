{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment
import TestGen
import Control.Monad
import Data.Maybe
import Test.QuickCheck


main :: IO ()
main = do
    args <- getArgs
    let count   = case args of i:_   -> read i; _ -> 10000
    let testNum = case args of
                    _:i:_
                      | let num = read i
                      , num < 0    -> drop (negate num) tests
                      | let num = read i
                      , num > 0    -> take num          tests
                      | otherwise  -> []
                    _ -> tests
    putStrLn $ "Testing with " ++ show count ++ " repetitions"
    let total' = length testNum
    let showOutput x = show x{output=""} ++ "\n" ++ output x
    bad <- fmap catMaybes $ forM (zip @Integer [1..] testNum) $ \(i,(msg,prop)) -> do
        putStrLn $ "Test " ++ show i ++ " of " ++ show total' ++ ": " ++ msg
        res <- quickCheckWithResult stdArgs{chatty=False, maxSuccess=count} prop
        case res of
            Success{} -> pure Nothing
            bad -> do putStrLn $ showOutput bad; putStrLn "TEST FAILURE!"; pure $ Just (msg,bad)
    if null bad then
        putStrLn $ "Success, " ++ show total' ++ " tests passed"
     else do
        putStrLn $ show (length bad) ++ " FAILURES\n"
        forM_ (zip @Integer [1..] bad) $ \(i,(a,b)) ->
            putStrLn $ "FAILURE " ++ show i ++ ": " ++ a ++ "\n" ++ showOutput b ++ "\n"
        fail $ "FAILURE, failed " ++ show (length bad) ++ " of " ++ show total' ++ " tests"
