
module AutoTest(
    module AutoTest,
    module Test.QuickCheck,
    module Data.List
    ) where

import Test.QuickCheck hiding (check,(==>))
import Data.Char
import System.Random
import Data.List
import Control.Monad

infixr 0 ==>
a ==> b = not a || b


constTest :: Bool -> IO ()
constTest True = return ()
constTest False = error "Failed on constTest"



data QFilePath = QFilePath FilePath
                 deriving Show

instance Arbitrary QFilePath where
    arbitrary = liftM QFilePath arbitrary
    coarbitrary = undefined


instance Arbitrary Char where
    arbitrary = elements "?|./:\\abcd 123;_"
    coarbitrary = undefined



-- below is mainly stolen from Test.QuickCheck, modified to crash out on failure

quickSafe :: Testable a => a -> IO ()
quickSafe prop = check quick prop

quick :: Config
quick = Config
  { configMaxTest = 500
  , configMaxFail = 1000
  , configSize    = (+ 3) . (`div` 2)
  , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
  }
         

check :: Testable a => Config -> a -> IO ()
check config a =
  do rnd <- newStdGen
     tests config (evaluate a) rnd 0 0 []


tests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO () 
tests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do done "OK, passed" ntest stamps
  | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
                                       error "More entropy required!"
  | otherwise               =
      do putStr (configEvery config ntest (arguments result))
         case ok result of
           Nothing    ->
             tests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             error ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps =
  do putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ".\n"
  display [x] = " (" ++ x ++ ").\n"
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"
