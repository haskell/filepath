
module Main where

import Data.Char
import Data.List
import System.IO


data Test = Expr String
          | Test [String] String
          deriving Show

isExpr (Expr{}) = True
isExpr _ = False


main = do src <- readFile "../System/FilePath/Internal.hs"
          let tests = concatMap getTest $ zip [1..] (lines src)
          writeFile "FilePath_Test.hs" (prefix ++ genTests tests)

prefix = unlines
    ["import AutoTest"
    ,"import qualified System.FilePath.Windows as W"
    ,"import qualified System.FilePath.Posix as P"
    ,"main = do"
    ]


getTest :: (Int,String) -> [(Int,Test)]
getTest (line,xs) | "-- > " `isPrefixOf` xs = f $ drop 5 xs
    where
        f x | "Windows:" `isPrefixOf` x = let res = grabTest (drop 8 x) in [g "W" res]
            | "Posix:"   `isPrefixOf` x = let res = grabTest (drop 6 x) in [g "P" res]
            | otherwise = let res = grabTest x in [g "W" res, g "P" res]

        g p (Expr x) = (line,Expr (h p x))
        g p (Test a x) = (line,Test a (h p x))
        
        h p x = joinLex $ map (addPrefix p) $ makeValid $ splitLex x

getTest _ = []


addPrefix :: String -> String -> String
addPrefix pre str | str `elem` fpops || (all isAlpha str && length str > 1 && not (str `elem` prelude))
                      = pre ++ "." ++ str
                  | otherwise = str


prelude = ["elem","uncurry","snd","fst","not","null","if","then","else"
          ,"True","False","concat","isPrefixOf","isSuffixOf"]
fpops = ["</>","<.>"]


grabTest :: String -> Test
grabTest x = if null free then Expr x else Test free x
    where
        free = sort $ nub [x | x <- lexs, length x == 1, all isAlpha x]
        lexs = splitLex x



splitLex :: String -> [String]
splitLex x = case lex x of
                [("","")] -> []
                [(x,y)] -> x : splitLex y
                y -> error $ "GenTests.splitLex, " ++ show x ++ " -> " ++ show y

-- Valid a => z   ===>  (\a -> z) (makeValid a)
makeValid :: [String] -> [String]
makeValid ("Valid":a:"=>":z) = "(\\":a:"->":z ++ ")":"(":"makeValid":a:")":[]
makeValid x = x


joinLex :: [String] -> String
joinLex = unwords


-- would be concat, but GHC has 'issues'
rejoinTests :: [String] -> String
rejoinTests xs = unlines $
                     [" block" ++ show i | i <- [1..length res]] ++
                     concat (zipWith rejoin [1..] res)
    where
        res = divide xs
    
        divide [] = []
        divide x = a : divide b
            where (a,b) = splitAt 50 x
        
        rejoin n xs = ("block" ++ show n ++ " = do") : xs


genTests :: [(Int, Test)] -> String
genTests xs = rejoinTests $ concatMap f $ zip [1..] (one++many)
    where
        (one,many) = partition (isExpr . snd) xs

        f (tno,(lno,test)) =
            [" putStrLn \"Test " ++ show tno ++ ", from line " ++ show lno ++ "\""
            ," " ++ genTest test]

-- the result must be a line of the type "IO ()"
genTest :: Test -> String
genTest (Expr x) = "constTest (" ++ x ++ ")"
genTest (Test free x) = "quickSafe (\\" ++ concatMap ((' ':) . f) free ++ " -> (" ++ x ++ "))"
    where
        f [a] | a >= 'x' = "(QFilePath " ++ [a] ++ ")"
        f x = x
