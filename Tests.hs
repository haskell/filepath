
module Main where

-- import the relevant modules under test
import System.FilePath
--import qualified System.FilePath.Windows as W
--import qualified System.FilePath.Posix as P


-- framework support for the various tests
data Test a = Test Int [Int]
              deriving Show

instance Monad Test where
    return _ = error "Not implemented"
    _ >>= _  = error "Not implemented"
    (Test c1 x1) >> (Test c2 x2) = Test (c1+c2) (x1 ++ map (+c1) x2)
    

(===) :: Eq a => a -> a -> Test ()
a === b | a == b = Test 1 []
        | a /= b = Test 1 [1]


tests = do
    getExtension "file.txt" === ".txt"
    getExtension "file" === ""
    getExtension "file/file.txt" === ".txt"
    getExtension "file.txt/boris" === ""
    getExtension "file.txt/boris.ext" === ".ext"
    getExtension "file/path.txt.bob.fred" === ".fred"
    
    setExtension "file.txt" ".bob" === "file.bob"
    setExtension "file.txt" "bob" === "file.bob"
    setExtension "file" ".bob" === "file.bob"
    setExtension "file.txt" "" === "file"
    setExtension "file.fred.bob" "txt" === "file.fred.txt"
    
    dropExtension "file.txt" === "file"
    dropExtension "file.txt/file.bob" === "file.txt/file"
    dropExtension "file/file" === "file/file"


main = if null failed then
            putStrLn $ "All tests passed (" ++ show count ++ ")"
       else
            putStrLn $ "FAILURES: " ++ show failed
    where (Test count failed) = tests
