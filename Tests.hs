
module Main where

-- import the relevant modules under test
import System.FilePath
import qualified System.FilePath.Windows as W
import qualified System.FilePath.Posix as P

import Test.QuickCheck

-- framework support for the various tests
data Test a = Test Int [Int]
              deriving Show

instance Monad Test where
    return _ = error "Not implemented"
    _ >>= _  = error "Not implemented"
    (Test c1 x1) >> (Test c2 x2) = Test (c1+c2) (x1 ++ map (+c1) x2)
    

(===) :: Eq a => a -> a -> Test ()
a === b = bool $ a == b

bool :: Bool -> Test ()
bool True  = Test 1 []
bool False = Test 1 [1]

data QFilePath = QFilePath FilePath
                 deriving Show

instance Arbitrary QFilePath where
    arbitrary = vector 25 >>= return . QFilePath

instance Arbitrary Char where
    arbitrary = oneof $ map return "./:\\abcd"


tests = do
    W.pathSeparator === '\\'
    P.pathSeparator === '/'
    bool $ all W.isPathSeparator "\\/"
    W.fileSeparator === ';'
    P.fileSeparator === ':'
    extSeparator === '.'

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
    dropExtension "file.txt.bob" === "file.txt"
    dropExtension "file.txt/file.bob" === "file.txt/file"
    dropExtension "file/file" === "file/file"
    dropExtension "file.." === "file."
    
    hasExtension "file.txt" === True
    hasExtension "file/file.txt" === True
    hasExtension "file.txt/file" === False


main = if null failed then
            putStrLn $ "All tests passed (" ++ show count ++ ")"
       else
            putStrLn $ "FAILURES: " ++ show failed
    where (Test count failed) = tests


extTest (QFilePath x) = null (getExtension x) /= hasExtension x
